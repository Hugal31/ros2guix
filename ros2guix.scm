#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(add-to-load-path (getcwd))

(use-modules (debugging assert)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 pretty-print)
             (ice-9 regex)
             (ice-9 rdelim)
             (ice-9 string-fun)
             (ice-9 vlist)
             (gnu packages)
             (guix build download)
             (guix build utils)
             (guix describe)
             (guix diagnostics)
             (guix memoization)
             (guix scripts)
             (ros2guix fixes)
             (ros2guix utils)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-37)
             (srfi srfi-43)
             (sxml simple)
             ((sxml xpath)
              #:select (select-kids sxpath node-join node-or node-self node-reduce node-trace node-typeof?))
             (yaml)
             (web uri))

(define base-rosdistro-url
  "https://raw.githubusercontent.com/ros/rosdistro/master/")
(define ros-distro-index
  (string-append base-rosdistro-url "index-v4.yaml"))
(define %github "github.com")

(define (show-help)
  "Show help"
  (display "Usage: ros2guix [OPTION] PACKAGES...
Convert the given PACKAGES.\n")
  (display "
  -h, --help             display this help and exit")
  (display "
  -a, --all              convert all the packages in the distribution")
  (display "
  -e, --regex            evaluate [PACKAGES] as regular expressions")
  (display "
  -o, --output FILE      output the result into FILE")
  (display "
  -r, --ros-distro DIST  specify ros distro")
  (display "
  -R, --recursive        also get the package dependencies, recursively")
  (display "
  -V, --version          display version information and exit"))

;; Args
(define %options
  (list (option '(#\a "all") #f #f
                (lambda (_1 _2 _3 result)
                  (alist-cons 'all-packages? #t result)))
        (option '(#\e "regex") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'regex #t result)))
        (option '(#\o "output") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'output (open-output-file arg) result)))
        (option '(#\r "ros-distro") #t #f
                   (lambda (opt name arg result)
                     (alist-cons 'ros-distro arg result)))
        (option '(#\R "recursive") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'recursive #t result)))
        (option '(#\h "help") #f #f
	        (lambda _
	          (show-help)))))

(define %default-options
  ;; Alist of default option values.
  `((all-packages? . #f)
    (ros-distro . ,(getenv "ROS_DISTRO"))
    (packages . ())
    (output . ,(current-output-port))))

(define-record-type <ros-package>
  (make-ros-package name url source-url tag version xml sxml)
  ros-package?
  (name ros-package-name)
  (url ros-package-url)
  (source-url ros-package-source-url)
  (tag ros-package-tag)
  (version ros-package-version)
  (xml ros-package-xml)
  (sxml ros-package-sxml/))

(define (ros-package-sxml p)
  (force (ros-package-sxml/ p)))

(define* (main #:optional (args (command-line)))
  (define (parse-options)
    (parse-command-line (cdr args) %options (list %default-options)
                        #:build-options? #f
                        #:argument-handler
                        (lambda (arg result)
                          (let ((previous-packages (assq-ref result 'packages)))
                            (alist-cons 'packages (cons arg previous-packages) result)))))

  (let* ((opts (parse-options))
         (output (assq-ref opts 'output))
         (recursive (assq-ref opts 'recursive))
         (ros-distro (assq-ref opts 'ros-distro))
         (packages-to-process (assq-ref opts 'packages))
         (packages-pred
          (cond
           ((assq-ref opts 'all-packages?) (const #t))
           ((null? packages-to-process) (error "You must specify at least one ROS package to convert."))
           ((assq-ref opts 'regex)
            (let ((res (map make-regexp packages-to-process)))
              (lambda (package)
                (any (lambda (re) (regexp-exec re (ros-package-name package))) res))))
           (else
            (lambda (package)
              (member (ros-package-name package) packages-to-process))))))

    (when (not ros-distro)
      (error "You must specify a ROS distro."))

    (let* ((distribution-cache (fetch-distribution-cache ros-distro))
           (ros-packages (distribution-cache->ros-packages distribution-cache))
           (matching-ros-packages (filter packages-pred ros-packages))
           (matching-ros-packages (if recursive (add-deps matching-ros-packages ros-packages) matching-ros-packages))
           (try-guix-packages
            (map
             (lambda (package)
               (with-exception-handler
                   (lambda (exn)
                     (format (current-error-port)
                             "Could not process package ~a: ~a\n" (ros-package-name package) exn)
                     #f)
                 (lambda () (create-guix-package package))
                 #:unwind? #t
                 #:unwind-for-type 'misc-error))
             matching-ros-packages))
           (guix-packages (filter identity try-guix-packages))
           (imported-modules (delete-duplicates!
                              (fold (lambda (package prev)
                                      (append (guess-package-imports package) prev))
                                    %default-modules
                                    guix-packages))))

      (with-output-to-port output
        (lambda ()
          (pretty-print
           `(define-module (ros ,(string->symbol ros-distro) generated)
              ,@(apply append (map (lambda (m) `(#:use-module ,m)) imported-modules))))
          (for-each (lambda (package)
                      (newline)
                      (pretty-print package))
                    guix-packages))))))

(define %default-modules
  '(((guix licenses) #:prefix license:)
    (guix git-download)
    (guix packages)))

(define (fetch-distribution-cache ros-distro)
  (let* ((index (fetch-ros-index))
         (distribution-cache-file (string-append "/tmp/ros-distro-" ros-distro "-cache.yaml"))
         (distribution (assoc-ref (assoc-ref index "distributions") ros-distro))
         (distribution-cache-url (assoc-ref distribution "distribution_cache"))
         (distribution-cache-zipped-file (url-fetch distribution-cache-url (string-append distribution-cache-file ".gz"))))

    (invoke "gzip" "-fd" distribution-cache-zipped-file)

    (let* ((cache (read-yaml-file distribution-cache-file))
           (cache-type (assoc-ref cache "type"))
           (cache-version (string->number (assoc-ref cache "version"))))
      (delete-file distribution-cache-file)
      (assert (equal? "cache" cache-type))
      cache)))

(define (fetch-ros-index)
  (let* ((index-file (url-fetch ros-distro-index "/tmp/ros-index.yaml"))
         (index (read-yaml-file index-file))
         (index-type (assoc-ref index "type"))
         (index-version (string->number (assoc-ref index "version"))))

    (delete-file index-file)
    (assert (equal? "index" index-type))
    (assert (eq? 4 index-version))

    index))

(define (distribution-cache->ros-packages cache)
  (let ((cache-version (string->number (assoc-ref cache "version"))))
    (assert (eq? 2 cache-version)))

  ;(format #t "cache is ~a\n" (substring (format #f "~a" cache) 0 30))
  ;(format #t "distribution_file is ~a\n" (substring (format #f "~a" (assoc-ref cache "distribution_file")) 0 30))

  (let* ((distribution-file (assoc-ref cache "distribution_file"))
                                        ; For now, take the first release platform
         (release (vector-ref distribution-file 0))
         (repositories (assoc-ref release "repositories"))
         (package-xmls (assoc-ref cache "release_package_xmls")))

    (apply append (map
                   (lambda (repository)
                     (repository->ros-packages repository package-xmls))

                   repositories))))

(define (add-deps selected-packages all-packages)
  "Given a list of ros-packages and the list of all ros-packages,
return a list of ros-pacakges with all the found dependencies"

  (let* ((dependency-names
          (flatten (map
                    (lambda (package)
                      (map cadr
                           (ros-package-xml-all-dependencies (ros-package-sxml package))))
                    selected-packages)))
         (dependencies
          (filter identity
                  (map
                   (lambda (dep)
                     ; TODO Log the package we cannot find
                     (find
                      (lambda (package)
                        (equal? dep (ros-package-name package)))
                      all-packages))
                   dependency-names)))

         (dep-deps (if (null? dependencies)
                       '()
                       (add-deps dependencies all-packages))))


    (delete-duplicates! (append selected-packages dep-deps) eq?)))

(define (repository->ros-packages repository packages-xmls)
  (let* ((repository-name (car repository))
         (repository-content (cdr repository))
         (source (assoc-ref repository-content "source"))
         (source-url (assoc-ref source "url"))
         (release (assoc-ref repository-content "release"))
         (packages (vector->list (or (assoc-ref release "packages") (vector repository-name))))
         (url (assoc-ref release "url"))
         (release-tag (assoc-ref (assoc-ref release "tags") "release"))
         (version (assoc-ref release "version")))

    (if release-tag
        (map
         (lambda (package-name)
           (let* ((tag (string-replace-substring
                        (string-replace-substring release-tag "{package}" package-name)
                        "{version}"
                        version))
                  (xml (assoc-ref packages-xmls package-name))
                  (sxml (delay (xml->sxml xml))))
             (make-ros-package
              package-name
              url
              source-url
              tag
              version
              xml
              sxml)))

         packages)

        '())))

(define (create-guix-package ros-package)
  (define (format-package-def package-name)
    (string->symbol package-name))

  (let*-values (((sxml) (ros-package-sxml ros-package))
                ((package-name) (ros-package-name ros-package))
                ((package-fix) (get-package-fix package-name))
                ((guix-package-name) (ros-package-to-guix package-name))
                ((package-description) (ros-package-xml-desc sxml))
                ((package-home-page) (ros-package-xml-home-page ros-package sxml))
                ((package-license) (ros-package-xml-license sxml))
                ((build-system) (or (package-fix-build-system package-fix) (guess-build-system sxml)))
                ((guix-license) (ros-license->guix-license package-license))
                ((native-inputs inputs propagated-inputs) (guess-package-dependencies ros-package))
                ((native-inputs) `(list ,@(map format-package-def native-inputs)))
                ((inputs) `(list ,@(map format-package-def inputs)))
                ((propagated-inputs) `(list ,@(map format-package-def propagated-inputs)))
                ((package-hash) (get-package-hash ros-package)))

    ;(pretty-print sxml)

    `(define-public ,(string->symbol guix-package-name)
       (package
        (name ,guix-package-name)
        (version ,(ros-package-version ros-package))
        (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url ,(ros-package-url ros-package))
                       (commit ,(ros-package-tag ros-package))))
                 (file-name (git-file-name name version))
                 (sha256
                  (base32 ,package-hash))))
        (build-system ,build-system)
        (arguments '(#:tests? #f))
        (native-inputs ,native-inputs)
        (inputs ,inputs)
        (propagated-inputs ,propagated-inputs)
        (home-page ,package-home-page)
        (synopsis ,(format #f "ROS package ~a" package-name))
        (description ,package-description)
        (license ,guix-license)))))

(define (get-package-hash package)
  "Try to use ZIP download if possible, otherwise use VCS"
  (if (string-contains (ros-package-url package) %github)
      (get-package-hash/github-archive package)
      (get-package-hash/git package)))

(define (get-package-hash/github-archive package)
  (let* ((url (ros-package-url package))
         (tag (ros-package-tag package))
         (package-name (ros-package-name package))
         (directory (mkdtemp (string-append "/tmp/" package-name "_XXXXXX")))
         (repo-directory (string-append directory "/sources"))
         (archive-ext ".tar.gz")
         (archive-name (string-append directory "/" package-name archive-ext))
         (archive-url (string-append (remove-suffix/read-only url ".git") "/archive/refs/heads/" tag archive-ext))
         (archive-file (url-fetch archive-url archive-name))
         (_ (mkdir repo-directory))
         (_ (invoke "tar" "-xzf" archive-file "-C" repo-directory "--strip-components" "1"))
         (hash (get-output (string-append "guix hash -r " repo-directory))))

    (delete-file-recursively directory)

    hash))

(define (get-package-hash/git package)
  (let* ((url (ros-package-url package))
         (tag (ros-package-tag package))
         (directory (mkdtemp (string-append "/tmp/" (ros-package-name package) "_XXXXXX")))
         (_ (invoke "git" "clone" url "--branch" tag  "--depth" "1" directory))
         (hash (get-output (string-append "guix hash -rx " directory))))

    (delete-file-recursively directory)

    hash))

(define (get-output command)
  (let* ((port (open-input-pipe command))
         (str (read-line port)))
    (close-pipe port)
    str))

(define (guess-package-dependencies ros-package)
  "Given a <ros-package>, return a three-sized list of native-inputs,
inputs and propagated inputs guix-like names"

  (define sxml (ros-package-sxml ros-package))

  (define (is-propagated-dependency dep)
    "Given a dep as a guix package name, return #t if it is a propagated dependency
     (e.g. a scripting dependnecy)."

    #f
    ;;(string-prefix? "python" dep)
    )

  (define (remove-version dep) (car (last-pair dep)))

  (define (get-cleaned-guix-dep proc)
    (map ros-package-to-guix
         (map remove-version (proc sxml))))

  ;; We do not handle versions for now.
  (let*-values (((propagated-build-dependencies build-dependencies)
                 (partition! is-propagated-dependency (get-cleaned-guix-dep ros-package-xml-build-dependencies)))
                ((propagated-dependencies dependencies)
                 (partition! is-propagated-dependency (get-cleaned-guix-dep ros-package-xml-dependencies)))

                ((run-dependencies)
                 (append propagated-build-dependencies propagated-dependencies (get-cleaned-guix-dep ros-package-xml-run-dependencies))))

    (let* ((package-fix (get-package-fix (ros-package-name ros-package)))
           (run-dependencies (delete-duplicates-sorted
                              (merge (sort! run-dependencies string<?)
                                     (package-fix-propagated-inputs package-fix)
                                     string<?)
                              string=?))
           (dependencies (lset-difference equal?
                                          (delete-duplicates-sorted
                                           (merge (sort! dependencies string<?) (package-fix-inputs package-fix) string<?)
                                                 string=?)
                                          run-dependencies))
           (build-dependencies (lset-difference equal?
                                                (delete-duplicates-sorted
                                                 (merge
                                                  (sort! build-dependencies string<?)
                                                  (package-fix-native-inputs package-fix)
                                                  string<?)
                                                 string=?)
                                                dependencies
                                                run-dependencies)))
      (values
       (sort! build-dependencies string<?)
       (sort! dependencies string<?)
       (sort! run-dependencies string<?)))))

(define (ros-package-to-guix dep)
  "Given dep as a string, will return a guix-style dependency name"

  (define hard-coded-mapping
    '(("pydocstyle" . "python-pydocstyle")))

  (define (conversion-heuristic dep)
    (set! dep (string-replace-substring dep "_" "-"))
    (set! dep (string-replace-substring dep "python3" "python"))

    dep)

  (or (assoc-ref hard-coded-mapping dep)
      (conversion-heuristic dep)))

(define (guess-package-imports package-definition)
  (let* ((package (third package-definition))
         (build-system (car (assq-ref package 'build-system)))
         (native-inputs (car (assq-ref package 'native-inputs)))
         (inputs (car (assq-ref package 'inputs)))
         (propagated-inputs (car (assq-ref package 'propagated-inputs)))
         (total-inputs (append (cdr native-inputs) (cdr inputs) (cdr propagated-inputs)))
         (total-inputs-symbols (delete-duplicates! total-inputs eq?)))

    (delete-duplicates! (filter-map m-symbol->module total-inputs-symbols) eq?)))

(define (symbol->module symbol)
  "Resolve symbol to a module path"

  (match (assq-ref hard-coded-symbol->module-map symbol)
    (#f
     (match (find-package-locations (symbol->string symbol))
       (() #f
        ;;(error "Could not find package location for " symbol)
        )
       (locations
        (map string->symbol
             (string-split
              (first (string-split
                      ;; Pick the first for now
                      (location-file (cdar locations))
                      #\.))
              #\/)))))

    (s s)))

(define m-symbol->module (memoize symbol->module))

(define hard-coded-symbol->module-map
  '((git-fetch . (guix git-download))
    (git-reference . (guix git-download))
    (git-file-name . (guix git-download))))

;; TODO: Some packages (async_web_server_cpp) depends on ament_cmake_ros, but
;; do not declare their build type in the export section. Try to guess those as well.
(define (guess-build-system sxml)
  ;; There is probably something more idiomatic
  (define (first-or-false l) (if (null? l) #f (car l)))

  (let ((exported-build-type (first-or-false (ros-package-xml-exported-build-type sxml)))
        (buildtools (ros-package-xml-buildtool-dependencies sxml)))

    (cond
     (exported-build-type

      ;; Guess on the exported build_type
      (cond
       ((equal? exported-build-type "ament_cmake") 'ament-cmake-build-system)
       ((equal? exported-build-type "ament_python") 'ament-python-build-system)
       ((equal? exported-build-type "cmake") 'cmake-build-system)
       (else (error "Could not guess build type for package.xml:" sxml exported-build-type))))

     (buildtools
      ;; Try to guess from the buildtool_depends
      (match (cadar buildtools)
        ("ament_cmake_ros" "ament_cmake")
        (a  (error "Could not guess build type from build tools" a))))

     (else (error "Could not guess the build type:" sxml)))))

(define (nodeset-match-ros-condition? nodeset)
  (define get-condition (sxpath '(@ (condition))))
  (match (get-condition nodeset)
    (() #t)
    ((('condition "$ROS_VERSION == 2")) #t)
    (_ #f)))

(define ros-package-xml-exported-build-type
  (sxpath
   `(package export build_type
             ,(node-self nodeset-match-ros-condition?)
             *text*)))

(define (ros-package-xml-home-page ros-package sxml)
  (let ((home-pages ((sxpath '(package url *text*)) sxml)))
    (if (null? home-pages)
        (ros-package-source-url ros-package)
        (first home-pages))))

(define (ros-package-xml-desc sxml)
  (first ((sxpath '(package description *text*)) sxml)))

(define (ros-package-xml-license sxml)
  (first ((sxpath '(package license *text*)) sxml)))

(define ros-package-xml-buildtool-dependencies
  (sxpath '(package buildtool_depend)))

(define ros-package-xml-build-dependencies
  (sxpath `(package
            ,(node-or
              (sxpath '(buildtool_depend))
              (sxpath '(build_depend))
              (sxpath '(test_depend)))
            ,(node-self nodeset-match-ros-condition?))))

(define ros-package-xml-dependencies
  (sxpath '(package depend ,(node-self nodeset-match-ros-condition?))))

(define ros-package-xml-run-dependencies
  (sxpath `(package
            ,(node-or
              (sxpath '(run_depend))
              (sxpath '(exec_depend))
              (sxpath '(buildtool_export_depend)))
            ,(node-self nodeset-match-ros-condition?))))

(define ros-package-xml-all-dependencies
  (node-or
   ros-package-xml-build-dependencies
   ros-package-xml-dependencies
   ros-package-xml-run-dependencies))

(define (ros-license->guix-license ros-license)
  (let ((guix-license (assoc-ref ros-licenses-to-guix-assoc ros-license)))
    (when (not guix-license)
      (error "Unknown Guix equivalent for license" ros-license))

    guix-license))

(define ros-licenses-to-guix-assoc
  '(("Apache-2.0" . license:asl2.0)
    ("Apache 2.0" . license:asl2.0)
    ("Apache License 2.0" . license:asl2.0)
    ("Apache License, Version 2.0" . license:asl2.0)
    ("BSD" . license:bsd-3)
    ("LGPLv3" . license:lgpl3)
    ("MIT" . license:expat)))
