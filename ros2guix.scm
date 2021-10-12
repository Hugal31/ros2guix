#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(use-modules (debugging assert)
             (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 string-fun)
             (ice-9 vlist)
             (gnu packages)
             (guix build download)
             (guix build utils)
             (guix describe)
             (guix diagnostics)
             (guix memoization)
             (guix scripts)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-37)
             (srfi srfi-43)
             (sxml simple)
             ((sxml xpath)
              #:select (sxpath))
             (yaml)
             (web uri))

(define base-rosdistro-url
  "https://raw.githubusercontent.com/ros/rosdistro/master/")
(define ros-distro-index
  (string-append base-rosdistro-url "index-v4.yaml"))

(define (show-help)
  "Show help"
  (display "Usage: ros2guix [OPTION] PACKAGES...
Convert the given PACKAGES.\n")
  (display "
  -h, --help             display this help and exit")
  (display "
  -r, --ros-distro       specify ros distro")
  (display "
  -V, --version          display version information and exit"))

;; Args
(define %options
  (list (option '(#\r "ros-distro") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'ros-distro arg result)))
        (option '(#\h "help") #f #f
	        (lambda _
	          (show-help)))))

(define %default-options
  ;; Alist of default option values.
  `((ros-distro . ,(getenv "ROS_DISTRO"))
    (packages . ())))

(define-record-type <ros-package>
  (make-ros-package name url source-url tag version xml)
  ros-package?
  (name ros-package-name)
  (url ros-package-url)
  (source-url ros-package-source-url)
  (tag ros-package-tag)
  (version ros-package-version)
  (xml ros-package-xml))

(define* (main #:optional (args (command-line)))
  (define (parse-options)
    (parse-command-line (cdr args) %options (list %default-options)
                        #:build-options? #f
                        #:argument-handler
                        (lambda (arg result)
                          (let ((previous-packages (assq-ref result 'packages)))
                            (alist-cons 'packages (cons arg previous-packages) result)))))

  (let* ((opts (parse-options))
         (ros-distro (assq-ref opts 'ros-distro))
         (packages-to-convert (assq-ref opts 'packages)))

    (when (not (pair? packages-to-convert))
      (error "You must specify at least one ROS package to convert."))
    (when (not ros-distro)
      (error "You must specify a ROS distro."))

    (format #t "Using ROS distro ~a\n" ros-distro)
    (format #t "ROS packages: ~a\n" packages-to-convert)

    (let* ((distribution-cache (fetch-distribution-cache ros-distro))
           (ros-packages (distribution-cache->ros-packages distribution-cache))
           (matching-ros-packages (filter
                                   (lambda (package)
                                     (find (lambda (name)
                                             (equal? name (ros-package-name package)))
                                           packages-to-convert))
                                   ros-packages))
           (guix-packages (map create-guix-package matching-ros-packages)))

      (pretty-print guix-packages))))

(define (fetch-distribution-cache ros-distro)
  (let* ((index (fetch-ros-index))
         (distribution-cache-file (string-append "/tmp/ros-distro-" ros-distro "-cache.yaml"))
         (distribution (assoc-ref (assoc-ref index "distributions") ros-distro))
         (distribution-cache-url (assoc-ref distribution "distribution_cache"))
         (distribution-cache-zipped-file (url-fetch distribution-cache-url (string-append distribution-cache-file ".gz"))))

    (pretty-print distribution-cache-file)
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
                  (xml (assoc-ref packages-xmls package-name)))
             (make-ros-package
              package-name
              url
              source-url
              tag
              version
              xml)))

         packages)

        '())))

(define (create-guix-package ros-package)
  (let* ((sxml (xml->sxml (ros-package-xml ros-package)))
         ;(_ (pretty-print sxml))
         (package-name (ros-package-name ros-package))
         (package-description (ros-package-xml-desc sxml))
         (package-home-page (ros-package-xml-home-page ros-package sxml))
         (package-license (ros-package-xml-license sxml))
         (build-system (guess-build-system sxml))
         (guix-license (ros-license->guix-license package-license)))

    ;(pretty-print sxml)

    `(define-public ,(string->symbol package-name)
       (package
        (name ,package-name)
        (version ,(ros-package-version ros-package))
        (source (origin
                 (method git-fetch)
                 (uri (git-reference
                       (url ,(ros-package-url ros-package))
                       (commit ,(ros-package-tag ros-package))
                       (file-name (git-file-name name version))))))
        (build-system ,build-system)
        (native-inputs '())
        (inputs '())
        (propataged-inputs '())
        (home-page ,package-home-page)
        (synopsis ,(format #f "ROS package ~a" package-name))
        (description ,package-description)
        (license ,guix-license)))))

(define (guess-package-imports package-definition)
  (let* ((package (third package-definition))
         (build-system (car (assq-ref package 'build-system)))
         (native-inputs (car (assq-ref package 'native-inputs)))
         (inputs (car (assq-ref package 'inputs)))
         (propagated-inputs (car (assq-ref package 'propagated-inputs)))
         (total-inputs (append (cadr native-inputs) (cadr inputs) (cadr propagated-inputs)))
         (total-inputs-symbols (delete-duplicates! (map cadadr total-inputs) eq?)))

    (delete-duplicates! (map m-symbol->module total-inputs-symbols) eq?)))

(define (symbol->module symbol)
  "Resolve symbol to a module path"

  (match (assq-ref hard-coded-symbol->module-map symbol)
    (#f
     (match (find-package-locations (symbol->string symbol))
       (() (error "Could not find package location for " symbol))
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

(define (guess-build-system sxml)
  (let* ((build-type (first (ros-package-xml-exported-build-type sxml))))

    (cond
     ((equal? build-type "ament_cmake") 'ament-build-system)
     (else (error "Could not guess build type for package.xml:" sxml)))))

(define ros-package-xml-exported-build-type
  (sxpath '(package export build_type *text*)))

(define (ros-package-xml-home-page ros-package sxml)
  (let ((home-pages ((sxpath '(package url *text*)) sxml)))
    (if (null? home-pages)
        (ros-package-source-url ros-package)
        (first home-pages))))

(define (ros-package-xml-desc sxml)
  (first ((sxpath '(package description *text*)) sxml)))

(define (ros-package-xml-license sxml)
  (first ((sxpath '(package license *text*)) sxml)))

(define (ros-license->guix-license ros-license)
  (let ((guix-license (assoc-ref ros-licenses-to-guix-assoc ros-license)))
    (when (not guix-license)
      (error "Unknown Guix equivalent for license" ros-license))

    guix-license))

(define ros-licenses-to-guix-assoc
  '(("Apache License 2.0" . license:asl2.0)
    ("BSD" . license:bsd-3)))
