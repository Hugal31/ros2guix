#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(use-modules (debugging assert)
             (ice-9 pretty-print)
             (ice-9 string-fun)
             (guix build download)
             (guix build utils)
             (guix scripts)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-37)
             (srfi srfi-43)
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
  (make-ros-package name url tag version xml)
  ros-package?
  (name ros-package-name)
  (url ros-package-url)
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
         (packages (reverse (assq-ref opts 'packages))))

    (when (not (pair? packages))
      (error "You must specify at least one ROS package to convert."))
    (when (not ros-distro)
      (error "You must specify a ROS distro."))

    (format #t "Using ROS distro ~a\n" ros-distro)
    (format #t "ROS packages: ~a\n" packages)

    (let* ((distribution-cache (fetch-distribution-cache ros-distro))
           (ros-package-list (distribution-cache->ros-packages distribution-cache)))
      (pretty-print ros-package-list))))

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

    (append (map
             (lambda (repository) (repository->ros-packages repository package-xmls))
             repositories))))

(define (repository->ros-packages repository packages-xmls)
  (let* ((repository-name (car repository))
         (repository-content (cdr repository))
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
              tag
              version
              xml)))

         packages)

        '())))
