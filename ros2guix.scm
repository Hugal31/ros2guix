#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

(use-modules (debugging assert)
             (ice-9 pretty-print)
             (guix build download)
             (guix build utils)
             (guix scripts)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-37)
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

    (let* ((distribution-cache (fetch-distribution-cache ros-distro)))
      (pretty-print distribution-cache))))

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
      ;(delete-file distribution-cache-file)
      (assert (equal? "cache" cache-type))
      (assert (eq? 2 cache-version))
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

;(fetch-distribution-cache "galactic")
;(main '("ros2guix" "-r" "noetic" "a" "b"))
