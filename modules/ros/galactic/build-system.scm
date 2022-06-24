(define-module (ros galactic build-system)
  #:use-module (guix build-system)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python))

(define cmake-lower (build-system-lower cmake-build-system))

(define* (ament-cmake-lower name
                            #:key (phases '(@ (guix build cmake-build-system) %standard-phases))
                            #:allow-other-keys
                            #:rest arguments)

  (apply
   cmake-lower name
   #:phases `(let* ((phases* ,phases)
                    (old-check (assq-ref phases* 'check))
                    (phases* (alist-delete 'check phases*))
                    (phases* (alist-cons-after 'install 'check old-check phases*))
                    (phases*
                     (alist-cons-after
                      'install
                      'source-ament-env

                      (lambda* (#:key outputs #:allow-other-keys #:rest args)
                                        ; Try to populate the environment with the DSV in out/share
                        (use-modules (srfi srfi-1))
                        (use-modules (ice-9 ftw))
                        (use-modules (ice-9 match))
                        (use-modules (ice-9 rdelim))
                        (use-modules (guix build utils))

                        (define (is-dsv? file)
                                (string-suffix? ".dsv" file))

                        (define (is-not-dot-dot? file)
                          (not (or (equal? "." file)
                                   (equal? ".." file))))

                        (define (load-dsv file install-prefix)
                          (format #t "load environment from ~a/~a\n" install-prefix file)
                          (define (parse-lines-file port)
                            (let* ((result '()))

                              (do ((line (read-line port) (read-line port)))
                                  ((eof-object? line))
                                (set! result (cons line result)))

                              (reverse result)))

                          (define* (prepend-non-duplicate envvar values #:key if-exists)
                            (format #t "Will prepend ~a to ~a\n" values envvar)

                            (let* ((values (map (lambda (value) (string-append install-prefix "/" value)) values))
                                   (original-env (or (getenv envvar) ""))
                                   (original-env-splitted (string-split original-env #\:))
                                   (new-env (fold
                                             (lambda (value env)
                                               (if (and (not (member value env)) (or (not if-exists) (access? value F_OK)))
                                                   (cons value env)
                                                   env))
                                             original-env-splitted
                                             values)))

                              ; TODO: Just change the search-path variable?
                              (setenv envvar (string-join new-env ":"))
                              (format #t "~a is now ~a\n" envvar (getenv envvar))))

                          (define (load-dsv-line line)
                            (format #t "Read line ~a\n" line)
                            (let* ((fields (string-split line #\;))
                                   (command (car fields))
                                   (args (cdr fields)))

                              ;; TODO! In some DSV there is only .sh files. In this case, find the .dsv equivalent
                              ; For some reason I have to use @
                              ((@ (ice-9 match) match) fields
                               (("source" (? is-dsv? file))
                                (load-dsv file install-prefix))
                               (("source" _) #f)
                               (("prepend-non-duplicate" envvar . values) (prepend-non-duplicate envvar values))
                               (("prepend-non-duplicate-if-exists" envvar . values) (prepend-non-duplicate envvar values #:if-exists #t))
                               (line (format #t "Unknown ament command ~a\n" line)))))

                          (call-with-input-file (if (string-prefix? "/" file) file (string-append install-prefix "/" file))
                            (lambda (port)
                              (for-each load-dsv-line (parse-lines-file port)))))

                        (let* ((out (assoc-ref outputs "out"))
                               (ament-index-packages-dir (string-append out "/share/ament_index/resource_index/packages"))
                               (packages-to-load (scandir ament-index-packages-dir is-not-dot-dot?))
                               (packages-dsv-to-load (apply append
                                                            (map
                                                             (lambda (package)
                                                               (map
                                                                (lambda (file)
                                                                  (string-append out "/share/" package "/environment/" file))
                                                                (scandir (string-append out "/share/" package "/environment") is-dsv?)))
                                                             packages-to-load))))

                          (format #t "Will load ~a\n" packages-dsv-to-load)

                          (for-each
                           (lambda (dsv)
                             (load-dsv dsv out))
                           packages-dsv-to-load)

                          #t))

                      phases*))

                    (phases*
                     (alist-cons-after
                      'source-ament-env
                      'add-cmake-prefix-path
                      ;; For some reason, the CMAKE_PREFIX_PATH is not natively included in the dsv.
                      ;; It is added later by colcon. Here we add it in an hardcoded way, but we could also:
                      ;; - Use colcon
                      ;; - Add a .dsv with CMAKE_PREFIX_PATH

                      (lambda* (#:key outputs #:allow-other-keys #:rest args)
                            (let ((out (assoc-ref outputs "out"))
                                  (current-cmake-prefix-path (or (getenv "CMAKE_PREFIX_PATH") "")))
                              (setenv "CMAKE_PREFIX_PATH" (string-append current-cmake-prefix-path ":" out)))

                            #t)

                      phases*)))

               phases*)

   arguments))

(define-public ament-cmake-build-system
  (build-system
    (name 'ament-cmake)
    (description "The Ament build system with CMake")
    (lower ament-cmake-lower)))

(define-public ament-python-build-system python-build-system)
