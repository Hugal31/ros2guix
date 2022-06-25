(define-module (ros galactic build ament-build-system)
  #:use-module ((guix build cmake-build-system) #:prefix cmake:)
  #:use-module ((guix build utils))
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases))

(define (is-dsv? file)
  (string-suffix? ".dsv" file))

(define (is-dot-or-dot-dot? file)
  (or (equal? "." file)
      (equal? ".." file)))

(define is-not-dot-dot? (negate is-dot-or-dot-dot?))

(define shebang (string->utf8 "#!"))

(define (starts-with-shebang file)
  "Looks if the file file starts with shebang"
  (bytevector=?
   shebang
   (call-with-input-file file
     (cut get-bytevector-n <> (bytevector-length shebang))
     #:binary #t)))

(define (is-script? file)
  "Return true if file is a regular executable file starting with a schebang"
  (and
   (access? file X_OK)
   (eq? 'regular (stat:type (stat file)))
   (starts-with-shebang file)))

(define (is-script-python? file)
  "Return true if the first line of the file contains python. Do not re-check if is-script?"
  (call-with-input-file file
    (lambda (port)
      (string-contains
       (read-line port)
       "python"
       2))))

(define* (source-ament-env
          #:key outputs #:allow-other-keys #:rest args)
                                        ; Try to populate the environment with the DSV in out/share

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
             (envvar (if (equal? "PYTHONPATH" envvar) "GUIX_PYTHONPATH" envvar))
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
                                       packages-to-load)))
         (current-cmake-prefix-path (or (getenv "CMAKE_PREFIX_PATH") "")))

    (format #t "Will load ~a\n" packages-dsv-to-load)

    (for-each
     (lambda (dsv)
       (load-dsv dsv out))
     packages-dsv-to-load)

    ;; For some reason, the CMAKE_PREFIX_PATH is not natively included in the dsv.
    ;; It is added later by colcon. Here we add it in an hardcoded way, but we could also:
    ;; - Use colcon
    ;; - Add a .dsv with CMAKE_PREFIX_PATH
    ;; Moreover, this CMAKE_PREFIX_PATH doesn't contains dependencies!

    (setenv "CMAKE_PREFIX_PATH" (string-append current-cmake-prefix-path ":" out))

    #t))

; Python wrap, for reference
(define* (python-wrap #:key inputs outputs #:allow-other-keys)
  (define (list-of-files dir)
    (find-files dir (lambda (file stat)
                      (and (eq? 'regular (stat:type stat))
                           (not (wrapped-program? file))))))

  (define bindirs
    (append-map (match-lambda
                  ((_ . dir)
                   (list (string-append dir "/bin")
                         (string-append dir "/sbin"))))
                outputs))

  ;; Do not require "bash" to be present in the package inputs
  ;; even when there is nothing to wrap.
  ;; Also, calculate (sh) only once to prevent some I/O.
  (define %sh (delay (search-input-file inputs "bin/bash")))
  (define (sh) (force %sh))

  (let* ((var `("GUIX_PYTHONPATH" prefix
                ,(search-path-as-string->list
                  (or (getenv "GUIX_PYTHONPATH") "")))))
    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each (cut wrap-program <> #:sh (sh) var)
                            files)))
              bindirs)))

; Heavily inspired by guix/build/python-build-system.
(define* (wrap
          #:key inputs outputs
          #:allow-other-keys #:rest args)

  (define (list-of-files dir)
    (find-files dir (lambda (file stat)
                      (and (is-script? file)
                           (not (wrapped-program? file))))))

  (define bindirs
    (append-map (match-lambda
                  ((_ . dir)
                   (list (string-append dir "/bin")
                         (string-append dir "/sbin"))))
                outputs))

  ;; Do not require "bash" to be present in the package inputs
  ;; even when there is nothing to wrap.
  ;; Also, calculate (sh) only once to prevent some I/O.
  (define %sh (delay (search-input-file inputs "bin/bash")))
  (define (sh) (force %sh))

  (let* ((python-var `("GUIX_PYTHONPATH" prefix
                       ,(search-path-as-string->list
                         (or (getenv "GUIX_PYTHONPATH") "")))))

    (for-each (lambda (dir)
                (let ((files (list-of-files dir)))
                  (for-each
                   (lambda (file)
                     (when (is-script-python? file)
                       (wrap-program file #:sh (sh) python-var)))
                   files)))
              bindirs))
  #t)

(define %cmake-check-phase
  (assq-ref cmake:%standard-phases 'check))

(define %standard-phases
  (modify-phases cmake:%standard-phases
    ;; Replace check after install, because ament only works on install tree.
    ;; Unless we use workspaces.
    (delete 'check)
    (add-after 'install 'source-ament-env source-ament-env)
    (add-after 'source-ament-env 'wrap wrap)
    (add-after 'source-ament-env 'check %cmake-check-phase)))
