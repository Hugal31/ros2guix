(define-module (ros galactic build-system ament)
  #:use-module (guix build-system)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix utils))


; Packages to import to the builder process
(define %ament-cmake-build-system-modules
  `((ros galactic build ament-build-system)
    ,@%cmake-build-system-modules))

(define (default-cmake target)
  "Return the default CMake package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages cmake))))
    (module-ref module
                (if target
                    'cmake-minimal-cross
                    'cmake-minimal))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (cmake (default-cmake target))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME"
  ;; Mainly copy-pasted from (guix build-system cmake) lower.
  (define private-keywords
    `(#:cmake #:inputs #:native-inputs
              ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@`(("cmake" ,cmake))
                    ,@native-inputs
                    ,@(if target '() inputs)
                    ,@(if target
                          (standard-cross-packages target 'host)
                          '())
                    ,@(standard-packages)))
    (host-inputs (if target inputs '()))

    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target ament-cmake-cross-build ament-cmake-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (ament-cmake-build name inputs
                            #:key
                            (imported-modules %ament-cmake-build-system-modules)
                            (modules '((ros galactic build ament-build-system)
                                       ; Select everything except %standard-phases
                                       ((guix build cmake-build-system)
                                        #:select (cmake-build))
                                       (guix build utils)))
                            #:allow-other-keys
                            #:rest arguments)
  (apply
   cmake-build
   name
   inputs
   #:imported-modules imported-modules
   #:modules modules
   arguments))

(define* (ament-cmake-cross-build name
                            #:key
                            (imported-modules %ament-cmake-build-system-modules)
                            (modules '((ros galactic build ament-build-system)
                                       (guix build utils)))
                            #:allow-other-keys
                            #:rest arguments)
  (apply
   cmake-cross-build
   name
   #:imported-modules imported-modules
   #:modules modules
   arguments))

(define-public ament-cmake-build-system
  (build-system
    (name 'ament-cmake)
    (description "The Ament build system with CMake")
    (lower lower)))

(define-public ament-python-build-system python-build-system)
