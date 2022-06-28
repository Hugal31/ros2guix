(define-module (ros2guix fixes)
  #:use-module (guix records)
  #:export (package-fixes
            get-package-fix
            this-package-fix
            package-fix?
            package-fix-build-system
            package-fix-native-inputs
            package-fix-inputs
            package-fix-propagated-inputs))

(define (sort-inputs inputs)
  (sort! inputs string<?))

(define-record-type* <package-fix>
  package-fix make-package-fix
  package-fix?
  this-package-fix
  (build-system package-fix-build-system (default #f))
  (native-inputs package-fix-native-inputs (default '()) (sanitize sort-inputs))
  (inputs package-fix-inputs (default '()) (sanitize sort-inputs))
  (propagated-inputs package-fix-propagated-inputs (default '()) (sanitize sort-inputs)))

(define null-package-fix (package-fix))

;; Alist of ros-name to package-fix
(define package-fixes
  `(("ament_cmake_core" . ,(package-fix
                            (build-system 'cmake-build-system)
                            (propagated-inputs (list
                                                "ament_package"
                                                "python"))))))

(define (get-package-fix name)
  (or (assoc-ref package-fixes name)
      null-package-fix))
