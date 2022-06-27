(define-module (ros catkin)
  #:use-module (gnu packages check)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public python-catkin-pkg-modules
  (package
    (name "python-catkin-pkg-modules")
    (version "0.4.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros-infrastructure/catkin_pkg.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mkhm3ix3b0rpsqp6wqw8v1q815wjdkl5nb1fhn5j9p0cndx5sp3"))))
    (build-system python-build-system)
    (native-inputs
     (list
      python-flake8
      python-mock))
    (propagated-inputs
     (list
      python-dateutil
      python-docutils
      python-pyparsing))
    (home-page "https://github.com/ros-infrastructure/catkin_pkg")
    (synopsis "Standalone Python library for the Catkin package system")
    (description "Standalone Python library for the catkin build system.")
    (license license:bsd-3)))
