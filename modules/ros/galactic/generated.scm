(define-module
  (ros galactic generated)
  #:use-module
  (basic-packages)
  #:use-module
  (gnu packages)
  #:use-module
  (gnu packages cmake)
  #:use-module
  (gnu packages gcc)
  #:use-module
  (gnu packages compression)
  #:use-module
  (gnu packages benchmark)
  #:use-module
  (gnu packages elf)
  #:use-module
  (gnu packages networking)
  #:use-module
  (gnu packages python-xyz)
  #:use-module
  (gnu packages check)
  #:use-module
  (gnu packages version-control)
  #:use-module (ros catkin)
  #:use-module
  ((guix licenses) #:prefix license:)
  #:use-module
  (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module
  (guix packages)
  #:use-module
  (ros galactic build-system ament)
  #:use-module
  (ros galactic third-parts))

(define-public ros2cli
  (package
    (name "ros2cli")
    (version "0.13.3-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ros2cli-release.git")
               (commit "release/galactic/ros2cli/0.13.3-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0fxmwdvrcwmp9h898s66whh9wykfcs6v11a56q006hc1hxd3w782"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            ament-xmllint
            python-pytest
            test-msgs))
    (inputs (list))
    (propagated-inputs
      (list python-argcomplete
            python-importlib-metadata
            python-netifaces
            python-packaging
            ; python-pkg-resources
            rclpy))
    (home-page "https://github.com/ros2/ros2cli.git")
    (synopsis "ROS package ros2cli")
    (description
      "Framework for ROS 2 command line tools.")
    (license license:asl2.0)))

(define-public ament-copyright
  (package
    (name "ament-copyright")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_copyright/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1hxlpk72zhsi7vp5vshac3n17h0nirwilz38mbwnn0vfy2nmgx9z"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-flake8 ament-pep257 python-pytest))
    (inputs (list))
    (propagated-inputs
      (list ament-lint python-importlib-metadata))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_copyright")
    (description
      "The ability to check source files for copyright and license information.")
    (license license:asl2.0)))

(define-public ament-flake8
  (package
    (name "ament-flake8")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit "release/galactic/ament_flake8/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "14w7s06knjh8iq89bpd81nkvr7znzailqaprd0y73jfacf6vjw88"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list))
    (inputs (list))
    (propagated-inputs
      (list ament-lint python-flake8))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_flake8")
    (description
      "The ability to check code for style and syntax conventions with flake8.")
    (license license:asl2.0)))

(define-public ament-pep257
  (package
    (name "ament-pep257")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit "release/galactic/ament_pep257/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "13ah42znnl84965hfhzmkph2hzrdf2rq7yy361nxs9fn4whkcr6c"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-flake8 python-pytest))
    (inputs (list))
    (propagated-inputs
      (list ament-lint python-pydocstyle))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_pep257")
    (description
      "The ability to check code against the style conventions in PEP 8 and generate xUnit test result files.")
    (license license:asl2.0)))

(define-public ament-xmllint
  (package
    (name "ament-xmllint")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_xmllint/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "053mnlg14xwdsa4rkg9bclv5s9z157a5m4d6gk35r40j934hw277"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            python-pytest))
    (inputs (list))
    (propagated-inputs
      (list ament-lint libxml2))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_xmllint")
    (description
      "The ability to check XML files like the package manifest using xmllint and generate xUnit test result files.")
    (license license:asl2.0)))

(define-public test-msgs
  (package
    (name "test-msgs")
    (version "1.0.3-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcl_interfaces-release.git")
               (commit "release/galactic/test_msgs/1.0.3-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "02w8lvlgxif0ybzx3xzva4jim2qwf31rkcrca2vhckkmrqm8ksf6"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            rosidl-default-generators
            builtin-interfaces
            test-interface-files
            ament-lint-auto
            ament-lint-common
            ament-cmake-gtest))
    (inputs (list))
    (propagated-inputs
      (list builtin-interfaces rosidl-default-runtime))
    (home-page
      "https://github.com/ros2/rcl_interfaces.git")
    (synopsis "ROS package test_msgs")
    (description
      "A package containing message definitions and fixtures used exclusively for testing purposes.")
    (license license:asl2.0)))

(define-public rclpy
  (package
    (name "rclpy")
    (version "1.9.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rclpy-release.git")
               (commit "release/galactic/rclpy/1.9.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1slxh9qqii6vh444xsazclgiv6ql03jq63ackvm1d6y1s1sis2f3"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            python-cmake-module
            pybind11-vendor
            rcpputils
            rcutils
            rmw-implementation-cmake
            ament-cmake-gtest
            ament-cmake-pytest
            ament-lint-auto
            ament-lint-common
            python-pytest
            rosidl-generator-py
            test-msgs))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            builtin-interfaces
            rcl-interfaces
            rosgraph-msgs
            rpyutils))
    (home-page "https://github.com/ros2/rclpy.git")
    (synopsis "ROS package rclpy")
    (description
      "Package containing the Python client.")
    (license license:asl2.0)))

(define-public ament-cmake
  (package
    (name "ament-cmake")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit "release/galactic/ament_cmake/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0clg5df6k62zwkxb062vjwxk0ggpbibigbgpzfwpabgi9vz9w4pc"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list cmake
            ament-cmake-export-dependencies))
    (inputs (list))
    (propagated-inputs (list ament-cmake-python ament-cmake-core ament-cmake-libraries ament-cmake-export-definitions ament-cmake-export-dependencies ament-cmake-export-include-directories ament-cmake-export-targets ament-cmake-export-interfaces ament-cmake-export-libraries ament-cmake-export-link-flags ament-cmake-target-dependencies ament-cmake-include-directories ament-cmake-version ament-cmake-test))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake")
    (description
      "The entry point package for the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-include-directories
  (package
    (name "ament-cmake-include-directories")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_include_directories/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1w7z1w723bpx933qyjak3d8dibwkbbijwvlkag4alk24827ybrmi"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_include_directories")
    (description
      "The functionality to order include directories according to a chain of prefixes in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-export-link-flags
  (package
    (name "ament-cmake-export-link-flags")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_link_flags/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0b9za030hg6a9jfdv2qbb9phpzz5bv718rs75m901k8yj2djvmv9"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_link_flags")
    (description
      "The ability to export link flags to downstream packages in the ament buildsystem.")
    (license license:asl2.0)))

(define-public python-cmake-module
  (package
    (name "python-cmake-module")
    (version "0.8.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/python_cmake_module-release.git")
               (commit
                 "release/galactic/python_cmake_module/0.8.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0li9qv8875snbf43fjbi990b7fpcphyb0s02v9hpym0mshmhpr9c"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/python_cmake_module.git")
    (synopsis "ROS package python_cmake_module")
    (description
      "Provide CMake module with extra functionality for Python.")
    (license license:asl2.0)))

(define-public pybind11-vendor
  pybind11
  ;; (package
  ;;   (name "pybind11-vendor")
  ;;   (version "2.2.6-2")
  ;;   (source
  ;;     (origin
  ;;       (method git-fetch)
  ;;       (uri (git-reference
  ;;              (url "https://github.com/ros2-gbp/pybind11_vendor-release.git")
  ;;              (commit
  ;;                "release/galactic/pybind11_vendor/2.2.6-2")))
  ;;       (file-name (git-file-name name version))
  ;;       (sha256
  ;;         (base32
  ;;           "11jwmb8asly3rrm2a8ar7ab4rcqqc97gss7ypf0f7z8nd8jyi41c"))))
  ;;   (build-system ament-cmake-build-system)
  ;;   (arguments '(#:tests? #f))
  ;;   (native-inputs (list ament-cmake))
  ;;   (inputs (list))
  ;;   (propagated-inputs (list))
  ;;   (home-page "https://github.com/pybind/pybind11")
  ;;   (synopsis "ROS package pybind11_vendor")
  ;;   (description "Wrapper around pybind11.")
  ;;   (license license:asl2.0))
  )

(define-public rcpputils
  (package
    (name "rcpputils")
    (version "2.2.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcpputils-release.git")
               (commit "release/galactic/rcpputils/2.2.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0r8qy5bw8p284wczmrn4kgc2gf20270ciaw54mcxk11ll1pp7ynm"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-ros
            ament-lint-common
            ament-lint-auto
            ament-cmake-gtest))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/rcpputils.git")
    (synopsis "ROS package rcpputils")
    (description
      "Package containing utility code for C++.")
    (license license:asl2.0)))

(define-public rcutils
  (package
    (name "rcutils")
    (version "4.0.2-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcutils-release.git")
               (commit "release/galactic/rcutils/4.0.2-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0972fb0p3b98399y5xc99izvfjinyf3v9hv2m219hq7q03kj2kc3"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            python-empy
            ament-cmake-gmock
            ament-cmake-gtest
            ament-cmake-pytest
            ament-lint-common
            ament-lint-auto
            mimick-vendor
            launch
            launch-testing
            launch-testing-ament-cmake
            osrf-testing-tools-cpp
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rcutils.git")
    (synopsis "ROS package rcutils")
    (description
      "Package containing various utility types and functions for C")
    (license license:asl2.0)))

(define-public rmw-implementation-cmake
  (package
    (name "rmw-implementation-cmake")
    (version "3.3.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rmw-release.git")
               (commit
                 "release/galactic/rmw_implementation_cmake/3.3.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "129plb3m7sp74z9c3gzakkqj1ywgqm470g11rdqxhp5izizppkm7"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rmw.git")
    (synopsis "ROS package rmw_implementation_cmake")
    (description
      "CMake functions which can discover and enumerate available implementations.")
    (license license:asl2.0)))

(define-public ament-cmake-gtest
  (package
    (name "ament-cmake-gtest")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_gtest/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0f984sjgnxymwxvpdz52l9hrlrhszyqzxkng0p46x406gxhpgp00"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list python))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_gtest")
    (description
      "The ability to add gtest-based tests in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-pytest
  (package
    (name "ament-cmake-pytest")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_pytest/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "04ilrhf4vya2nimnjwx8fw9swhj9rc5hdvql3zjhfc2gxvp9mv4i"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_pytest")
    (description
      "The ability to run Python tests using pytest in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-lint-auto
  (package
    (name "ament-lint-auto")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_lint_auto/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0rl15mr46mwr6wr5a95lhas6685sclyibxkalhb69hpggz2myby0"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core ament-cmake-test))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_lint_auto")
    (description
      "The auto-magic functions for ease to use of the ament linters in CMake.")
    (license license:asl2.0)))

(define-public ament-lint-common
  (package
    (name "ament-lint-common")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_lint_common/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1f4vbzz1740asrrv2c7qlfjd5zqmw4sv9algr2sqyin51qcwzs4a"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-export-dependencies))
    (inputs (list))
    (propagated-inputs
      (list ament-cmake-copyright
            ament-cmake-cppcheck
            ament-cmake-cpplint
            ament-cmake-flake8
            ament-cmake-lint-cmake
            ament-cmake-pep257
            ament-cmake-uncrustify
            ament-cmake-xmllint))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_lint_common")
    (description
      "The list of commonly used linters in the ament buildsytem in CMake.")
    (license license:asl2.0)))

(define-public rosidl-generator-py
  (package
    (name "rosidl-generator-py")
    (version "0.11.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_python-release.git")
               (commit
                 "release/galactic/rosidl_generator_py/0.11.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1i4a58w77mgx2n5yb1657jzz4cffnfijacgdq08mml855d4rhnsx"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            rosidl-runtime-c
            ament-cmake-pytest
            ament-index-python
            ament-lint-auto
            ament-lint-common
            python-numpy
            python-pytest
            python-cmake-module
            rmw
            rosidl-cmake
            rosidl-typesupport-introspection-c
            rosidl-typesupport-fastrtps-c
            rosidl-generator-c
            rosidl-generator-cpp
            rosidl-parser
            rosidl-typesupport-c
            rpyutils
            test-interface-files))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python python-numpy rosidl-cli))
    (home-page
      "https://github.com/ros2/rosidl_python.git")
    (synopsis "ROS package rosidl_generator_py")
    (description
      "Generate the ROS interfaces in Python.")
    (license license:asl2.0)))

(define-public ament-index-python
  (package
    (name "ament-index-python")
    (version "1.2.0-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_index-release.git")
               (commit
                 "release/galactic/ament_index_python/1.2.0-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0q0hf1h962rjg7w8sbmmfg34kssnw9j5h0ks7sj0f57dy8dgliq2"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            python-pytest))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_index.git")
    (synopsis "ROS package ament_index_python")
    (description
      "Python API to access the ament resource index.")
    (license license:asl2.0)))

(define-public builtin-interfaces
  (package
    (name "builtin-interfaces")
    (version "1.0.3-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcl_interfaces-release.git")
               (commit
                 "release/galactic/builtin_interfaces/1.0.3-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "13m43pk6gpjk8d594gnpiqra5ghcgbgiz2w4llfriycirr6ngaly"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            rosidl-default-generators
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list rosidl-default-runtime))
    (home-page
      "https://github.com/ros2/rcl_interfaces.git")
    (synopsis "ROS package builtin_interfaces")
    (description
      "A package containing message and service definitions for types defined in the OMG IDL Platform Specific Model.")
    (license license:asl2.0)))

(define-public rcl-interfaces
  (package
    (name "rcl-interfaces")
    (version "1.0.3-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcl_interfaces-release.git")
               (commit
                 "release/galactic/rcl_interfaces/1.0.3-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1zvwmnfz6jgv9vcrcbzvcfi98ivkfpa77qxm15z0nncqdfqcn2w4"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            rosidl-default-generators
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list rosidl-default-runtime))
    (home-page
      "https://github.com/ros2/rcl_interfaces.git")
    (synopsis "ROS package rcl_interfaces")
    (description
      "The ROS client library common interfaces. This package contains the messages and services which ROS client libraries will use under the hood to communicate higher level concepts such as parameters.")
    (license license:asl2.0)))

(define-public rosgraph-msgs
  (package
    (name "rosgraph-msgs")
    (version "1.0.3-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rcl_interfaces-release.git")
               (commit "release/galactic/rosgraph_msgs/1.0.3-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "187xp39nb4x1m81x188qh00c0wrzj4wbcray3a0hbp5xbg1b24bd"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            rosidl-default-generators
            builtin-interfaces
            ament-lint-common))
    (inputs (list))
    (propagated-inputs
      (list builtin-interfaces rosidl-default-runtime))
    (home-page
      "https://github.com/ros2/rcl_interfaces.git")
    (synopsis "ROS package rosgraph_msgs")
    (description
      "Messages relating to the ROS Computation Graph. These are generally considered to be low-level messages that end users do not interact with.")
    (license license:asl2.0)))

(define-public rpyutils
  (package
    (name "rpyutils")
    (version "0.2.0-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rpyutils-release.git")
               (commit "release/galactic/rpyutils/0.2.0-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "16p7gzjn1iixrbz1r9s00pnk1blhx967c7qavg9a50xlcn3v63z8"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            ament-xmllint
            python-pytest))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/rpyutils.git")
    (synopsis "ROS package rpyutils")
    (description
      "Package containing various utility types and functions for Python")
    (license license:asl2.0)))

(define-public rosidl-default-generators
  (package
    (name "rosidl-default-generators")
    (version "1.1.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_defaults-release.git")
               (commit
                 "release/galactic/rosidl_default_generators/1.1.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0czir7qf458prv5c70hx7adw9lfdp5vk6jfxj4lmppj85ihk8kdc"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/rosidl_defaults.git")
    (synopsis
      "ROS package rosidl_default_generators")
    (description
      "A configuration package defining the default ROS interface generators.")
    (license license:asl2.0)))

(define-public test-interface-files
  (package
    (name "test-interface-files")
    (version "0.8.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/test_interface_files-release.git")
               (commit
                 "release/galactic/test_interface_files/0.8.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "00l4r8apprxkgbzjns78a6rdg4xi2j9012p5137ilgbl8r6qxdnl"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/test_interface_files.git")
    (synopsis "ROS package test_interface_files")
    (description
      "A package containing message definitions and fixtures used exclusively for testing purposes.")
    (license license:asl2.0)))

(define-public rosidl-default-runtime
  (package
    (name "rosidl-default-runtime")
    (version "1.1.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_defaults-release.git")
               (commit
                 "release/galactic/rosidl_default_runtime/1.1.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1y9a9js9bgilicn0hj24773yxxfh37sknx74w6n0q7619sqjf6w0"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     (list ament-cmake
           ament-xmllint
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/rosidl_defaults.git")
    (synopsis "ROS package rosidl_default_runtime")
    (description
      "A configuration package defining the runtime for the ROS interfaces.")
    (license license:asl2.0)))

(define-public ament-cmake-version
  (package
    (name "ament-cmake-version")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_version/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1fzk65v9axp6w5wpr7cxqs23dn41bafk9792yqbyn66zfb2f867i"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_version")
    (description
      "The ability to override the exported package version in the ament buildsystem.")
    (license license:asl2.0)))

(define-public ament-lint
  (package
    (name "ament-lint")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit "release/galactic/ament_lint/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1d4ab7fxmsikzh4p5dkqx6sxwrz4ss0ni2hgp3qmcv371l64y7ms"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_lint")
    (description
      "Providing common API for ament linter packages.")
    (license license:asl2.0)))

(define-public ament-cmake-core
  (package
    (name "ament-cmake-core")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_core/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1nij20icrq17qrmw4j9kdx6i3z7kjh8hynv9m4ga53dp70h5vsym"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list cmake
            ament-package
            python-catkin-pkg-modules))
    (inputs (list))
    (propagated-inputs (list ament-package cmake python python-catkin-pkg-modules))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_core")
    (description
      "The core of the ament buildsystem in CMake. Several subcomponents provide specific funtionalities: * environment: provide prefix-level setup files * environment_hooks: provide package-level setup files and environment hooks * index: store information in an index and retrieve them without crawling * package_templates: templates from the ament_package Python package * symlink_install: use symlinks for CMake install commands")
    (license license:asl2.0)))

(define-public ament-cmake-export-interfaces
  (package
    (name "ament-cmake-export-interfaces")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_interfaces/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1dqy2r17rpl6ns4w7b2dnhjqd7ygyz8z4j30zyjc4r4l9pg1qhr8"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_interfaces")
    (description
      "The ability to export interfaces to downstream packages in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-export-libraries
  (package
    (name "ament-cmake-export-libraries")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_libraries/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1xp4gbfpiadl7n5nrphziwjl714g5fgfniw44rml4vi8q4p0058r"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_libraries")
    (description
      "The ability to export libraries to downstream packages in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-export-dependencies
  (package
    (name "ament-cmake-export-dependencies")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_dependencies/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1clh8c3sipgan8xsmip91nk35vgzak7hxl7mhxzdkak9avq0fnzc"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_dependencies")
    (description
      "The ability to export dependencies to downstream packages in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-copyright
  (package
    (name "ament-cmake-copyright")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_copyright/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "18rf3is5yn6rayxgxwkwpmx5a6srr14vlhzvl042vhfzddma95ls"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-copyright
            ament-cmake-lint-cmake))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_copyright")
    (description
      "The CMake API for ament_copyright to check every source file contains copyright reference.")
    (license license:asl2.0)))

(define-public ament-cmake-cppcheck
  (package
    (name "ament-cmake-cppcheck")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_cppcheck/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0wzxx6mgwvrwkkpxqh6qarqw5fk5969piw1swfijwfca3as96mm4"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_cppcheck")
    (description
      "The CMake API for ament_cppcheck to perform static code analysis on C/C++ code using Cppcheck.")
    (license license:asl2.0)))

(define-public ament-cmake-cpplint
  (package
    (name "ament-cmake-cpplint")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_cpplint/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0pamwwnznfcpxyz7kb6mfanj6kv4sz2n1lgygln6qmnq5nb0145n"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_cpplint")
    (description
      "The CMake API for ament_cpplint to lint C / C++ code using cpplint.")
    (license license:asl2.0)))

(define-public ament-cmake-flake8
  (package
    (name "ament-cmake-flake8")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_flake8/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1b49jpnpbkv2fvas4vhzpccd3swdc5kmw1h1flsdfygf1ynn788x"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_flake8")
    (description
      "The CMake API for ament_flake8 to check code syntax and style conventions with flake8.")
    (license license:asl2.0)))

(define-public ament-cmake-lint-cmake
  (package
    (name "ament-cmake-lint-cmake")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_lint_cmake/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1h834fgilsh8gri53v967f7r2pzf99vpdhcvwikdxmr39n9lscv3"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-lint-cmake))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_lint_cmake")
    (description
      "The CMake API for ament_lint_cmake to lint CMake code using cmakelint.")
    (license license:asl2.0)))

(define-public ament-cmake-pep257
  (package
    (name "ament-cmake-pep257")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_pep257/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0irb3zvcflzlx5cxs5imd1l76digaf73qmvly2342irv7nb69hcj"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_pep257")
    (description
      "The CMake API for ament_pep257 to check code against the style conventions in PEP 257.")
    (license license:asl2.0)))

(define-public ament-cmake-uncrustify
  (package
    (name "ament-cmake-uncrustify")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_uncrustify/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0z0yw9y6sqwzr8zbxgs0lrm7akacn94mss5b0hxczsidqz3hrac1"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_uncrustify")
    (description
      "The CMake API for ament_uncrustify to check code against styleconventions using uncrustify.")
    (license license:asl2.0)))

(define-public ament-cmake-xmllint
  (package
    (name "ament-cmake-xmllint")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_xmllint/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0wydj7nw8xllsixd24rr3w3n5cf101r0hk08if4k79isfajg20bx"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list ament-xmllint))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_xmllint")
    (description
      "The CMake API for ament_xmllint to check XML file using xmmlint.")
    (license license:asl2.0)))

(define-public ament-cmake-test
  (package
    (name "ament-cmake-test")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_test/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "161ifb8rypygsnlq2cj8g4ylksqq2zlmf2s89jf3plyph4ld8nz6"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core ament-cmake-python))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_test")
    (description
      "The ability to add tests in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public rosidl-runtime-c
  (package
    (name "rosidl-runtime-c")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_runtime_c/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1s63f91rsnf22ghs0jz6ncx5sw8mm7589yg1y4rr2cd4invib8k5"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            rosidl-typesupport-interface
            ament-lint-auto
            ament-lint-common
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_runtime_c")
    (description
      "Provides definitions, initialization and finalization functions, and macros for getting and working with rosidl typesupport types in C.")
    (license license:asl2.0)))

(define-public rmw
  (package
    (name "rmw")
    (version "3.3.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rmw-release.git")
               (commit "release/galactic/rmw/3.3.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0bp22alma2i2har7f3hmz69crgcxlzsj3l3l0fhw5cd3441a5rwn"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            ament-cmake-version
            rcutils
            rosidl-runtime-c
            ament-cmake-gmock
            ament-lint-auto
            ament-lint-common
            osrf-testing-tools-cpp))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rmw.git")
    (synopsis "ROS package rmw")
    (description "Contains the ROS middleware API.")
    (license license:asl2.0)))

(define-public rosidl-cmake
  (package
    (name "rosidl-cmake")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit "release/galactic/rosidl_cmake/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1j797n147rv9ddv80hrmhrsgybv9c72jbgm43pnadfhkhcba7489"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-python
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs
      (list rosidl-adapter rosidl-parser))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_cmake")
    (description
      "The CMake functionality to invoke code generation for ROS interface files.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-introspection-c
  (package
    (name "rosidl-typesupport-introspection-c")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_typesupport_introspection_c/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1j94b8b1mvx4aj2p8qm9cgi6akw8s0i7ag23az2z52z0iwhcxgi1"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-cmake
            rosidl-parser))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis
      "ROS package rosidl_typesupport_introspection_c")
    (description
      "Generate the message type support for dynamic message construction in C.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-fastrtps-c
  (package
    (name "rosidl-typesupport-fastrtps-c")
    (version "1.2.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_typesupport_fastrtps-release.git")
               (commit
                 "release/galactic/rosidl_typesupport_fastrtps_c/1.2.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "15w3nkc3mmym3zwx4ypla03jadjn3i94f7l48gsjbbhhgb08d3hh"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            fastrtps-cmake-module
            fastcdr
            fastrtps
            rosidl-cmake
            rosidl-runtime-c
            rosidl-typesupport-fastrtps-cpp
            ament-cmake-gtest
            ament-lint-auto
            ament-lint-common
            osrf-testing-tools-cpp
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-typesupport-interface))
    (home-page
      "https://github.com/ros2/rosidl_typesupport_fastrtps.git")
    (synopsis
      "ROS package rosidl_typesupport_fastrtps_c")
    (description
      "Generate the C interfaces for eProsima FastRTPS.")
    (license license:asl2.0)))

(define-public rosidl-generator-c
  (package
    (name "rosidl-generator-c")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_generator_c/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0dw99d9258zyg2hgvq0r15rqy7068nh80h17n6dnyv6f3xkgfz15"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-python
            ament-cmake-ros
            ament-cmake-gtest
            ament-lint-auto
            ament-lint-common
            rosidl-cmake
            rosidl-runtime-c
            test-interface-files))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-parser))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_generator_c")
    (description "Generate the ROS interfaces in C.")
    (license license:asl2.0)))

(define-public rosidl-generator-cpp
  (package
    (name "rosidl-generator-cpp")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_generator_cpp/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0kkgda7k10s1va1xh6vlblqlb52m09877sz0z9l1pvn1k4dns8vs"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-gtest
            ament-lint-auto
            ament-lint-common
            rosidl-cmake
            rosidl-runtime-cpp
            rosidl-runtime-c
            test-interface-files))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-parser))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_generator_cpp")
    (description
      "Generate the ROS interfaces in C++.")
    (license license:asl2.0)))

(define-public rosidl-parser
  (package
    (name "rosidl-parser")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit "release/galactic/rosidl_parser/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "069fk72lsgjnk35d9cnwbm7sh4v8ya6xw1zlcx54vnh063b075wb"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-pytest
            ament-lint-auto
            ament-lint-common
            python-pytest))
    (inputs (list))
    (propagated-inputs
      (list python-lark-parser rosidl-adapter))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_parser")
    (description
      "The parser for `.idl` ROS interface files.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-c
  (package
    (name "rosidl-typesupport-c")
    (version "1.2.1-3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_typesupport-release.git")
               (commit
                 "release/galactic/rosidl_typesupport_c/1.2.1-3")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1mjfdqdirm63xpsxilz45dy1rxc9vwzsj78m5pfdhccb2qlcp5nr"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-ros
            rosidl-runtime-c
            rosidl-typesupport-introspection-c
            ament-lint-auto
            ament-lint-common
            mimick-vendor
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-typesupport-interface))
    (home-page
      "https://github.com/ros2/rosidl_typesupport.git")
    (synopsis "ROS package rosidl_typesupport_c")
    (description
      "Generate the type support for C messages.")
    (license license:asl2.0)))

(define-public rosidl-cli
  (package
    (name "rosidl-cli")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit "release/galactic/rosidl_cli/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "00f86j57vydg3k9fid3qws4djya74ir4yzcgv9ngjpbpy9n40b7h"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            ament-xmllint
            python-pytest
            python-argcomplete
            python-importlib-metadata))

    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_cli")
    (description
      "Command line tools for ROS interface generation.")
    (license license:asl2.0)))

(define-public ament-cmake-ros
  (package
    (name "ament-cmake-ros")
    (version "0.9.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake_ros-release.git")
               (commit
                 "release/galactic/ament_cmake_ros/0.9.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0plnpcin1d8d0jchrj9z2zs1cwsl98mxa0wkfwasznqb44fqkqkz"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     (list ament-cmake
           ament-pep257
           ament-xmllint
            ament-lint-auto
            ament-lint-common
            ament-flake8))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/ament_cmake_ros.git")
    (synopsis "ROS package ament_cmake_ros")
    (description
      "The ROS specific CMake bits in the ament buildsystem.")
    (license license:asl2.0)))

(define-public ament-cmake-gmock
  (package
    (name "ament-cmake-gmock")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_gmock/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "15a2rmnbvy5f1icx402z7hmd0py8g7n30gzvkvj82pd5kr956j94"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_gmock")
    (description
      "The ability to add Google mock-based tests in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public mimick
  (package
   (name "mimick")
   (version "0.2.6-2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ros2/Mimick")
           (commit "f171450b5ebaa3d2538c762a059dfc6ab7a01039")))
     (file-name (git-file-name name version))
     (sha256
      (base32
        "0j1gf346alnmr0vws4hwl1xbd6cb4a8lpmk39y7bidpahyqn5jpm"))))
   (build-system cmake-build-system)
   (home-page "https://github.com/ros2/Mimick")
   (synopsis "A KISS, cross-platform C mocking library")
   (description
    "Mimick aims to be a simple of use and powerful mocking and stubbing library for C.

It doesn't rely on external code generation or compiler plugin to work -- simply link the library to your tests, and you're good to go!")
   (license license:expat)))

(define-public mimick-vendor
  (package
   (name "mimick-vendor")
   (version "0.2.6-2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/ros2-gbp/mimick_vendor-release.git")
           (commit "release/galactic/mimick_vendor/0.2.6-2")))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0284k61lh32cad6bpkq17xfr2n3x112g165skn7w4rr627iyhhwq"))))
   (build-system ament-cmake-build-system)
   (arguments
    `(#:tests? #f
      ;; Prevent CMake from cloning mimick
      #:phases (modify-phases %standard-phases
                 (add-before 'configure 'patch-git-download
                             (lambda* (#:key inputs #:allow-other-keys #:rest keys)
                                      (substitute* "CMakeLists.txt"
                                        (("GIT_REPOSITORY .+$")
                                         (string-append "SOURCE_DIR " (assoc-ref inputs "mimick")))))))))
   (native-inputs
    `(("ament-cmake" ,ament-cmake)
      ("ament-lint-auto" ,ament-lint-auto)
      ("ament-lint-common" ,ament-lint-common)
      ("mimick" ,(package-source mimick))))
   (inputs (list))
   (propagated-inputs (list))
   (home-page "https://github.com/Snaipe/Mimick")
   (synopsis "ROS package mimick_vendor")
   (description
    "Wrapper around mimick, it provides an ExternalProject build of mimick.")
   (license license:asl2.0)))

(define-public launch
  (package
    (name "launch")
    (version "0.17.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/launch-release.git")
               (commit "release/galactic/launch/0.17.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1diar2327mk9w5341gqy3r2nl6chv2wr0vjx55nncpc7586r02af"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            python-pytest
            osrf-pycommon))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            python-importlib-metadata
            python-lark-parser
            python-pyyaml))
    (home-page "https://github.com/ros2/launch.git")
    (synopsis "ROS package launch")
    (description "The ROS launch tool.")
    (license license:asl2.0)))

(define-public launch-testing
  (package
    (name "launch-testing")
    (version "0.17.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/launch-release.git")
               (commit
                 "release/galactic/launch_testing/0.17.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1rngdn6lxnrvjk88l2l79wyckq5n57libpg9vascp2mgs19nmapq"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            launch
            python-pytest))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python osrf-pycommon))
    (home-page "https://github.com/ros2/launch.git")
    (synopsis "ROS package launch_testing")
    (description
      "A package to create tests which involve launch files and multiple processes.")
    (license license:asl2.0)))

(define-public launch-testing-ament-cmake
  (package
    (name "launch-testing-ament-cmake")
    (version "0.17.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/launch-release.git")
               (commit
                 "release/galactic/launch_testing_ament_cmake/0.17.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "06sgwr3wmsilpnhf51rilbf58w3vsf288xgllr3bj5xw87am3d90"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-copyright
            python-cmake-module
            launch-testing))
    (inputs (list))
    (propagated-inputs (list launch-testing))
    (home-page "https://github.com/ros2/launch.git")
    (synopsis
      "ROS package launch_testing_ament_cmake")
    (description
      "A package providing cmake functions for running launch tests from the build.")
    (license license:asl2.0)))

(define-public osrf-testing-tools-cpp
  (package
    (name "osrf-testing-tools-cpp")
    (version "1.4.0-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/osrf_testing_tools_cpp-release.git")
               (commit
                "release/galactic/osrf_testing_tools_cpp/1.4.0-2")))
        (patches (search-patches "patches/osrf-testing-tools-cpp-zlib.patch"))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0pxw31qxdvpf4dgkhyj7k0dsa45zd9y9jaynq3yxmswp6f2bsh7a"))))
    (build-system cmake-build-system)
    (arguments
     ;; For some reason, libdw and libz do not want to be in the RPATH by default
     '(#:configure-flags
       `(,(string-append "-DCMAKE_BUILD_RPATH=" (assoc-ref %build-inputs "zlib") "/lib"
                         ";" (assoc-ref %build-inputs "elfutils") "/lib")
         ,(string-append "-DCMAKE_INSTALL_RPATH=" (assoc-ref %build-inputs "zlib") "/lib"
                         ";" (assoc-ref %build-inputs "elfutils") "/lib"
                         ";" (assoc-ref %outputs "out") "/lib"))))
    (native-inputs (list python libiberty-pic binutils-pic))
    ;; TODO tests cannot link with zlib, weird, it is not in the runpath. Also, the library doesn't mention the zlib, but links against it for some reason.
    ;; Idea: explicitely link agains the zlib (with a CMake patch), so the runpath should be fixed.
    (inputs (list elfutils zlib))
    (propagated-inputs (list))
    (home-page
      "https://github.com/osrf/osrf_testing_tools_cpp.git")
    (synopsis "ROS package osrf_testing_tools_cpp")
    (description
      "Testing tools for C++, and is used in various OSRF projects.")
    (license license:asl2.0)))

(define-public performance-test-fixture
  (package
    (name "performance-test-fixture")
    (version "0.0.8-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/performance_test_fixture-release.git")
               (commit
                 "release/galactic/performance_test_fixture/0.0.8-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0mml5wr8l1fjd3kksrccpg4wpb1nvhkjv0bif3z1pskwk76s0z4h"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-lint-auto
            ament-lint-common
            benchmark))
    (inputs (list osrf-testing-tools-cpp))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/performance_test_fixture.git")
    (synopsis "ROS package performance_test_fixture")
    (description
      "Test fixture and CMake macro for using osrf_testing_tools_cpp with Google Benchmark")
    (license license:asl2.0)))

(define-public ament-package
  (package
    (name "ament-package")
    (version "0.13.1-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_package-release.git")
               (commit
                 "release/galactic/ament_package/0.13.1-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1k6yb951mrxnsda92gkyj81b3lzggr62bkkjjlbmlm5pm0d19194"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list python-flake8 python-pytest))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_package.git")
    (synopsis "ROS package ament_package")
    (description
      "The parser for the manifest files in the ament buildsystem.")
    (license license:asl2.0)))

(define-public ament-cmake-export-targets
  (package
    (name "ament-cmake-export-targets")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_targets/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0k2bn2msl5vvb8zrpkdpxbcyvihhy14fx107rwl4pw27i90911w0"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_targets")
    (description
      "The ability to export targets to downstream packages in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-google-benchmark
  (package
    (name "ament-cmake-google-benchmark")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_google_benchmark/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1ldh1c69dl8d45zl6ap2pgd5jx4b0r9j1vnd7d3h8bb33ksf9c3b"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-export-dependencies
            ament-cmake-python))
    (inputs (list))
    (propagated-inputs
      (list ament-cmake-test googlebenchmark))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_google_benchmark")
    (description
      "The ability to add Google Benchmark tests in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public osrf-pycommon
  (package
    (name "osrf-pycommon")
    (version "2.0.0-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/osrf_pycommon-release.git")
               (commit "release/galactic/osrf_pycommon/2.0.0-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "16nb7ky5mf5si4x4hp2whqnq8rx1240yvfs60fvqjcm8lwa55558"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list))
    (inputs (list))
    (propagated-inputs
      (list python-importlib-metadata python-mock))
    (home-page
      "https://github.com/osrf/osrf_pycommon.git")
    (synopsis "ROS package osrf_pycommon")
    (description
      "Commonly needed Python modules, used by Python software developed at OSRF.")
    (license license:asl2.0)))

(define-public ament-cmake-python
  (package
    (name "ament-cmake-python")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_python/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1kcdfxgxvpx2sjwdynnhm7j3z602i2d3fyw1fcl7cd6q6nn0rkw1"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list python))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_python")
    (description
      "The ability to use Python in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-lint-cmake
  (package
    (name "ament-lint-cmake")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_lint_cmake/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1p2ksfp273syx67cv02gm7yaj85b1npjrmc3amycws38hdi8p0ls"))))
    (build-system ament-python-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-copyright
            ament-flake8
            ament-pep257
            python-pytest))
    (inputs (list python-pyaml))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_lint_cmake")
    (description
      "The ability to lint CMake code using cmakelint and generate xUnit test result files.")
    (license license:asl2.0)))

(define-public ament-cmake-export-definitions
  (package
    (name "ament-cmake-export-definitions")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_definitions/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0nrmgchc649w5vkqmxgsazqfrxc2fnrihn23zkmgal854dmmhnfw"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_definitions")
    (description
      "The ability to export definitions to downstream packages in the ament buildsystem.")
    (license license:asl2.0)))

(define-public ament-cmake-target-dependencies
  (package
    (name "ament-cmake-target-dependencies")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_target_dependencies/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "180iil3zlz3ypy9944na3n700gnfgcfh56wxijd6vknccfwc5h1h"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_target_dependencies")
    (description
      "The ability to add definitions, include directories and libraries of a package to a target in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-interface
  (package
    (name "rosidl-typesupport-interface")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_typesupport_interface/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1xwhg961icbmws9lj52zjqbni72wqlsqa3720nq6amq4vnz4ypis"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-lint-auto
            ament-lint-common
            ament-cmake-gtest))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis
      "ROS package rosidl_typesupport_interface")
    (description
      "The interface for rosidl typesupport packages.")
    (license license:asl2.0)))

(define-public rosidl-adapter
  (package
    (name "rosidl-adapter")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_adapter/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0ha2jdz5nbmxz53c2lwzfgv2kn74vbkc63q9ds8jqjyhlbji0gds"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-pytest
            ament-lint-common
            ament-lint-auto))
    (inputs (list))
    (propagated-inputs (list python-empy rosidl-cli))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_adapter")
    (description
      "API and scripts to parse .msg/.srv/.action files and convert them to .idl.")
    (license license:asl2.0)))

(define-public rosidl-runtime-cpp
  (package
    (name "rosidl-runtime-cpp")
    (version "2.2.2-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl-release.git")
               (commit
                 "release/galactic/rosidl_runtime_cpp/2.2.2-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "11shzng0mriqfy4x061z0rr5a69vr9my4dxq4504x4gbks09v7na"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            ament-cmake-gtest
            ament-lint-auto
            ament-lint-common
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs (list))
    (home-page "https://github.com/ros2/rosidl.git")
    (synopsis "ROS package rosidl_runtime_cpp")
    (description
      "Provides definitions and templated functions for getting and working with rosidl typesupport types in C++.")
    (license license:asl2.0)))

(define-public fastrtps-cmake-module
  (package
    (name "fastrtps-cmake-module")
    (version "1.2.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_typesupport_fastrtps-release.git")
               (commit
                 "release/galactic/fastrtps_cmake_module/1.2.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0v0hwhxsp3vp4lv49jyg1qca9b0qdxrvvim39cmjy0jkpz7nby01"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
     (list ament-cmake
           ament-xmllint
            ament-lint-auto
            ament-lint-common))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ros2/rosidl_typesupport_fastrtps.git")
    (synopsis "ROS package fastrtps_cmake_module")
    (description
      "Provide CMake module to find eProsima FastRTPS.")
    (license license:asl2.0)))

(define-public fastcdr
  (package
    (name "fastcdr")
    (version "1.0.20-3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/fastcdr-release.git")
               (commit "release/galactic/fastcdr/1.0.20-3")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1bd9i8c92fh9bha3z3p9gx7xxm9g2xb4rcna4wknkgd2x6ds7g8q"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/eProsima/Fast-CDR.git")
    (synopsis "ROS package fastcdr")
    (description "CDR serialization implementation.")
    (license license:asl2.0)))

(define-public fastrtps
  (package
    (name "fastrtps")
    (version "2.3.4-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/fastrtps-release.git")
               (commit "release/galactic/fastrtps/2.3.4-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1zqr06nc8whp9xwqa2h9ny5mhv91gqmvgkxc66y88ivs8a22jaq1"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list cmake asio tinyxml2 foonathan-memory))
    (inputs (list wolfssl fastcdr))
    (propagated-inputs (list))
    (home-page
      "https://github.com/eProsima/Fast-DDS.git")
    (synopsis "ROS package fastrtps")
    (description "Implementation of RTPS standard.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-fastrtps-cpp
  (package
    (name "rosidl-typesupport-fastrtps-cpp")
    (version "1.2.1-2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/rosidl_typesupport_fastrtps-release.git")
               (commit
                 "release/galactic/rosidl_typesupport_fastrtps_cpp/1.2.1-2")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0wiv8kyqmn36ixmsdirhrbqsfayych8bjd70hc4caamh6j23bwc9"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake
            fastrtps-cmake-module
            fastcdr
            fastrtps
            rosidl-cmake
            rosidl-runtime-c
            rosidl-runtime-cpp
            ament-cmake-gtest
            ament-lint-auto
            ament-lint-common
            osrf-testing-tools-cpp
            performance-test-fixture))
    (inputs (list))
    (propagated-inputs
      (list ament-index-python
            rosidl-cli
            rosidl-typesupport-interface))
    (home-page
      "https://github.com/ros2/rosidl_typesupport_fastrtps.git")
    (synopsis
      "ROS package rosidl_typesupport_fastrtps_cpp")
    (description
      "Generate the C++ interfaces for eProsima FastRTPS.")
    (license license:asl2.0)))

(define-public ament-cmake-libraries
  (package
    (name "ament-cmake-libraries")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_libraries/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1f7pwdv0j5fvyn9s9vipg8b1finmfn2h0sz7l1fvvycfbmn14qbb"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis "ROS package ament_cmake_libraries")
    (description
      "The functionality to deduplicate libraries in the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake-export-include-directories
  (package
    (name "ament-cmake-export-include-directories")
    (version "1.1.5-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_cmake-release.git")
               (commit
                 "release/galactic/ament_cmake_export_include_directories/1.1.5-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "023xjdj9rkdjhwas071iid2gcl5izc7gwkrghx4zq2a7ghd06s9d"))))
    (build-system ament-cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs (list ament-cmake-core))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_cmake.git")
    (synopsis
      "ROS package ament_cmake_export_include_directories")
    (description
      "The ability to export include directories to downstream packages in the ament buildsystem in CMake.")
    (license license:asl2.0)))
