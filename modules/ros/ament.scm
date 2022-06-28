(define-module (ros ament)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (ros catkin))

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
   (build-system python-build-system)
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

(define (transform-string s c1 c2)
  (string-map (lambda (c) (if (char=? c c1) c2 c)) s))

(define (dash-to-underscores s)
  (transform-string s #\- #\_))

(define* (make-ros-release-package
          nam
          #:optional (repo-name (dash-to-underscores nam))
          #:key version release (distro "galactic") (provider "ros2-gbp") hash)
  (package
    (name nam)
    (version version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://github.com/" provider "/" repo-name "-release.git"))
             (commit (string-append "release/" distro "/" (dash-to-underscores nam) "/" release))))
       (file-name (git-file-name name version))
       (sha256 hash)))
    (home-page #f)
    (build-system #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define* (make-ament-cmake-package
          nam
          #:key (version "1.1.5") (release "1.1.5-1") (distro "galactic")
          hash)
  (package
    (inherit (make-ros-release-package
              nam
              "ament_cmake"
              #:version version
              #:release release
              #:distro distro
              #:hash hash))
    (build-system cmake-build-system)
    (home-page
     "https://github.com/ament/ament_cmake.git")
    (synopsis (string-append "ROS package " nam))
    (description
     "The entry point package for the ament buildsystem in CMake.")
    (license license:asl2.0)))

(define-public ament-cmake
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake"
     #:hash (base32 "0clg5df6k62zwkxb062vjwxk0ggpbibigbgpzfwpabgi9vz9w4pc")))
   (arguments '(#:tests? #f))
   (propagated-inputs
    (list
     ament-cmake-core
     ament-cmake-export-definitions
     ament-cmake-export-dependencies
     ament-cmake-export-include-directories
     ament-cmake-export-interfaces
     ament-cmake-export-libraries
     ament-cmake-export-link-flags
     ament-cmake-export-targets
     ament-cmake-libraries
     ament-cmake-python
     ament-cmake-target-dependencies
     ament-cmake-test
     ament-cmake-version))))

(define-public ament-cmake-core
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-core"
     #:hash (base32 "1nij20icrq17qrmw4j9kdx6i3z7kjh8hynv9m4ga53dp70h5vsym")))
   (arguments '(#:tests? #f))
   (propagated-inputs
    (list
     ament-package
     cmake
     python
     python-catkin-pkg-modules))))

(define-public ament-cmake-black
  (package
    (name "ament-cmake-black")
    (version "0.1.0-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Timple/ament_black-release.git")
               (commit
                 "release/galactic/ament_cmake_black/0.1.0-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0fw4vw0s3prx94pgcy583cpc8qyfbcmvrnyjrcy9fsd0y705v2vz"))))
    (build-system cmake-build-system)
    ;(arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-cmake-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/Timple/ament_black.git")
    (synopsis "ROS package ament_cmake_black")
    (description
      "The CMake API for ament_black to lint Python code using black.")
    (license license:asl2.0)))

(define-public ament-cmake-auto
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-auto"
     #:hash (base32 "0brq3ri9pdgp94sg0b2n53zgi88qwhpx7k98vby9f8vcx03kna74")))
   (propagated-inputs (list ament-cmake))))

(define-public ament-cmake-export-definitions
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-definitions"
     #:hash (base32 "0nrmgchc649w5vkqmxgsazqfrxc2fnrihn23zkmgal854dmmhnfw")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-dependencies
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-dependencies"
     #:hash (base32 "1clh8c3sipgan8xsmip91nk35vgzak7hxl7mhxzdkak9avq0fnzc")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-include-directories
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-include-directories"
     #:hash (base32 "023xjdj9rkdjhwas071iid2gcl5izc7gwkrghx4zq2a7ghd06s9d")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-interfaces
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-interfaces"
     #:hash (base32 "1dqy2r17rpl6ns4w7b2dnhjqd7ygyz8z4j30zyjc4r4l9pg1qhr8")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-libraries
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-libraries"
     #:hash (base32 "1xp4gbfpiadl7n5nrphziwjl714g5fgfniw44rml4vi8q4p0058r")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-link-flags
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-link-flags"
     #:hash (base32 "0b9za030hg6a9jfdv2qbb9phpzz5bv718rs75m901k8yj2djvmv9")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-export-targets
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-export-targets"
     #:hash (base32 "0k2bn2msl5vvb8zrpkdpxbcyvihhy14fx107rwl4pw27i90911w0")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))))

(define-public ament-cmake-gmock
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-gmock"
     #:hash (base32 "15a2rmnbvy5f1icx402z7hmd0py8g7n30gzvkvj82pd5kr956j94")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))
   (description
    "The ability to add Google mock-based tests in the ament buildsystem in CMake.")
   (license license:asl2.0)))

(define-public ament-cmake-google-benchmark
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-google-benchmark"
     #:hash (base32 "1ldh1c69dl8d45zl6ap2pgd5jx4b0r9j1vnd7d3h8bb33ksf9c3b")))
   (arguments '(#:tests? #f))
   (native-inputs
    (list ament-cmake-core
          ament-cmake-export-dependencies
          ament-cmake-python))
   (propagated-inputs
    (list ament-cmake-test benchmark))
   (description
    "The ability to add Google Benchmark tests in the ament buildsystem in CMake.")))

(define-public ament-cmake-gtest
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-gtest"
     #:hash (base32 "0f984sjgnxymwxvpdz52l9hrlrhszyqzxkng0p46x406gxhpgp00")))
   (native-inputs (list ament-cmake-core))
   (description
    "The ability to add gtest-based tests in the ament buildsystem in CMake.")))

(define-public ament-cmake-include-directories
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-include-directories"
     #:hash (base32 "1w7z1w723bpx933qyjak3d8dibwkbbijwvlkag4alk24827ybrmi")))
   (native-inputs (list ament-cmake-core))
   (description
    "The functionality to order include directories according to a chain of prefixes in the ament buildsystem in CMake.")))

(define-public ament-cmake-libraries
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-libraries"
     #:hash (base32 "1f7pwdv0j5fvyn9s9vipg8b1finmfn2h0sz7l1fvvycfbmn14qbb")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))
   (description
    "The functionality to deduplicate libraries in the ament buildsystem in CMake.")))

(define-public ament-cmake-nose
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-nose"
     #:hash (base32 "161sa3xpg6h8msc23wdb4cfi0vlgi4mlx03vdalp20hh6zm6pwqp")))
   (native-inputs (list ament-cmake-core))
   (propagated-inputs (list python-nose))
   (description
      "The ability to add nose-based tests in the ament buildsystem in CMake.")))

(define-public ament-cmake-pytest
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-pytest"
     #:hash (base32 "04ilrhf4vya2nimnjwx8fw9swhj9rc5hdvql3zjhfc2gxvp9mv4i")))
   (native-inputs (list ament-cmake-core))
   (propagated-inputs (list python-pytest))
   (description
    "The ability to run Python tests using pytest in the ament buildsystem in CMake.")))

(define-public ament-cmake-python
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-python"
     #:hash (base32 "1kcdfxgxvpx2sjwdynnhm7j3z602i2d3fyw1fcl7cd6q6nn0rkw1")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))
   (description
    "The ability to use Python in the ament buildsystem in CMake.")
   (license license:asl2.0)))

(define-public ament-cmake-target-dependencies
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-target-dependencies"
     #:hash (base32 "180iil3zlz3ypy9944na3n700gnfgcfh56wxijd6vknccfwc5h1h")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))
   (description
    "The ability to add definitions, include directories and libraries of a package to a target in the ament buildsystem in CMake.")))

(define-public ament-cmake-test
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-test"
     #:hash (base32 "161ifb8rypygsnlq2cj8g4ylksqq2zlmf2s89jf3plyph4ld8nz6")))
   (arguments '(#:tests? #f))
   (propagated-inputs
    (list ament-cmake-core ament-cmake-python))
   (description
    "The ability to add tests in the ament buildsystem in CMake.")
   (license license:asl2.0)))

(define-public ament-cmake-version
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-version"
     #:hash (base32 "1fzk65v9axp6w5wpr7cxqs23dn41bafk9792yqbyn66zfb2f867i")))
   (arguments '(#:tests? #f))
   (native-inputs (list ament-cmake-core))
   (description
    "The ability to override the exported package version in the ament buildsystem.")))

(define-public ament-cmake-catch2
  (package
   (inherit
    (make-ament-cmake-package
     "ament-cmake-catch2"
     #:hash (base32 "1wihmfvidk0bnr32v2w8d2zhv7za73575dra9vs8qmzwms3b5vxl")))
   (native-inputs (list ament-cmake-core))
   (synopsis "ROS package ament_cmake_catch2")
   (description
    "Allows integrating catch2 tests in the ament buildsystem with CMake")))

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
   (build-system cmake-build-system)
   (arguments '(#:tests? #f))
   (propagated-inputs
    (list ament-cmake
          ament-lint-auto
          ament-lint-common))
   (home-page
    "https://github.com/ros2/ament_cmake_ros.git")
   (synopsis "ROS package ament_cmake_ros")
   (description
    "The ROS specific CMake bits in the ament buildsystem.")
   (license license:asl2.0)))

(define-public ament-cmake-clang-format
  (package
    (name "ament-cmake-clang-format")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_clang_format/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1nwgxpafdmqxnv17qdaqjb1d70m5cys7w4pv7q8nqywpzf6xmmd3"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_clang_format")
    (description
      "The CMake API for ament_clang_format to lint C / C++ code using clang format.")
    (license license:asl2.0)))

(define-public ament-cmake-clang-tidy
  (package
    (name "ament-cmake-clang-tidy")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_clang_tidy/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0a1h86h9690ayk0pjjcs83yi4jnbsli1rg1lajbzfg6l5cb2v708"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_clang_tidy")
    (description
      "The CMake API for ament_clang_tidy to lint C / C++ code using clang tidy.")
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
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-copyright
            ament-cmake-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
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
    (build-system cmake-build-system)
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
    (build-system cmake-build-system)
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
    (build-system cmake-build-system)
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
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (native-inputs
      (list ament-cmake-core
            ament-cmake-test
            ament-lint-cmake))
    (inputs (list))
    (propagated-inputs (list))
    (home-page
      "https://github.com/ament/ament_lint.git")
    (synopsis "ROS package ament_cmake_lint_cmake")
    (description
      "The CMake API for ament_lint_cmake to lint CMake code using cmakelint.")
    (license license:asl2.0)))

(define-public ament-cmake-mypy
  (package
    (name "ament-cmake-mypy")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_mypy/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1yrbgxp9m247lyxdrpi2s0ncvf9jll7ghylgbz7dq49j9zwvqsd5"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_mypy")
    (description
      "The CMake API for ament_mypy to perform static type analysis on python code with mypy.")
    (license license:asl2.0)))

(define-public ament-cmake-pclint
  (package
    (name "ament-cmake-pclint")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_pclint/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0l8b1nfg9a2rwqyzdy7s795ag8s4ndqgw8ah2xz0fa3i641h7g54"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_pclint")
    (description
      "The CMake API for ament_pclint to perform static code analysis on C/C++ code using PCLint.")
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
    (build-system cmake-build-system)
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

(define-public ament-cmake-pycodestyle
  (package
    (name "ament-cmake-pycodestyle")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_pycodestyle/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0d2hccvqgpwv8i9xq3bjxxf4j9fii0bz3rg4gmbj0vq3xfvc0k72"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_pycodestyle")
    (description
      "The CMake API for ament_pycodestyle to check code against the style conventions in PEP 8.")
    (license license:asl2.0)))

(define-public ament-cmake-pyflakes
  (package
    (name "ament-cmake-pyflakes")
    (version "0.10.7-1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ros2-gbp/ament_lint-release.git")
               (commit
                 "release/galactic/ament_cmake_pyflakes/0.10.7-1")))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1lz3i23kks37av929f1da2bp825hifkz6c3kjf50g0akvj6lfp6l"))))
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_pyflakes")
    (description
      "The CMake API for ament_pyflakes to check code using pyflakes.")
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
    (build-system cmake-build-system)
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
    (build-system cmake-build-system)
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
    (synopsis "ROS package ament_cmake_xmllint")
    (description
      "The CMake API for ament_xmllint to check XML file using xmmlint.")
    (license license:asl2.0)))
