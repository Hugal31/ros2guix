(define-module (colcon)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ros catkin))

(define-public python-scspell
  (package
   (name "python-scspell")
   (version "2.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/myint/scspell.git")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1iq0icx2kghrhqvjb55x0f35zcmkh099h7aa75zy92k2lg3jm6dj"))))
   (build-system python-build-system)
   (home-page "https://github.com/myint/scspell")
   (synopsis "Spell checker for source code")
   (description "scspell is a spell checker for source code")
   (license license:gpl2)))

(define-public colcon-core
  (package
   (name "colcon-core")
   (version "0.9.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1qfh30sma8pkkfwb91lb9l22vjabmrfq7xz6mvkm75vs7azyvbr8"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'set-home
          (lambda _ (setenv "HOME" "/tmp")))
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_spell_check and not test_sequential_keyboard_interrupt")))))))
   (native-inputs
    (list
     python-flake8
     python-pydocstyle
     python-scspell))
   (propagated-inputs
    (list
     python-empy
     python-coloredlogs
     python-distlib
     python-pytest
     python-pytest-cov
     python-pytest-rerunfailures
     python-pytest-repeat))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "collective construction")
   (description "Colcon is a command line tool to improve the workflow of building, testing and using multiple software packages.  It automates the process, handles the ordering and sets up the environment to use the packages.")
   (license license:asl2.0)))

(define-public colcon-library-path
  (package
   (name "colcon-library-path")
   (version "0.2.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1hzc7xvzlnhpbjbbz0dxzzqxfdd3ppz6jjwkg1693hlwylbzr5px"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not flake8")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "An extension for colcon-core to set an environment variable to find shared libraries at runtime.")
   (description "An extension for colcon-core to set an environment variable to find shared libraries at runtime.")
   (license license:asl2.0)))

(define-public colcon-cmake
  (package
   (name "colcon-cmake")
   (version "0.2.9")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "15bv58zkc6l8qzx1k48gldkqvyp72j9bs3hvb7vl389rrwk1x9by"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'set-home
          (lambda _ (setenv "HOME" "/tmp")))
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core colcon-library-path))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to support CMake packages.")
   (description "Extension for colcon to support CMake packages.")
   (license license:asl2.0)))

(define-public colcon-pkg-config
  (package
   (name "colcon-pkg-config")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1pxlhabf25b24ls69v7424dalq4qgvazcarvnn95yz21i1km2aq8"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon adding an environment variable to find pkg-config files")
   (description "Extension for colcon adding an environment variable to find pkg-config files")
   (license license:asl2.0)))

(define-public colcon-python-setup-py
  (package
   (name "colcon-python-setup-py")
   (version "0.2.7")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1jysnnpgsdad9q3gc2yz89lvhi0asqxgpzy69vgcldffqq4cbh0s"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "")
   (description "")
   (license license:asl2.0)))

(define-public colcon-recursive-crawl
  (package
   (name "colcon-recursive-crawl")
   (version "0.2.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0an3xq6s5m9hqz777jjwzmp9347qd8xb7b9swg6a5hlgp6hgan68"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to recursively crawl for packages.")
   (description "Extension for colcon to recursively crawl for packages.")
   (license license:asl2.0)))

(define-public colcon-ros
  (package
   (name "colcon-ros")
   (version "0.3.9")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1xb69fn79zcp8p8hrn6kb53npgs2gay7bia4rgd7dzynmrjyvhdi"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python"
               "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     python-catkin-pkg-modules
     colcon-core
     colcon-cmake
     colcon-pkg-config
     colcon-python-setup-py
     colcon-recursive-crawl))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to support ROS packages")
   (description "Extension for colcon to support ROS packages.")
   (license license:asl2.0)))

(define-public colcon-parallel-executor
  (package
   (name "colcon-parallel-executor")
   (version "0.2.4")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "145xdf773acxpwjss18flvkv5x0zgdkd17m7pdjji5r74k5siybi"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to process packages in parallel.")
   (description "Extension for colcon to process packages in parallel.")
   (license license:asl2.0)))

(define-public colcon-argcomplete
  (package
   (name "colcon-argcomplete")
   (version "0.3.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "08pfqfb04nhrkanjlhw5i2b58fxw4dlhlw0m1bqf0ssrwps9ma03"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core
     python-argcomplete))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "")
   (description "")
   (license license:asl2.0)))

(define-public colcon-bash
  (package
   (name "colcon-bash")
   (version "0.4.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "166xh8iyk5zvgdr6sakmck830zx5n71ws45ni0y2h9cli39pmp6q"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to provide Bash scripts")
   (description "Extension for colcon to provide Bash scripts.")
   (license license:asl2.0)))

(define-public colcon-package-information
  (package
   (name "colcon-package-information")
   (version "0.3.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1mvx91qbqhfah08hx06bpk06vbcirdqxr54sgjpwibhifkn6kh7w"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to output package information")
   (description "Extension for colcon to output package information.")
   (license license:asl2.0)))

(define-public colcon-cd
  (package
   (name "colcon-cd")
   (version "0.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "10aqh7l92d9df5afm4g592r1hmzpmynnnnahl7kw0qnmplqqc66f"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke "python" "-m" "pytest" "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core
     colcon-package-information))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "A shell function for colcon to change the current working directory")
   (description "A shell function for colcon to change the current working directory.")
   (license license:asl2.0)))

(define-public colcon-defaults
  (package
   (name "colcon-defaults")
   (version "0.2.6")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "078m6k22k6j5ljka942j77xpm74wrl9hxy1y7j6s8fxvsar9ack1"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core python-pyyaml))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to read defaults from a config file")
   (description "Extension for colcon to read defaults from a config file.")
   (license license:asl2.0)))

(define-public colcon-devtools
  (package
   (name "colcon-devtools")
   (version "0.2.3")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0ri5psplxvi9bvix7270kj0bxzg859i2ah921ynzpxwgzrxxw6na"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to provide information about all extension points and extensions")
   (description "Extension for colcon to provide information about all extension points and extensions.")
   (license license:asl2.0)))

(define-public colcon-metadata
  (package
   (name "colcon-metadata")
   (version "0.2.5")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0ksm560wjr1malblm5871v43r6b2fdfz13p2zjcag38j7fss2b08"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core
     python-pyyaml))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to read package metadata from files.")
   (description "Extension for colcon to read package metadata from files.")
   (license license:asl2.0)))

(define-public colcon-notification
  (package
   (name "colcon-notification")
   (version "0.2.9")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1gdjqq81vaci95q6nf9car1qgprzi0yrv244ya0s6cpwx6nl3xrd"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core
     python-notify2))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to provide status notifications")
   (description "Extension for colcon to provide status notifications.")
   (license license:asl2.0)))

(define-public colcon-output
  (package
   (name "colcon-output")
   (version "0.2.9")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1ssvnd72z9kv6cmcyq2x4pn7bfkbqq0q79nll30p1shyr9wm1ih1"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to customize the output in various ways")
   (description "Extension for colcon to customize the output in various ways.")
   (license license:asl2.0)))

(define-public colcon-package-selection
  (package
   (name "colcon-package-selection")
   (version "0.2.9")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1bappn8impadd9ckiln6wv6q39pa22a91av41kn52razy48jjckr"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to select the packages to process")
   (description "Extension for colcon to select the packages to process.")
   (license license:asl2.0)))

(define-public colcon-test-result
  (package
   (name "colcon-test-result")
   (version "0.3.8")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "040dj959cfqsqzf02g5r3j32gm37n9i47gp1j18cd6vhk4ca7pg2"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to provide information about the test results")
   (description "Extension for colcon to provide information about the test results.")
   (license license:asl2.0)))

(define-public colcon-zsh
  (package
   (name "colcon-zsh")
   (version "0.4.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1qfb3v8mcisiaq55a96wlnlfypwaz9ihj3mjy8p9ypn24z141cf2"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs (list colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to provide Z shell scripts")
   (description "Extension for colcon to provide Z shell scripts.")
   (license license:asl2.0)))

(define-public colcon-installed-package-information
  (package
   (name "colcon-installed-package-information")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "1lgq4b9fmscmjpgq7xd3d7z7iybjg325asxz1viqw62ljkqvk8ah"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extensions for colcon to inspect packages which have already been installed")
   (description "Extensions for colcon to inspect packages which have already been installed.")
   (license license:asl2.0)))

(define-public colcon-override-check
  (package
   (name "colcon-override-check")
   (version "0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url (string-append "https://github.com/colcon/" name ".git"))
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "0g73mczqwzr4aycrdjf5aifr42qxb08dbv4zyvhrgjkm5n64jap7"))))
   (build-system python-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? inputs outputs #:allow-other-keys)
            (when tests?
              (add-installed-pythonpath inputs outputs)
              (invoke
               "python" "-m" "pytest"
               "-k" "not test_flake8 and not test_spell_check")))))))
   (native-inputs
    (list
     python-flake8
     python-flake8-blind-except
     python-flake8-quotes
     python-pep8-naming
     python-pydocstyle
     python-pylint
     python-pytest
     python-pytest-cov
     python-scspell))
   (propagated-inputs
    (list
     colcon-core
     colcon-installed-package-information))
   (home-page "https://colcon.readthedocs.io")
   (synopsis "Extension for colcon to check for potential problems when overriding installed packages")
   (description "Extension for colcon to check for potential problems when overriding installed packages.")
   (license license:asl2.0)))
