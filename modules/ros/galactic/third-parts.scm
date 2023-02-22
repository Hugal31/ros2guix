(define-module (ros galactic third-parts)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public foonathan-memory
  (package
    (name "foonathan-memory")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/foonathan/memory.git")
             (commit "v0.7-1")))
       (patches (search-patches "patches/foonathan-doctest.patch"))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l1d2y7yj9i8hx6qhzfhki43fix5a451mknldxxbvrcbfa0s4x02"))))
    (build-system cmake-build-system)
    (native-inputs (list doctest))
    (home-page "https://github.com/foonathan/memory")
    (synopsis "C++ memory allocator library")
    (description "STL compatible C++ memory allocator library using a new RawAllocator concept that is similar to an Allocator but easier to use and write.")
    (license license:zlib)))
