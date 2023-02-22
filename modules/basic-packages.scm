(define-module (basic-packages)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc))

;; Some packages compiled with -fPIC because it is the rule on Ubuntu,
;; and some ROS packages (namely osrf-testing-tools-cpp) need them to build shared libraries.

(define-public binutils-pic
  (package
    (inherit binutils)
    (name "binutils")
    (arguments
    `(#:out-of-source? #t   ;recommended in the README
      #:configure-flags '("CFLAGS=-fPIC -g -O2"

                          ;; Add `-static-libgcc' to not retain a dependency
                          ;; on GCC when bootstrapping.
                          "LDFLAGS=-static-libgcc"

                          ;; Turn on --enable-new-dtags by default to make the
                          ;; linker set RUNPATH instead of RPATH on binaries.
                          ;; This is important because RUNPATH can be overriden
                          ;; using LD_LIBRARY_PATH at runtime.
                          "--enable-new-dtags"

                          ;; Don't search under /usr/lib & co.
                          "--with-lib-path=/no-ld-lib-path"

                          ;; Install BFD.  It ends up in a hidden directory,
                          ;; but it's here.
                          "--enable-install-libbfd"

                          ;; Make sure 'ar' and 'ranlib' produce archives in a
                          ;; deterministic fashion.
                          "--enable-deterministic-archives"

                          "--enable-64-bit-bfd"
                          "--enable-compressed-debug-sections=all"
                          "--enable-lto"
                          "--enable-separate-code"
                          "--enable-threads")))))

(define-public libiberty-pic
  (package
    (inherit libiberty)
    (name "libiberty")
    (arguments
     `(#:out-of-source? #t
       #:make-flags '("CFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "libiberty")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (lib     (string-append out "/lib/"))
                    (include (string-append out "/include/")))
               (install-file "libiberty.a" lib)
               (install-file "../include/libiberty.h" include)))))))))
