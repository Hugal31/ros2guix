(define-module (ros2guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (add-imports
            add-or-replace-definition
            delete-duplicates-sorted
            flatten
            remove-suffix/read-only))

(define* (delete-duplicates-sorted/reversed list #:optional (= equal?) (acc '()))
  (match list
    ((h) (cons h acc))
    ((h . t) (delete-duplicates-sorted/reversed
              t
              =
              (if (= h (car t)) acc (cons h acc))))
    (() acc)))

(define* (delete-duplicates-sorted list #:optional (= equal?))
  "delete-duplicates! for sorted lists"

  (reverse (delete-duplicates-sorted/reversed list =)))

(define (flatten x)
  "Flatten list for one level"

  (reduce append '() x))

(define (remove-suffix/read-only str suffix)
  "Remove suffix if it is at the end of str"
  (if (string-suffix? suffix str)
      (substring/read-only str 0 (- (string-length str) (string-length suffix)))
      str))

(define (copy-sexps in-port out-port)
  (let loop ((sexp (read in-port)))
    (unless (eof-object? sexp)
      (pretty-print sexp out-port)
      (newline)
      (loop (read in-port)))))

(define* (add-or-replace-definition
          symbol-name
          new-definition
          #:optional
          (read-port (current-input-port))
          (write-port (current-output-port)))
  "Replace a SEXP definition matchin either (define ...) or (define-public)
  in the current port. Add it at the end if not found."

  (define (pred value)
    (eq? symbol-name value))

  (let loop ((sexp (read read-port)))
    (unless (eof-object? sexp)
      (pretty-print
       (match sexp
         (('define-public (? pred) _ ...) `(define-public ,symbol-name ,new-definition))
         (('define (? pred) _ ...) `(define ,symbol-name ,new-definition))
         (_ sexp))
       write-port)
      (newline)
      (loop (read read-port)))))

(define (sexp->string s)
  (with-output-to-string (lambda () (write s))))

(define (sexp<? lhs rhs)
  (string<? (sexp->string lhs) (sexp->string rhs)))

(define* (add-imports
          imports
          #:optional
          (read-port (current-input-port))
          (write-port (current-output-port)))
  "Add the given import in the module read by read-port if it doesn't exists"

  (define (add-to-module-list a b)
    (delete-duplicates-sorted (merge (sort a sexp<?) (sort b sexp<?) sexp<?) equal?))

  (define (extract-define-module-uses definitions)
    "Given a list of define-module arguments, return a pair of values,
     the first one being the list of imports, the second one being the remaining arguments"
    (match definitions
      (('#:use-module module . rest)

       (let-values (((modules others) (extract-define-module-uses rest)))
         (values (cons module modules) others)))

      (('#:export exports . rest)
       (let-values (((modules others) (extract-define-module-uses rest)))
         (values modules (append `(#:export ,exports) others))))

      ('() (values '() '()))))

  (let loop ((sexp (read read-port)))
    (match sexp
      ((? eof-object?) #f)

      (('define-module module . definitions)
       (let*-values (((modules other) (extract-define-module-uses definitions))
                     ((all-modules) (add-to-module-list imports modules))
                     ((all-modules-prefixed) (flatten (map (lambda (m) `(#:use-module ,m)) all-modules))))
         (pretty-print `(define-module ,module ,@all-modules-prefixed ,@other)))
       (copy-sexps read-port write-port)
       #t)

      (('use-modules . definitions)
       (pretty-print `(use-modules ,@(add-to-module-list definitions imports)))
       (copy-sexps read-port write-port)
       #t)

      (_ (loop (read read-port))))))
