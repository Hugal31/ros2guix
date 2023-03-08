(define-module (ros2guix modules)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ros2guix utils)
  #:export (add-imports
            add-or-replace-definition
            apply-func-states
            update-definition*
            update-module
            update-module*))

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

      ((keyword value . rest)
       (let-values (((modules others) (extract-define-module-uses rest)))
         (values modules (cons* keyword value others))))

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

(define (symbol->string* s)
  (if (symbol? s)
      (symbol->string s)
      s))

(define (apply-func-states value procs)
  "Given a value and a list of procs returning the same type of value as well as a boolean,
apply each proc to the value and return the new value as well as a new list of proc containing
the proc that returned #f"

  (car
   (fold
    (lambda (proc values-and-procs)
      (let ((value (car values-and-procs))
            (procs (cdr values-and-procs)))
        (receive (new-value applied)
            (proc value)
          (cons new-value (if applied (cons proc procs) procs)))))
    (cons value '())
    procs)))

(define* (update-definition* define-kind
                             name
                             definition)
  "Return a proc taking a sexp and returning a sexp and a proc.
If the sexp is (define-public NAME ...) or (define NAME ...), replace the ...
with definition. If the sexp is #f, return (DEFINE-KIND NAME DEFINITION).
In both case, the second returned value is #t.

If the sexp doesn't match the NAME, the second value is #f."

  (define (pred value)
    (eq? name value))

  ;; TODO Use a monad? We need to store the state of the proc, if it was called or not.
  ;; E.g. return an optional, two values, use a state monad, ...
  (lambda (sexp)
    (match sexp
      (#f (values (list define-kind name definition) #t))
      (('define (? pred) _ ...) (values `(define ,name ,definition) #t))
      (('define-public (? pred) _ ...) (values `(define-public ,name ,definition) #t))
      (_ (values sexp #f)))))

(define (update-module path module imports definitions)
  "Update the file defining the module MODULE in PATH, which is a list of
symbols or string, such as '(guix packages)). Add the IMPORTS and update
the DEFINITIONS in this module."
  (let* ((module (map symbol->string* module))
         (module-path (string-join module "/"))
         (file-path (%search-load-path module-path))
         (output
          (call-with-output-string
            (lambda (write-port)
              (call-with-input-file file-path
                (lambda (read-port)
                  (update-module* read-port write-port module imports definitions)))))))
    (call-with-output-file file-path
      (cut put-string <> output))))

(define (update-module* read-port write-port module imports definitions)
  (pretty-print `(define-module ,module
                   ,@(flatten (map (lambda (imp) `(#:use-module ,imp)) imports))) write-port)

  (for-each
   (lambda (def)
     (newline write-port)
     (pretty-print def write-port))
   definitions))
