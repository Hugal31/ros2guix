(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (guix build utils)
             (ice-9 pretty-print)
             (ros2guix modules)
             (srfi srfi-13)
             (srfi srfi-26))

(define (call-in-temporary-dir proc)
  (let* ((directory (or (getenv "TMPDIR" "/tmp")))
         (template (string-append directory "/tests.XXXXXX"))
         (out (mkdtemp! template))
         (original-dir (getcwd)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (chdir out)
        (proc))
      (lambda ()
        (false-if-exception (chdir original-dir))
        (false-if-exception (delete-file-recursively out))))))

(define (pretty-print-sexps/string sexps)
  (if (not (null? sexps))
      (call-with-output-string
        (lambda (p)
          (pretty-print (car sexps) p)
          (for-each (lambda (sexp)
                      (newline p)
                      (pretty-print sexp p))
                    (cdr sexps))))
      ""))

(suite "Test apply-func-states"
       (tests
        (test "should return input when no proc"
              e
              (assert-equal
               "input"
               (apply-func-states "input" '())))

        (test "should return no functions when they return #t"
              e
              (begin
                (define (uppercase s)
                  (values (string-upcase s) #t))

                (assert-equal
                 "INPUT"
                 (apply-func-states "input" (list uppercase)))))

        (test "should return functions returning #f"
              e
              (begin
                (define (incr-up-to-10 i)
                  (values (+ i 1) (>= i 10)))

                (assert-equal
                 5
                 (apply-func-states 4 (list incr-up-to-10)))))))

(suite "Test update-module"
       (tests
        (test "should create module if doesn't exists"
              e
              (assert-equal
               (pretty-print-sexps/string
                '((define-module (my module)
                    #:use-module (gnu packages)
                    #:use-module (gnu packages emacs))

                  (define-public foo 'bar)))

               (call-with-output-string
                 (cut update-module* (%make-void-port "r") <>
                      '(my module)
                      '((gnu packages) (gnu packages emacs))
                      '((define-public foo 'bar))))))))
