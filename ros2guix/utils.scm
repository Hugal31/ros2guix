(define-module (ros2guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-26)
  #:export (add-or-replace-definition
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

(define* (add-or-replace-definition
          symbol-name
          new-definition
          #:optional (read-port (current-input-port)) (write-port (current-output-port)))
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
