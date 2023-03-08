(define-module (ros2guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (delete-duplicates-sorted
            flatten
            remove-suffix/read-only))

;; From GNU Guile manual, SRFI-1.
(define* (delete-duplicates-sorted lst #:optional (= equal?))
  "delete-duplicates! for sorted lists"

  (fold-right (lambda (elem ret)
                (if (= elem (first ret))
                    ret
                    (cons elem ret)))
              (list (last lst))
              lst))

(define (flatten x)
  "Flatten list for one level"

  (fold-right (lambda (elem ret)
                (append elem ret))
              (list)
              x))

(define (remove-suffix/read-only str suffix)
  "Remove suffix if it is at the end of str"
  (if (string-suffix? suffix str)
      (substring/read-only str 0 (- (string-length str) (string-length suffix)))
      str))
