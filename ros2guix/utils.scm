(define-module (ros2guix utils)
  #:use-module (ice-9 match)
  #:export (delete-duplicates-sorted
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
