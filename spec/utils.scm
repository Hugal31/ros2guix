(add-to-load-path (dirname (dirname (current-filename))))

(use-modules (ggspec lib)
             (ros2guix utils))

(suite "Test flatten"
       (tests
        (test "Should work on empty list"
              e
              (assert-equal
               '()
               (flatten '())))

        (test "Should work with single item"
              e
              (assert-equal
               '(1 2 3)
               (flatten '((1 2 3)))))

        (test "Should work with 2 items"
              e
              (assert-equal
               '(1 2 3 4 5 6)
               (flatten '((1 2 3 4) (5 6)))))))
