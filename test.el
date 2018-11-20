;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'scheme-datum)

(defun scheme-datum-write-test (expected obj)
  (should (equal expected
                 (with-output-to-string
                   (scheme-datum-write-simple obj)))))

(ert-deftest test-scheme-datum-write ()
  (scheme-datum-write-test "(42 'foo #(\"bar\") . 3.14)"
                           '(42 'foo ["bar"] . 3.14))
  )


(ert-deftest test-scheme-datum-write-special-float ()
  (scheme-datum-write-test "(+inf.0 -inf.0 +nan.0)"
                           (list (/ 1 0.0) (/ -1 0.0) (/ 0 0.0)))
  )

(ert-deftest test-scheme-datum-write-exotic-symbol ()
  (scheme-datum-write-test "|(* 1 2)|"
                           '\(*\ 1\ 2\))
  (scheme-datum-write-test "|+-*/_~!@$%^&=:<>{}|"
                           '+-*/_~!@$%^&=:<>{})
  (scheme-datum-write-test "|\\||" '|)
  (scheme-datum-write-test "||" '##)
  )

(ert-deftest test-scheme-datum-write-t-and-nil ()
  (scheme-datum-write-test "(t ())" (list t nil))
  )

(ert-deftest test-scheme-datum-write-true-false-nil ()
  (scheme-datum-write-test "(#t #f nil)"
   (list scheme-datum-true scheme-datum-false scheme-datum-nil-symbol))
  )
