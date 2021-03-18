(defpackage xra/tests/main
  (:use :cl
        :xra
        :rove))
(in-package :xra/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :xra)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
