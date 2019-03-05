;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux numbers)
  #:use-module (apps aux numbers)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-numbers")


;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)

(test-group
 "[procedure] minus-one"
 (test-equal "0 minus 1" -1 (minus-one 0))
 (test-equal "6 minus 1" 5 (minus-one 6)))


(test-group
 "[procedure] plus-one"
 (test-equal "0 plus one." 1 (plus-one 0))
 (test-equal "5 plus one." 6 (plus-one 5)))


(test-group
 "[procedure] range"
 (test-equal
  "Range with equal start and end."
  (list 8) (range 8 8))
 (test-equal
  "Range of positive integers."
  (list 0 1 2 3 4 5) (range 0 5))
 (test-equal
  "Range of negative integers."
  (list -5 -4 -3 -2 -1 0) (range -5 0))
 (test-equal
  "Range of negative and positive integers."
  (list -5 -4 -3 -2 -1 0 1 2 3 4 5) (range -5 5)))

(test-end SUITE_NAME)
