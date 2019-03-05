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
 (test-equal "0 minus 1" (minus-one 0) -1)
 (test-equal "6 minus 1" (minus-one 6) 5))


(test-group
 "[procedure] plus-one"
 (test-equal "0 plus one." (plus-one 0) 1)
 (test-equal "5 plus one." (plus-one 5) 6))


(test-group
 "[procedure] range"
 (test-equal
  "Range with equal start and end."
  (range 8 8) (list 8))
 (test-equal
  "Range of positive integers."
  (range 0 5) (list 0 1 2 3 4 5))
 (test-equal
  "Range of negative integers."
  (range -5 0) (list -5 -4 -3 -2 -1 0))
 (test-equal
  "Range of negative and positive integers."
  (range -5 5) (list -5 -4 -3 -2 -1 0 1 2 3 4 5))
 )


(test-end SUITE_NAME)
