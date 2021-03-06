;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps base types)
  #:use-module (apps base types)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-base-types")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] context-datum"

 (test-equal
  "Return the appropriate value for the given key in the context."
  "lemon"
  (context-datum '(("HEALTH" . 82) ("COOKIE" . "lemon")) "COOKIE")))


(test-end SUITE_NAME)
