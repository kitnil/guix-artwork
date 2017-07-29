;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux sxml)
  #:use-module (apps aux sxml)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-sxml")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] sxml->string*"

 (test-equal
  "Converting an empty SXML tree results in an empty string."
  (sxml->string* '())
  "")

 (test-equal
  "Convert non-empty SXML tree to string."
  (sxml->string*
   '(p "Hello " (span (@ (class "planet")) "Earth") ". We are writing from " (a (@ (href "https://mars.org/")) "Mars") "."))
  "Hello Earth. We are writing from Mars."))


(test-end SUITE_NAME)
