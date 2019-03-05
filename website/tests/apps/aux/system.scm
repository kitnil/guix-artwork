;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux system)
  #:use-module (apps aux system)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-system")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] path-join"

 (test-equal
  "Build a relative path to a file."
  (string-join (list "docs" "essays" "humanity.odt")
	       file-name-separator-string)
  (path-join "docs" "essays" "humanity.odt"))

 (test-equal
  "Build an absolute path to a directory."
  (string-join (list "" "en" "docs" "manual")
	       file-name-separator-string)
  (path-join "" "en" "docs" "manual"))

 (test-equal
  "Append a slash to the end of the path when specified."
  (string-join (list "" "docs" "manual" "")
	       file-name-separator-string)
  (path-join "" "docs" "manual" ""))

 (test-equal
  "Build path to the root directory."
  file-name-separator-string
  (path-join "")))


(test-end SUITE_NAME)
