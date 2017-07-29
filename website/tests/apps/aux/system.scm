;;; GuixSD website --- GNU's advanced distro website
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
  (path-join "docs" "essays" "humanity.odt")
  (string-join (list "docs" "essays" "humanity.odt")
	       file-name-separator-string))

 (test-equal
  "Build an absolute path to a directory."
  (path-join "" "en" "docs" "manual")
  (string-join (list "" "en" "docs" "manual")
	       file-name-separator-string))

 (test-equal
  "Append a slash to the end of the path when specified."
  (path-join "" "docs" "manual" "")
  (string-join (list "" "docs" "manual" "")
	       file-name-separator-string))

 (test-equal
  "Build path to the root directory."
  (path-join "")
  file-name-separator-string))


(test-end SUITE_NAME)
