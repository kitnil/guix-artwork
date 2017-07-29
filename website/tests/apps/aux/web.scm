;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux web)
  #:use-module (apps aux web)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-web")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] slugify"

 (test-assert
  "Text is lowercase."
  (equal? (slugify "Biology") "biology"))

 (test-assert
  "Separate words with a hyphen."
  (equal? (slugify "Human anatomy") "human-anatomy"))

 (test-assert
  "Remove reserved characters for IRIs."
  (equal? (slugify ":/?#[]@!$&'()*+,;=") ""))

 (test-assert
  "Remove reserved characters for file names."
  (equal? (slugify ":/?*\\%\"|<>") "")))



(test-group
 "[procedure] url-path-join"

 (test-equal
  "Build a relative path to a web resource."
  (url-path-join "blog" "tags" "index.html")
  "blog/tags/index.html")

 (test-equal
  "Build an absolute path to a directory."
  (url-path-join "" "en" "docs" "manual")
  "/en/docs/manual")

 (test-equal
  "Append a slash to the end of the path when specified."
  (url-path-join "" "docs" "manual" "")
  "/docs/manual/")

 (test-equal
  "Build a path to the root directory."
  (url-path-join "")
  "/"))


(test-end SUITE_NAME)
