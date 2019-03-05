;;; GNU Guix web site
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

 (test-equal
  "Text is lowercase."
  "biology"
  (slugify "Biology"))

 (test-equal
  "Separate words with a hyphen."
  "human-anatomy"
  (slugify "Human anatomy"))

 (test-equal
  "Remove reserved characters for IRIs."
  ""
  (slugify ":/?#[]@!$&'()*+,;="))

 (test-equal
  "Remove reserved characters for file names."
  ""
  (slugify ":/?*\\%\"|<>")))



(test-group
 "[procedure] url-path-join"

 (test-equal
  "Build a relative path to a web resource."
  "blog/tags/index.html"
  (url-path-join "blog" "tags" "index.html"))

 (test-equal
  "Build an absolute path to a directory."
  "/en/docs/manual"
  (url-path-join "" "en" "docs" "manual"))

 (test-equal
  "Append a slash to the end of the path when specified."
  "/docs/manual/"
  (url-path-join "" "docs" "manual" ""))

 (test-equal
  "Build a path to the root directory."
  "/"
  (url-path-join "")))


(test-end SUITE_NAME)
