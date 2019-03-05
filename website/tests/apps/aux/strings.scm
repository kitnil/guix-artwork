;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux strings)
  #:use-module (apps aux strings)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-strings")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] string-summarize"

 (test-equal
  "Return an empty string if there are no words."
  ""
  (string-summarize "" 10))

 (test-equal
  "Return the orginal string when there are less words than the required number."
  "GNU Guix will be present at FOSDEM next month with talks on a number of areas of active development."
  (string-summarize
   "GNU Guix will be present at FOSDEM next month with talks on a number of areas of active development."
   40))

 (test-equal
  "Return an extract with the required number of words."
  "Last week we were celebrating the release of GNU Guile 2.2.0, the Scheme implementation that powers Guix. This is a major milestone and Guile developers naturally wanted to make it"
  (string-summarize
   "Last week we were celebrating the release of GNU Guile 2.2.0, the Scheme implementation that powers Guix. This is a major milestone and Guile developers naturally wanted to make it easy for users to discover all the goodies of 2.2.0 as soon as possible. One of the major roadblocks to that, as for any non-trivial piece of software, is deployment: because your distro is unlikely to have Guile 2.2.0 packaged on Day 1, you have to build it by yourself, which means getting the right dependencies installed and then building Guile itself. That’s not difficult for a developer, but it’s certainly cumbersome."
   30)))


(test-end SUITE_NAME)
