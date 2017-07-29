;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages types)
  #:use-module (srfi srfi-9)
  #:export (ilink
	    ilink?
	    ilink-name
	    ilink-url
	    lint-issue
	    lint-issue?
	    lint-issue-type
	    lint-issue-description))


;;;
;;; Data types.
;;;

;;; License (record type)
;;; ---------------------
;;;
;;; A license object represents a copyright license or public domain
;;; dedication.
;;;
;;; Objects of this type can be created with the "license" procedure
;;; as well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; name (string)
;;;   The human readable name of the license. For example: "GPL 2+",
;;;   "CC-BY-SA 3.0", etc.
;;;
;;; uri (string)
;;;   The URL to the definition of the license on the web.
;;;
;;; comment (string)
;;;   A comment about the license?
;;;
(define-record-type <license>
  (make-license name uri comment)
  license?
  (name    license-name)
  (uri     license-uri)
  (comment license-comment))

;;; Helper procedures.

(define* (license #:key name uri (comment ""))
  "Return a <license> object with the given attributes."
  (make-license name uri comment))



;;; ILink (record type)
;;; -------------------
;;;
;;; A link to a web resource.
;;;
;;; Fields:
;;;
;;; name (string)
;;;   A descriptive name for the link. For example:
;;;   "i686 build", "graphics.scm", etc.
;;;
;;; url (string)
;;;   The URL to the web resource.
;;;
(define-record-type <ilink>
  (ilink name url)
  ilink?
  (name    ilink-name)
  (url     ilink-url))



;;; Lint Issue (record type)
;;; ------------------------
;;;
;;; A lint issue object represents an issue reported by any of the lint
;;; checkers available for GNU Guix (see `guix lint --list-checkers`).
;;;
;;; Objects of this type can be created with the "lint-issue" procedure
;;; as well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; type (string)
;;;   The name of the checker the issue belongs to. For example:
;;;   "home-page", "license", "source", etc.
;;;
;;;   See `guix lint --list-checkers` for all the names of the checkers.
;;;
;;; description (string)
;;;   The details of the issue.
;;;
(define-record-type <lint-issue>
  (make-lint-issue type description)
  lint-issue?
  (type lint-issue-type)
  (description lint-issue-description))

;;; Helper procedures.

(define (lint-issue type description)
  "Return a <lint-issue> object with the given attributes."
  (make-lint-issue type description))
