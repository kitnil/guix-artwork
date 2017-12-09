;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates components)
  #:use-module (apps aux lists)
  #:use-module (apps aux strings)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base utils)
  #:use-module (apps packages data)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gnu-maintenance)
  #:use-module (srfi srfi-1)
  #:export (detailed-package-preview
	    issue-count->shtml
	    lint-issue->shtml
	    package-preview
	    sidebar))


;;;
;;; Components.
;;;

(define (detailed-package-preview package)
  "Return an SHTML div element representing the given PACKAGE object.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference."
  `(div
    (@ (class "package-preview"))
    (h3
     (@ (class "package-name"))
     ,(package-name package) " " ,(package-version package) " "
     ,(if (package-issues? package) '(span (@ (class "red-tag")) "") " ")
     (span
      (@ (class "package-synopsis"))
      " — "
      ,(package-synopsis-shtml package)))

    (div
     (@ (class "package-description"))
     ,(if (gnu-package? package)
          '(p (i "This is a GNU package.  "))
          "")
     ,(package-description-shtml package))

    (ul
     (@ (class "package-info"))
     (li (b "License:") " "
	 ,(license->shtml (package-license package))
	 ".")

     (li (b "Website:") " "
	 ,(link-subtle #:label (package-home-page package)
		       #:url (package-home-page package)) ".")

     (li (b "Package source:") " "
	 ,(let* ((l (package-location package))
		 (ilink (location->ilink l)))
	    (link-subtle #:label (ilink-name ilink)
			 #:url (ilink-url ilink)))
	 ".")

     (li (b "Patches:") " "
	 ,(patches->shtml (package-patches package))
	 ".")

     (li (b "Lint issues:") " "
     	 ,(if (null? (package-lint-issues package))
     	      "No"
     	      (link-subtle #:label "Yes"
     	 		   #:url (guix-url "packages/issues/")))
     	 ".")

     (li (b "Builds:") " " ,(supported-systems->shtml package) ".")
     "\n")))


(define (issue-count->shtml count)
  "Return an SHTML representation of COUNT in the form 'X issue(s)'.

   COUNT (natural)
     A natural number.

   RETURN (shtml)
     A span element if the count is 0. A mark element otherwise."
  `(,(if (> count 0) 'mark 'span)
    ,(number->string count)
    ,(if (= count 1) " issue" " issues")))


(define (license->shtml license)
  "Return an SHTML representation of the LICENSE.

   LICENSE (itemization)
     One of two types of object:
     — A <license> object as defined in the (apps packages types)
       module.
     — A list of <license> objects.

   RETURN (shtml)
     One or more links to the licenses."
  (cond ((license? license)
	 (link-subtle #:label (license-name license)
		      #:url (license-uri license)))
	(else
	 (separate
	  (map (lambda (l) ; a license object.
		 (link-subtle #:label (license-name l)
			      #:url (license-uri l)))
	       license)
	  ", "))))


(define (lint-issue->shtml issue)
  "Return an SHTML div element representing the given ISSUE object.

   ISSUE (<lint-issue>)
     A lint issue object as defined in the (apps packages types) module."
  `(div
    (@ (class "lint-issue"))
    (p (@ (class "lint-issue-type")) ,(lint-issue-type issue) ":")
    (pre ,(lint-issue-description issue))))


(define (package-preview package)
  "Return an SHTML a element representing the given PACKAGE object.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference."
  `(a
    (@ (class "item-preview")
       (href ,(guix-url (url-path-join (package-url-path package) ""))))
    (h3 ,(package-name package) " " ,(package-version package))
    (p
     (@ (class "item-summary"))
     ,(string-summarize (package-description package) 30)
     "…")))


(define (patches->shtml patches)
  "Return an SHTML representation of PATCHES.

   PATCHES (list)
     A list of <link> objects as defined in (apps packages types)
     module.

   RETURN (shtml)
     If the list of patches is empty, return the string 'None'.
     Otherwise, return a list of links to patches."
  (if (null? patches)
      "None"
      (separate
       (map (lambda (patch)
	      (link-subtle #:label (ilink-name patch)
			   #:url (ilink-url patch)))
	    patches)
       ", ")))


(define* (sidebar #:optional (active-letter ""))
  "Return an SHTML section element representing the sidebar of the
   package list.

   ACTIVE-LETTER (string)
     The letter in which the current packages are listed."
  `(section
    (@ (class "side-bar"))
    (h3 (@ (class "a11y-offset")) "Packages menu: ")

    (h4 (@ (class "bar-title bar-title-top")) "Browse alphabetically")
    (div
     (@ (class "bar-box-padded"))
     ,@(map
	(lambda (letter)
	  (list
	   (button-little
	    #:label letter
	    #:url (guix-url (url-path-join "packages" letter ""))
	    #:active (string=? letter active-letter))
	   " ")) ; NOTE: Force space for readability in non-CSS browsers.
	alphabet))

    ;; FIXME: This is currently too costly to produce so we just disable it.

    ;; (h4 (@ (class "bar-title")) "Packages Issues")
    ;; (ul
    ;;  (@ (class "bar-list"))
    ;;  (li (@ (class "bar-item"))
    ;;      (a (@ (class "bar-link")
    ;;            (href ,(guix-url "packages/issues/lint/"))) "Lint"))
    ;;  (li (@ (class "bar-item"))
    ;;      (a (@ (class "bar-link")
    ;;            (href ,(guix-url "packages/issues/reproducibility/")))
    ;;         "Reproducibility")))
    ))


(define (supported-systems->shtml package)
  "Return a list of SHTML a links to SYSTEMS builds.

   SYSTEMS (<package>)
     A package object as defined in Guix API.

   RETURN (shtml)
     If the list of supported systems of the package is empty, return
     the string 'None'. Otherwise, return a list of links to systems
     builds in hydra."
  (let ((build-url "https://hydra.gnu.org/job/gnu/master/")
	(package-id (string-append (package-name package)
				   "-"
				   (package-version package)))
	(systems (lset-intersection
                  string=?
                  %hydra-supported-systems
                  (package-transitive-supported-systems package))))
    (if (null? systems)
	"None"
	(separate
	 (map (lambda (system)
		(link-subtle #:label system
			     #:url (string-append build-url
						  package-id
						  "."
						  system)))
	      systems)
	 ", "))))
