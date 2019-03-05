;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base types)
  #:use-module (srfi srfi-9)
  #:export (contact
	    contact?
	    contact-description
	    contact-log
	    contact-name
	    contact-url
	    context-datum
	    crumb
	    crumb?
	    crumb-label
	    crumb-url
	    screenshot
	    screenshot?
	    screenshot-caption
	    screenshot-image
	    screenshot-preview
	    screenshot-slug
	    screenshot-title))


;;;
;;; Data types.
;;;

;;; Contact (record type)
;;; ---------------------
;;;
;;; A contact object represents a contact medium such as a mailing
;;; list, IRC channel, email address, etc.
;;;
;;; Objects of this type can be created with the "contact"
;;; procedure as well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; name (string)
;;;   The name of the contact medium. For example:
;;;   "Development mailing list".
;;;
;;; description (SXML)
;;;   A short description. For example:
;;;   '(p "Discussion about the development of Guix.").
;;;
;;; url (string)
;;;   A URL to the main page of the contact medium.
;;;
;;; log (string)
;;;   A URL to the archive or log of previous public communications
;;;   help on the contact medium (empty string if there is no log).
;;;
(define-record-type <contact>
  (make-contact name description url log)
  contact?
  (name contact-name)
  (description contact-description)
  (url contact-url)
  (log contact-log))

;;; Helper procedures.

(define* (contact #:key (name "") (description "") (url "") (log ""))
  "Return a <contact> object with the given attributes."
  (make-contact name description url log))



;;; Context (association list)
;;; --------------------------
;;;
;;; A context object is a collection of data to be rendered in the
;;; template of a web resource.
;;;
;;; A context can have any number of custom keys depending on the
;;; requirements of a given template.
;;;
;;; The following is an example of a context object to be used with an
;;; SHTML template:
;;;
(define some-context
  (list
   (cons "LANGUAGE" "es")
   (cons "CHARSET" "UTF-8")
   (cons "AUTHOR" "Jane Roe")
   (cons "FRIENDS" (list "John Doe" "Nyoro N." "Jack the Lad"))))

;;; Helper procedures.

(define (context-datum context key)
  "Return the value of KEY in the given CONTEXT.

   CONTEXT (Context)
     See more information about the Context type in (apps base types).

   KEY (atom)
     Any atomic value allowed for association list keys."
  (assoc-ref context key))



;;; Crumb (record type)
;;; -------------------
;;;
;;; A crumb object represents one of the parts of a breadcrumbs
;;; component of a website.
;;;
;;; Objects of this type can be created with the "crumb" procedure as
;;; well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; label (string)
;;;   A human readable name for the crumb. For example: "Blog".
;;;
;;; url (string)
;;;   The URL to the web resource related to the crumb.
;;;
(define-record-type <crumb>
  (make-crumb label url)
  crumb?
  (label crumb-label)
  (url crumb-url))

;;; Helper procedures.

(define (crumb label url)
  "Return a <crumb> object with the given attributes."
  (make-crumb label url))



;;; Screenshot (record type)
;;; ------------------------
;;;
;;; A screenshot object represents an image of a software view seen
;;; on a screen.
;;;
;;; Objects of this type can be created with the "screenshot"
;;; procedure (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; title (string)
;;;   A title for the screenshot.
;;;
;;; slug (string)
;;;     Slug-like URL name for the screenshot. For example:
;;;     gnome-3-desktop.
;;;
;;; image (string)
;;;   A URL to the full size image of the screenshot.
;;;
;;; preview (string)
;;;   A URL to a small size image of the screenshot.
;;;
;;; caption (string)
;;;   A short text describing the screenshot.
;;;
(define-record-type <screenshot>
  (make-screenshot title slug image preview caption)
  screenshot?
  (title screenshot-title)
  (slug screenshot-slug)
  (image screenshot-image)
  (preview screenshot-preview)
  (caption screenshot-caption))

;;; Helper procedures.

(define* (screenshot #:key title slug image preview caption)
  "Return a <screenshot> object with the given attributes."
  (make-screenshot title slug image preview caption))
