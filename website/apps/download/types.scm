;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps download types)
  #:use-module (srfi srfi-9)
  #:export (download
	    download?
	    download-base-url
	    download-description
	    download-image
	    download-manual
	    download-title
	    download-variants
	    variant
	    variant?
	    variant-label
	    variant-file
	    make-download
	    make-variant))

;;;
;;; Data types.
;;;


;;; Download (record type)
;;; ----------------------
;;;
;;; A download object represents an item that can be downloaded in one
;;; or several flavors.
;;;
;;; Objects of this type can be created with the "download"
;;; procedure as well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; title (string)
;;;   The human readable name of the download. For example:
;;;   "Guix Installer".
;;;
;;; description (SXML)
;;;   A description of the download. For example:
;;;   '(p "Graphical installer of the Guix System Distribution.")
;;;
;;; image (string)
;;;   A URL to an illustrative image for the download.
;;;
;;; base-url (string)
;;;   The base URL where all the variants of the download can be
;;;   found. For example:
;;;   "https://alpha.gnu.org/gnu/guix/guixsd-usb-install-0.12.0"
;;;
;;; variants (list)
;;;   A list of <variant> objects that represent the different flavors
;;;   available for download.
;;;
;;; manual (string)
;;;   A URL to the instructions for the download.
;;;
(define-record-type <download>
  (make-download title description image base-url variants manual)
  download?
  (title download-title)
  (description download-description)
  (image download-image)
  (base-url download-base-url)
  (variants download-variants)
  (manual download-manual))

;;; Helper procedures.

(define* (download #:key title description image base-url variants manual)
  "Return a <download> object with the given attributes."
  (make-download title description image base-url variants manual))



;;; Variant (record type)
;;; ---------------------
;;;
;;; A variant object represents a specific file that can be downloaded.
;;;
;;; Objects of this type can be created with the "variant" procedure
;;; as well (see Helper procedures below).
;;;
;;; Fields:
;;;
;;; label (string)
;;;   A human readable name for the variant. For example: "Manual (PDF)".
;;;
;;; file (string)
;;;   The file name. For example: "manual.pdf".
;;;
(define-record-type <variant>
  (make-variant label file)
  variant?
  (label variant-label)
  (file variant-file))

;;; Helper procedures.

(define (variant label file)
  "Return a <variant> object with the given attributes."
  (make-variant label file))
