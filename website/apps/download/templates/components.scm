;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps download templates components)
  #:use-module (apps download types)
  #:export (download))


;;;
;;; Components.
;;;

(define (download dnd)
  "Return an SHTML representation of the given download object.

   DND (<download>)
     A download object as defined in (apps download types)."
  `(div
    (@ (class "download-box"))
    (img (@ (src ,(download-image dnd)) (alt "")))
    (h3 ,(download-title dnd))
    ,(download-description dnd)
    (p "Download options:")
    ,@(map (lambda (variant)
	     `(a
	       (@ (class "download-btn")
		  (download "")
		  (href ,(string-append
			  (download-base-url dnd)
			  (variant-file variant))))
	       ,(variant-label variant)
	       " ")) ; Force a space for readability in non-CSS browsers.
	  (download-variants dnd))

    (p
     "Signatures: "
     ,@(map (lambda (variant)
	     `(a
	       (@ (class "signature-btn")
		  (download "")
		  (href ,(string-append
			  (download-base-url dnd)
			  (variant-file variant) ".sig")))
	       ,(variant-label variant)
	       " ")) ; Force a space for readability in non-CSS browsers.
	    (download-variants dnd)))

    (p (a (@ (href ,(download-manual dnd))) "Installation instructions") ".")))
