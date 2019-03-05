;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps download builder)
  #:use-module (apps download templates download)
  #:use-module (apps download data)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:export (builder))


;;;
;;; Application builder.
;;;

(define (builder site posts)
  "Return the list of web resources that compose the app.

   This procedure is a Haunt builder procedure.

   SITE (<site>)
     A site object that defines all the properties of the website. See
     Haunt <site> objects for more information.

   POSTS (list of <post>)
     A list of post objects that represent articles from the blog. See
     Haunt <post> objects for more information.

   RETURN (list of <page>)
     A list of page objects that represent the web resources of the
     application. See Haunt <page> objects for more information."
  (list (download-builder)))



;;;
;;; Helper builders.
;;;

(define (download-builder)
  "Return a Haunt page representing the Download page of the website."
  (let ((context
	 (list
	  (cons "downloads" system-downloads))))
    (make-page "download/index.html" (download-t context) sxml->html)))
