(define-module (www shared)
  #:export (html-page-header
	    html-page-description
	    html-page-links
	    html-page-footer))

(define (html-page-header title)
  `(head (meta (@ (charset "utf-8")))
	 (meta (@ (name "author")
		  (content "GuixSD Contributors")))
	 (meta (@ (name "description")
		  (content
		   "GuixSD is GNU's advanced system distribution. GNU is an
operating system which respects the freedom of computer users. You are free to
run the system for any purpose, study how it works, improve it, and share it
with the whole world.")))
	 (meta (@ (name "keywords")
		  (content
		   "GNU, FSF, Free Software Foundation, Linux, Emacs, GCC,
Unix, Free Software, Libre Software, Operating System, GNU Kernel, GNU Hurd,
GUix Package Manager, Guile Scheme, Functional package management")))
	 (meta (@ (name "viewport")
		  (content "width=device-width, initial-scale=1.0")))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href "/software/guix/static/base/css/base.css")))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href "/software/guix/static/base/css/index.css")))
	 (link (@ (type "image/png")
		  (rel "icon")
		  (href "/software/guix/static/base/img/favicon.png")))
	 (link (@ (rel "license") (href "Pending...")))
	 (title ,(string-append title " - GuixSD"))))

(define (html-page-description)
  `(div (@ (class "message-box msg-info"))
	(span (@ (class "msg-label")) "Note ")
	"The Guix System Distribution (GuixSD) is alpha software. This means it
is not production-ready. It may contain bugs and lack important features. But
more than a disclaimer, this is an invitation to join us in improving it. See "
	(a (@ (href "/software/guix/contribute/")) "Contributing")
	", for more information. We hope you can soon switch to GuixSD without
fear. "))

(define (html-page-links)
  `(div (@ (id "header-box"))
	(a (@ (id "logo") (href "/software/guix/"))
	   (img (@ (src "/software/guix/static/base/img/GuixSD-logo.png")
		   (alt "GuixSD"))))
	(ul (@ (id "site-nav"))
	    (li (a (@ (href "/software/guix/download/")) "Download"))
	    (li (a (@ (href "/software/guix/package-list.html")) "Packages"))
	    (li (a (@ (href "/software/guix/help/")) "Help"))
	    (li (a (@ (href "/software/guix/contribute/")) "Contribute"))
	    (li (a (@ (href "/software/guix/donate/")) "Donate"))
	    (li (a (@ (href "/software/guix/about/")) "About")))))

(define (html-page-footer)
  `(div (@ (id "footer-box"))
	"copyleft 2015 GuixSD "
	(a (@ (href "/software/guix/contribute/") (class "hlink-yellow"))
	   "Contributors")
	". Made with " (span (@ (class "metta")) "♥") " by humans."))
