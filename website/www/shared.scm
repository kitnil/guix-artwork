(define-module (www shared)
  #:use-module (www utils)
  #:export (latest-guix-version
            html-page-header
	    html-page-description
	    html-page-links
	    html-page-footer))

(define latest-guix-version
  (make-parameter "0.8.1"))

(define* (html-page-header title #:key (css "article.css"))
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
		  (href ,(css-url "base.css"))))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href ,(css-url css))))
	 (link (@ (type "image/png")
		  (rel "icon")
		  (href ,(image-url "favicon.png"))))
	 (link (@ (rel "license") (href "Pending...")))
	 (title ,(string-append title " - GuixSD"))))

(define (html-page-description)
  `(div (@ (class "message-box msg-info"))
	(span (@ (class "msg-label")) "Note ")
	"The Guix System Distribution (GuixSD) is alpha software, "
        "which means it is "
        (a (@ (href ,(base-url "manual/html_node/System-Installation.html#Limitations")))
           "not production-ready")
        ".  But you can "
	(a (@ (href ,(base-url "contribute"))) "help") "!"))

(define (html-page-links)
  `(div (@ (id "header-box"))
	(a (@ (id "logo") (href ,(base-url "")))
	   (img (@ (src ,(image-url "GuixSD-logo.png"))
		   (alt "GuixSD"))))
	(ul (@ (id "site-nav"))
	    (li (a (@ (href ,(base-url "download"))) "Download"))
	    (li (a (@ (href ,(guix-url "package-list.html"))) "Packages"))
	    (li (a (@ (href ,(base-url "help"))) "Help"))
	    (li (a (@ (href ,(base-url "contribute"))) "Contribute"))
	    (li (a (@ (href ,(base-url "donate"))) "Donate"))
	    (li (a (@ (href ,(base-url "about"))) "About")))))

(define (html-page-footer)
  `(div (@ (id "footer-box"))
	"copyleft 2015 GuixSD "
	(a (@ (href ,(base-url "contribute")) (class "hlink-yellow"))
	   "Contributors")
	". Made with " (span (@ (class "metta")) "♥") " by humans."))
