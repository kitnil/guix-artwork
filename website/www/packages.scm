(define-module (www packages)
  #:use-module (www shared)
  #:export (packages-page))

(define packages-page
  `(html (@ (lang "en"))
	 ,(html-page-header "Packages")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Packages")
		(p "The Guix System Distribution provides 1,500+ packages
transparently "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master#tabs-status"))
		      "available as pre-built binaries")
		   ". This is a complete lists of the packages. Our "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
		      "continuous integration system")
		   " shows their current build status.")))
	  ,(html-page-footer))))
