;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps blog templates post-list)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module ((apps blog templates components) #:prefix blog:)
  #:export (post-list-t))


(define (post-list-t context)
  "Return a list of blog posts in SHTML with the data in CONTEXT."
  (let ((page-number
	 (number->string (context-datum context "page-number")))
	(total-pages
	 (number->string (context-datum context "total-pages"))))
    (theme
     #:title (list (string-append "Page " page-number) "Blog")
     #:description
     "Blog posts about GNU Guix."
     #:keywords
     '("GNU" "Linux" "Unix" "Free software" "Libre software"
       "Operating system" "GNU Hurd" "GNU Guix package manager"
       "GNU Guile" "Guile Scheme" "Transactional upgrades"
       "Functional package management" "Reproducibility")
     #:active-menu-item "Blog"
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/item-preview.css")
	   (guix-url "static/base/css/sidebar.css"))
     #:crumbs
     (list (crumb "Blog" (guix-url "blog/"))
	   (crumb (string-append "Page " page-number)
		  (guix-url (url-path-join "blog"
					   "page"
					   page-number
					   ""))))
     #:content
     `(main
       (section
	(@ (class "page centered-text"))
	(h2 "Blog"
	    ,(page-indicator (string->number page-number)
			     (string->number total-pages)))

	(div
	 (@ (class "sheet"))
	 ,@(map blog:post-preview (context-datum context "items"))
	 ,(page-selector (string->number total-pages)
			 (string->number page-number)
			 (guix-url "blog")))

	,(blog:sidebar (context-datum context "tags")))))))
