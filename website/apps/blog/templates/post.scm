;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps blog templates post)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps blog utils)
  #:use-module ((apps blog templates components) #:prefix blog:)
  #:use-module (apps i18n)
  #:use-module (haunt post)
  #:use-module (srfi srfi-19)
  #:export (post-t))


(define (post-t context)
  "Return a page in SHTML for the post in the given CONTEXT."
  (let* ((post (context-datum context "post"))
	 (tags (post-ref post 'tags)))
    (theme
     #:title (list (post-ref post 'title)
		   (date->string (post-date post) "~Y")
                   (C_ "webpage title" "Blog"))
     #:description
     (G_ "Blog posts about GNU Guix.")
     #:keywords tags
     #:active-menu-item (C_ "website menu" "Blog")
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/code.css")
	   (guix-url "static/blog/css/post.css"))
     #:crumbs
     (list (crumb (C_ "website menu" "Blog") (guix-url "blog/"))
	   (crumb (post-ref post 'title)
		  (guix-url (post-url-path post))))
     #:content
     `(main
       (article
	(@ (class "page centered-block limit-width"))
	(h2 ,(post-ref post 'title))
	(p
	 (@ (class "post-metadata centered-text"))
	 ,(post-ref post 'author) " — "
         ,(date->string (post-date post) (C_ "SRFI-19 date->string format"
                                             "~B ~e, ~Y")))

	,(syntax-highlight (post-sxml post))

	(div
	 (@ (class "tag-list"))
         ,(G_ `(p "Related topics:"))

	 ,@(map
	    (lambda (tag)
	      (list
	       (button-little
		#:label tag
		#:url (guix-url (tag-url-path tag)))
	       " ")) ; NOTE: Force space for readability in non-CSS browsers.
	    (sort tags tag-first?))))))))
