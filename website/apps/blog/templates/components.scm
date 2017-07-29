;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps blog templates components)
  #:use-module (apps aux strings)
  #:use-module (apps aux sxml)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps blog utils)
  #:use-module (haunt post)
  #:use-module (srfi srfi-19)
  #:export (post-preview
	    sidebar))


;;;
;;; Components.
;;;

(define (post-preview post)
  "Return an SHTML representation of the given post object.

   POST (<post>)
     A post object (see Haunt's manual for more information)."
  `(a
    (@ (class "item-preview")
       (href ,(guix-url (url-path-join (post-url-path post) ""))))
    (h3 ,(post-ref post 'title))
    (p
     (@ (class "item-date"))
     ,(date->string (post-date post) "~B ~e, ~Y"))
    (p
     (@ (class "item-summary"))
     ,(string-summarize (sxml->string* (post-sxml post)) 30)
     "…")))


(define* (sidebar tags #:optional (current-tag #false))
  "Return an SHTML section element representing the sidebar of the blog.

   TAGS (association list)
     An association list of tags mapped to blog posts as returned by
     Haunt's 'posts/group-by-tag' procedure in (haunt post) module."
  `(section
    (@ (class "side-bar"))
    (h3 (@ (class "a11y-offset")) "Blog menu: ")

    (h4
     (@ (class "bar-title bar-title-top"))
     ,(if current-tag
	  "Get topic updates"
	  "Get blog updates"))
    (ul
     (@ (class "bar-list"))
     (li (@ (class "bar-item"))
	 (a (@ (class "bar-link feed-link")
	       ,(if current-tag
		    `(href ,(guix-url
			     (url-path-join "feeds" "blog"
					    (string-append
					     (slugify current-tag)
					     ".atom"))))
		    `(href ,(guix-url (url-path-join "feeds" "blog.atom")))))
	    " Atom feed")))

    (h4 (@ (class "bar-title")) "Posts by topic")
    (ul
     (@ (class "bar-list"))
     ,@(map
	(lambda (tag)
	  `(li (@ (class "bar-item"))
	       (a (@ (class "bar-link")
		     (href ,(guix-url (url-path-join (tag-url-path tag) ""))))
		  ,tag)))
	(sort tags tag-first?)))))
