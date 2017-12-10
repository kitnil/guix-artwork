;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps blog builder)
  #:use-module (apps aux system)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps blog templates feed)
  #:use-module (apps blog templates post-list)
  #:use-module (apps blog templates post)
  #:use-module (apps blog templates tag)
  #:use-module (apps blog utils)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt site)
  #:use-module (haunt utils)
  #:use-module (sxml simple)
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
  (flatten
   (list
    (blog-feed-builder site posts)
    (post-list-builder posts)
    (posts-builder posts)
    (tag-feed-builder site posts)
    (tags-builder posts))))



;;;
;;; Helper builders.
;;;

(define (blog-feed-builder site posts)
  "Return a Haunt page representing the atom feed of the blog."
  (let* ((domain (site-domain site))
	 (sorted-posts (posts/reverse-chronological posts))
	 (max-posts 10) ; Number of posts to add to the feed.
	 (context
	  (list
	   (cons "domain" domain)
	   (cons "title" "GuixSD — Blog")
	   (cons "id" (url-path-join domain "feeds" "blog.atom"))
	   (cons "alternate" (url-path-join domain "blog" ""))
	   (cons "posts"
		 (if (> (length sorted-posts) max-posts)
		     (list-head sorted-posts max-posts)
		     sorted-posts)))))
    (make-page (path-join "feeds" "blog.atom")
	       (atom-feed-t context)
	       sxml->xml)))


(define (post-list-builder posts)
  "Return a list of Haunt pages representing paginated POSTS."
  (let ((context
	 (list
	  (cons "tags" (post-groups->tag-list
			(posts/group-by-tag posts))))))
    (paginate #:dataset (posts/reverse-chronological posts)
	      #:base-path "blog"
	      #:template post-list-t
	      #:context context
	      #:writer sxml->html)))


(define (posts-builder posts)
  "Return a list of Haunt pages representing blog posts."
  (map
   (lambda (post)
     (let ((context (list (cons "post" post))))
       (make-page (path-join (post-url-path post) "index.html")
		  (post-t context)
		  sxml->html)))
   posts))


(define (tag-feed-builder site posts)
  "Return a Haunt page representing the atom feed of a blog topic."
  (let ((post-groups (posts/group-by-tag posts)))
    (map
     (lambda (tagged-posts)
       (let* ((domain (site-domain site))
	      (tag-name (car tagged-posts))
	      (tag-slug (slugify tag-name))
	      (file-name (string-append tag-slug ".atom"))
	      (context
	       (list
		(cons "domain" domain)
		(cons "title"
		      (string-append "GuixSD — Blog — " tag-name))
		(cons "id" (url-path-join domain
					  "feeds"
					  "blog"
					  file-name))
		(cons "alternate" (url-path-join domain
						 "blog"
						 "tags"
						 tag-slug
						 ""))
		(cons "posts"
		      (posts/reverse-chronological (cdr tagged-posts))))))
	 (make-page (path-join "feeds" "blog" file-name)
		    (atom-feed-t context)
		    sxml->xml)))
     post-groups)))


(define (tags-builder posts)
  "Return a list of lists of Haunt pages representing POSTS grouped by
   tag.

   Each list of pages corresponds to the paginated blog posts of one
   tag."
  (let ((post-groups (posts/group-by-tag posts)))
    (map
     (lambda (tagged-posts)
       (let ((context
	      (list
	       (cons "tag" (car tagged-posts))
	       (cons "tags" (post-groups->tag-list post-groups)))))
	 (paginate #:dataset (posts/reverse-chronological (cdr tagged-posts))
		   #:base-path (tag-system-path (car tagged-posts))
		   #:template tag-t
		   #:context context
		   #:writer sxml->html)))
     post-groups)))
