;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps blog templates feed)
  #:use-module (apps aux strings)
  #:use-module (apps aux sxml)
  #:use-module (apps aux web)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps blog utils)
  #:use-module (haunt html)
  #:use-module (haunt post)
  #:use-module (srfi srfi-19)
  #:export (atom-feed-t))


(define (atom-feed-t context)
  "Return an SXML representation of a Blog's topic atom feed."
  (let ((domain (context-datum context "domain"))
	(title (context-datum context "title"))
	(id (context-datum context "id"))
	(alternate (context-datum context "alternate"))
	(posts (context-datum context "posts")))
    `(feed
      ;; Feed info.
      (@ (xmlns "http://www.w3.org/2005/Atom"))
      (id ,id)
      (title ,title)
      (author (name "GNU Guix") (uri ,domain))
      (icon ,(guix-url "static/base/img/icon.png"))
      (updated ,(date->string (current-date) "~4"))
      (link (@ (rel "alternate") (href ,alternate)))

      ;; Feed entries.
      ,@(map
	 (lambda (post)
	   `(entry
	     (id ,(url-path-join domain (post-url-path post) ""))
	     (title ,(post-ref post 'title))
	     (author (name ,(post-ref post 'author)))
	     (published ,(date->string (post-date post) "~4"))
	     (updated ,(date->string (post-date post) "~4"))
	     ;(rights (@ (type "text")) ,(post-copyright post))
	     (link (@ (rel "alternate")
		      (href ,(url-path-join domain
					    (post-url-path post)
					    ""))))
	     ,@(map
		(lambda (tag)
		  `(category (@ (term ,tag))))
		(post-ref post 'tags))
	     (summary ,(string-summarize (sxml->string* (post-sxml post)) 100) "…")
	     (content (@ (type "html")) ,(sxml->html-string (post-sxml post)))))
	 posts))))
