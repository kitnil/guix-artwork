;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(define-module (apps base builder)
  #:use-module (apps aux system)
  #:use-module (apps base data)
  #:use-module (apps base templates about)
  #:use-module (apps base templates contact)
  #:use-module (apps base templates irc)
  #:use-module (apps base templates contribute)
  #:use-module (apps base templates donate)
  #:use-module (apps base templates graphics)
  #:use-module (apps base templates help)
  #:use-module (apps base templates home)
  #:use-module (apps base templates menu)
  #:use-module (apps base templates screenshot)
  #:use-module (apps base templates security)
  #:use-module (apps base types)
  #:use-module (apps blog utils)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt post)
  #:use-module (haunt utils)
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
   (list (menu-builder)
	 (home-builder site posts)
	 (screenshots-builder)
	 (help-builder)
	 (donate-builder)
	 (about-builder)
	 (contact-builder)
	 (irc-builder)
	 (contribute-builder)
	 (security-builder)
	 (graphics-builder))))



;;;
;;; Helper builders.
;;;

(define (about-builder)
  "Return a Haunt page representing the About page of the website."
  (make-page "about/index.html" (about-t) sxml->html))


(define (contact-builder)
  "Return a Haunt page representing the Contact page of the website."
  (let ((context (list (cons "contact-media" contact-media))))
    (make-page "contact/index.html" (contact-t context) sxml->html)))


(define (irc-builder)
  "Return a Haunt page with an embedded Kiwi IRC widget."
  (make-page "contact/irc/index.html" (irc-t) sxml->html))


(define (contribute-builder)
  "Return a Haunt page representing the Contribute page of the website."
  (make-page "contribute/index.html" (contribute-t) sxml->html))


(define (donate-builder)
  "Return a Haunt page representing the Donate page of the website."
  (make-page "donate/index.html" (donate-t) sxml->html))


(define (graphics-builder)
  "Return a Haunt page representing the Graphics page of the website."
  (make-page "graphics/index.html" (graphics-t) sxml->html))


(define (help-builder)
  "Return a Haunt page representing the Help page of the website."
  (make-page "help/index.html" (help-t) sxml->html))


(define (home-builder site posts)
  "Return a Haunt page representing the Home page of the website."
  (let ((context
	 (list
	  (cons "screenshots" screenshots)
	  (cons "posts" (posts/latest posts 3))
	  (cons "contact-media" (list-head contact-media 3)))))
    (make-page "guix.html" (home-t context) sxml->html)))


(define (menu-builder)
  "Return a Haunt page representing the website menu."
  (make-page "menu/index.html" (menu-t) sxml->html))


(define (screenshots-builder)
  "Return a list of Haunt page representing screenshot pages."
  (map
   (lambda (shot)
     (let ((context
	    (list (cons "screenshot" shot)
		  (cons "screenshots" screenshots))))
       (make-page (path-join "screenshots"
			     (screenshot-slug shot)
			     "index.html")
		  (screenshot-t context)
		  sxml->html)))
   screenshots))


(define (security-builder)
  "Return a Haunt page representing the Security page of the website."
  (make-page "security/index.html" (security-t) sxml->html))
