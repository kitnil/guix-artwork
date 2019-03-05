;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates theme)
  #:use-module (apps base templates components)
  #:use-module (apps base utils)
  #:export (theme))


(define* (theme #:key
		(lang-tag "en")
		(title '())
		(description "")
		(keywords '())
		(active-menu-item "About")
		(css '())
		(scripts '())
		(crumbs '())
		(content '(div "")))
  "Return an SHTML document using the website's theme.

   LANG-TAG (string)
     IETF language tag. This is used to specify the language of the
     document. For example: en, en-CA. If not provided, the value
     defaults to English (en).

   TITLE (list)
     A list of strings to form the value of the title element of the
     document. The elements of the list are joined together with em
     dashes as separators between them. For example, a list with two
     strings like 'Hello', and 'Blog' will result in a title like
     'Hello — Blog — Guix'.

   DESCRIPTION (string)
     The description of the document. This is the value used for the
     description meta element.

   KEYWORDS (list)
     A list of keyword strings that will be used as the value for
     the keywords meta element of the document.

   ACTIVE-MENU-ITEM (string)
     The label of the menu item in the navigation bar that should be
     highlighted to indicate the current section of the website that
     is being browsed. If not provided, the value defaults to 'About'.

   CSS (list)
     A list of strings that represent absolute URL paths to additional
     style sheets. For example: '/static/app/css/style.css'. If not
     provided, the value defaults to an empty list.

   SCRIPTS (list)
     A list of strings that represent absolute URL paths to additional
     script files. For example: '/static/app/js/builds.js'. If not
     provided, the value defaults to an empty list.

   CRUMBS (list)
     A list of <crumb> objects as defined in (apps base types). This
     objects are used to form the breadcrumbs of the website.

   CONTENT (SHTML)
     A main element with the content of the page. For example:
     '(main (h2 'Hello World!') (p 'Once upon a time...'))."
  `((doctype "html")

    (html
     (@ (lang "en"))

     (head
      ,(if (null? title)
	   `(title "GNU Guix")
	   `(title ,(string-join (append title '("GNU Guix")) " — ")))
      (meta (@ (charset "UTF-8")))
      (meta (@ (name "keywords") (content ,(string-join keywords ", "))))
      (meta (@ (name "description") (content ,description)))
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0")))
      ;; Menu prefetch.
      (link (@ (rel "prefetch") (href ,(guix-url "menu/index.html"))))
      ;; Base CSS.
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/elements.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/common.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/messages.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/navbar.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/breadcrumbs.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/buttons.css"))))
      (link (@ (rel "stylesheet") (href ,(guix-url "static/base/css/footer.css"))))
      ;; Additional CSS.
      ,@(map (lambda (style-sheet)
	       `(link (@ (rel "stylesheet") (href ,style-sheet))))
	     css)
      ;; Feeds.
      (link (@ (type "application/atom+xml") (rel "alternate")
	       (title "GNU Guix — Activity Feed")
	       (href ,(guix-url "feeds/blog.atom"))))
      (link (@ (rel "icon") (type "image/png")
	       (href ,(guix-url "static/base/img/icon.png"))))
      (link (@ (rel "icon") (type "image/svg+xml") (sizes "any")
	       (href ,(guix-url "static/base/img/icon.svg"))))
      ;; Additional scripts.
      ,@(map (lambda (script)
	       `(script (@ (src ,script)) ""))
	     scripts))

     (body
      ,(navbar #:active-item active-menu-item)

      ,(if (null? crumbs) "" (breadcrumbs crumbs))

      ,content
      (footer
       "Made with " (span (@ (class "metta")) "♥")
       " by humans and powered by "
       (a (@ (class "link-yellow") (href ,(gnu-url "software/guile/")))
	  "GNU Guile") ".  "
	  (a
	   (@ (class "link-yellow")
	      (href "//git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website"))
	   "Source code")
	  " under the "
	  (a
	   (@ (class "link-yellow")
	      (href ,(gnu-url "licenses/agpl-3.0.html")))
	   "GNU AGPL") ".")))))
