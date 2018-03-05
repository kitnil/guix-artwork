;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates contact)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (contact-t))


(define (contact-t context)
  "Return the Contact page in SHTML with the data in CONTEXT."
  (theme
   #:title '("Contact")
   #:description
   "A list of channels to communicate with GuixSD and GNU Guix users
   and developers about anything you want."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Community" "Mailing lists" "IRC channels" "Bug reports" "Help")
   #:active-menu-item "About"
   #:css (list
	  (guix-url "static/base/css/page.css")
          (guix-url "static/base/css/buttons.css"))
   #:crumbs (list (crumb "Contact" "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 "Contact")

      ,@(map
	 contact->shtml
	 (context-datum context "contact-media"))))))
