;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates help)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (help-t))


(define (help-t)
  "Return the Help page in SHTML."
  (theme
   #:title '("Help")
   #:description
   "A list of resources about how to use GuixSD and GNU Guix, plus
   information about getting help from the community of users and
   developers."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Help resources")
   #:active-menu-item "Help"
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/item-preview.css"))
   #:crumbs (list (crumb "Help" "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      (h2 "Help")

      (div
       (@ (class "centered-text"))

       (div
	(@ (class "summary-box"))
	(img
	 (@ (src ,(guix-url "static/base/img/guixsd-manual-icon.png"))
	    (alt "")))
	(h3 "GuixSD Manual")
	(p
	 "The documentation about the Guix System Distribution is
         available online as part of the GNU Guix package manager
         manual.")
	(p
	 ,(link-more
	   #:label "Read GuixSD manual"
	   #:url (manual-url "GNU-Distribution.html"))))


       (div
	(@ (class "summary-box"))
	(img
	 (@ (src ,(guix-url "static/base/img/guix-manual-icon.png"))
	    (alt "")))
	(h3 "GNU Guix Manual")
	(p
	 "Documentation for the GNU Guix package manager is available
         online. You may also find more information about Guix by running "
	 (code "info guix") ".")
	(p
	 ,(link-more
	   #:label "Read Guix manual"
	   #:url (guix-url "manual/"))
         ,(link-more
	   #:label "Get Guix reference card"
	   #:url (guix-url "guix-refcard.pdf"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/library-icon.png"))
		(alt "")))
	(h3 "GNU Manuals")
	(p
	 "GuixSD is a distribution of the "
	 (a (@ (href ,(gnu-url))) "GNU operating system")
	 ".  Documentation for GNU packages is
         available online in various formats. ")
	(p
	 ,(link-more
	   #:label "Browse GNU manuals"
	   #:url (gnu-url "manual"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/chat-icon.png"))
		(alt "")))
	(h3 "IRC Chat")
	(p
	 "For real-time support from the community, you can connect
         to the " (code "#guix") " channel on irc.freenode.net. There
         you can get help about anything related to both the Guix
         System Distribution and GNU Guix.")
	(p
	 "The " (code "#guix") " channel is logged. Previous
         conversations can be browsed online. See the "
	 (a (@ (href "https://gnunet.org/bot/log/guix/"))
	    "channel logs")
	 ". ")
	(p
	 ,(link-more
	   #:label "Connect"
	   #:url (guix-url "contact/irc/"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/email-icon.png"))
		(alt "")))
	(h3 "Mailing lists")
	(p
	 "Email support from the community is also available through
         several mailing list. The messages sent to the lists are
         public and archived online.")

	(p
	 ,(link-more
	   #:label "See all lists"
	   #:url (guix-url "contact/")))))))))
