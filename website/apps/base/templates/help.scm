;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates help)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (help-t))


(define (help-t)
  "Return the Help page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Help"))
   #:description
   (G_ "A list of resources about how to use GNU Guix, plus
   information about getting help from the community of users and
   developers.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Help resources") #\|)
   #:active-menu-item (C_ "website menu" "Help")
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/item-preview.css"))
   #:crumbs (list (crumb (C_ "website menu" "Help") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "Help"))

      (div
       (@ (class "centered-text"))


       (div
	(@ (class "summary-box"))
	(img
	 (@ (src ,(guix-url "static/base/img/manual-icon.png"))
	    (alt "")))
        ,(G_ `(h3 "GNU Guix Manual"))
        ,(G_
          `(p
            "Documentation for GNU Guix is available
            online.  You may also find more information about Guix by running "
            ,(G_ `(code "info guix")) "."))
        (p
         ,(link-more #:label (G_ "Read Guix manual")
                     #:url (guix-url "manual/en" #:localize #f)))
        (p
         (a (@ (href ,(guix-url "manual/de" #:localize #f))) "Deutsch") " | "
         (a (@ (href ,(guix-url "manual/en" #:localize #f))) "English") " | "
         (a (@ (href ,(guix-url "manual/es" #:localize #f))) "español") " | "
         (a (@ (href ,(guix-url "manual/fr" #:localize #f))) "français") " | "
         (a (@ (href ,(guix-url "manual/ru" #:localize #f))) "русский")  " | "
         (a (@ (href ,(guix-url "manual/zh-cn" #:localize #f))) "简体中文"))

        ,(link-more
          #:label (G_ "Get Guix reference card")
	  #:url (guix-url "guix-refcard.pdf")))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/library-icon.png"))
		(alt "")))
        ,(G_ `(h3 "GNU Manuals"))
        ,(G_
          `(p
            "Guix is a distribution of the "
            ,(G_ `(a (@ (href ,(gnu-url))) "GNU operating system"))
            ".  Documentation for GNU packages is
            available online in various formats. "))
	(p
	 ,(link-more
           #:label (G_ "Browse GNU manuals")
	   #:url (gnu-url "manual"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/chat-icon.png"))
		(alt "")))
        ,(G_ `(h3 "IRC Chat"))
        ,(G_
          `(p
            "For real-time support from the community, you can connect
            to the " (code "#guix") " channel on irc.freenode.net. There
            you can get help about anything related to GNU Guix."))
        ,(G_
          `(p
            "The " (code "#guix") " channel is logged. Previous
            conversations can be browsed online. See the "
            ,(G_ `(a (@ (href ,guix-irc-log-url)) "channel logs")) ". "))
	(p
	 ,(link-more
           #:label (G_ "Connect")
	   #:url (guix-url "contact/irc/"))))


       (div
	(@ (class "summary-box"))
	(img (@ (src ,(guix-url "static/base/img/email-icon.png"))
		(alt "")))
        ,(G_ `(h3 "Mailing lists"))
        ,(G_
          `(p
            "Email support from the community is also available through
            several mailing list. The messages sent to the lists are
            public and archived online."))

	(p
	 ,(link-more
           #:label (G_ "See all lists")
	   #:url (guix-url "contact/")))))))))
