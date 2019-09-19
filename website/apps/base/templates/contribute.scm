;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates contribute)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (contribute-t))


(define (contribute-t)
  "Return the Contribute page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Contribute"))
   #:description
   (G_ "Check all the ways you can contribute to make GNU Guix
   better, and join the world-wide community of volunteers.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Volunteer|Development|\
Translation|I18N|L10N|Artwork") #\|)
   #:active-menu-item (C_ "website menu" "About")
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/item-preview.css"))
   #:crumbs (list (crumb (C_ "website menu" "Contribute") "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      ,(G_ `(h2 "Contribute"))

      ,(G_
        `(p
          "GNU Guix is a large project developed
           mostly by volunteers from all around the world. You are welcome
           to join us in the "
          ,(G_
            `(a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
                "development mailing list"))
          " or in the "
          ,(G_
            `(a (@ (href ,(guix-url "contact/irc/"))) "#guix channel"))
          " in IRC Freenode. Tell us how would you like to help, and we
          will do our best to guide you. "))

      ,(G_
        `(p
          "We want to provide a warm, friendly, and harassment-free environment,
           so that anyone can contribute to the best of their abilities.  To
           this end our project uses a “Contributor Covenant”, which was adapted
           from "
          ,(G_ ((lambda (url)
                  `(a (@ (href ,url)) url))
                "https://contributor-covenant.org/"))
          ".  You can find the full pledge in the "
          ,(G_
            `(a (@ (href "//git.savannah.gnu.org/cgit/guix.git/tree/CODE-OF-CONDUCT")
                   (class "mono"))
                "CODE-OF-CONDUCT"))
          " file."))

      (div
       (@ (class "centered-text"))

       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "pms")) "Project Management"))
        ,(G_
          `(p
            "We use "
            ,(G_ `(a (@ (href "https://savannah.gnu.org/")) "Savannah"))
            " as the central point for development, maintenance and
            distribution of the Guix System Distribution and GNU Guix."))
        ,(G_
          `(p
            "The source files for all the components of the project,
            including software, web site, documentation, and artwork, are
            available in "
            ,(G_ `(a (@ (href "https://savannah.gnu.org/git/?group=guix"))
                     "Git repositories"))
            " at Savannah. "))
	(p
	 ,(link-more
           #:label (G_ "Access Savannah")
	   #:url "https://savannah.gnu.org/projects/guix")))

       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "art")) "Art"))
        ,(G_
          `(p
            "We are always looking for artists to help us design and
            improve user interfaces, and create multimedia material for
            documentation, presentations, and promotional items. "))
        ,(G_
          `(p
            "The artwork used in the different components of the project
            is available in the "
            ,(G_
              `(a (@ (href "//git.savannah.gnu.org/cgit/guix/guix-artwork.git"))
                  "guix-artwork"))
            " repository. "))
	(p
	 ,(link-more
           #:label (G_ "Contribute")
	   #:url "https://lists.gnu.org/mailman/listinfo/guix-devel")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "documentation")) "Documentation"))
        ,(G_
          `(p
            "You can read the "
            ,(G_ `(a (@ (href ,(guix-url "help/"))) "project documentation"))
            " already available in the system and in the website, and
            help us identify any errors or omissions. Creating new
            manuals, tutorials, and blog entries will also help users and
            developers discover what we do. "))
        ,(G_
          `(p
            "Helping improve the documentation of the "
            ,(G_ `(a (@ (href ,(guix-url "packages/"))) "packaged software"))
            " is another way to contribute. "))
	(p
	 ,(link-more
           #:label (G_ "Start writing")
	   #:url "https://lists.gnu.org/mailman/listinfo/guix-devel")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "packages")) "Packages"))
        ,(G_
          `(p
            "Hundreds of software, documentation, and assets need to be
            packaged to make it easier for users to install their
            favorite tools with the Guix package manager, and be
            productive using the system. "))
        ;; TRANSLATORS: Packaging Guidelines is a section name in the
        ;; English (en) manual.
        ,(G_
          `(p
            "Information on how to add packages to the distribution can
            be found "
            ,(G_
              (manual-href
               "in the manual"
               (G_ "en")
               (G_ "Packaging-Guidelines.html")))
            ". "))
        ,(G_
          `(p
            "Check out the "
            ,(G_ `(a (@ (href ,(guix-url "packages/")))
                     "package database"))
            " for a list of available packages, and the "
            ,(G_ `(a (@ (href "//bugs.gnu.org/guix-patches"))
                     "patch-tracking database"))
            " for a list of pending submissions."))
	(p
	 ,(link-more
           #:label (G_ "Send a new package")
	   #:url "https://lists.gnu.org/mailman/listinfo/guix-patches")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "programming")) "Programming"))
        ,(G_
          `(p
            "Source code is in the "
            ,(G_ `(a (@ (href "//git.savannah.gnu.org/cgit/guix.git/"))
                     "main Git repository"))
            ".  "
            "We use "
            ,(G_ `(a (@ (href ,(gnu-url "software/guile"))) "GNU Guile"))
            " as the main programming and extension language for the
            components of the system. "))
        ;; TRANSLATORS: Contributing is a section name in the English
        ;; (en) manual.
        ,(G_
          `(p
            "You will find it useful to browse the "
            ,(G_
              `(a (@ (href ,(gnu-url "software/guile/manual")))
                  "Guile manual"))
            " or other "
            ,(G_ `(a (@ (href "http://www.schemers.org/Documents/#intro-texts"))
                     "introductory material about Scheme"))
            ". Also, make sure to read the "
            ,(G_ (manual-href "Contributing"
                              (G_ "en")
                              (G_ "Contributing.html")))
            " section of the manual for more details on the development
            setup, as well as the coding and cooperation conventions used
            in the project. "))
	(p
	 ,(link-more
           #:label (G_ "Send a patch")
	   #:url "https://lists.gnu.org/mailman/listinfo/guix-patches")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "sysadmin")) "System Administration"))
        ,(G_
          `(p
            "Our system infrastructure makes it possible for all the
            contributors to communicate and collaborate in the project,
            and users to be able to download and install packages. Help
            us keep the system up and running smoothly. "))
        ,(G_
          `(p
            "You can also "
            ,(G_ `(a (@ (href ,(guix-url "donate/")))
                     "donate hardware or hosting"))
            " for our "
            ,(G_ `(a (@ (href "https://hydra.gnu.org")) "build farm")) ".  "))
	(p
	 ,(link-more
           #:label (G_ "Contribute")
	   #:url "https://lists.gnu.org/mailman/listinfo/guix-devel")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "testing")) "Test and Bug Reports"))
        ,(G_
          `(p
            "Install the software and send feedback to the community
            about your experience. Help the project by reporting bugs."))
        ,(G_
          `(p
            "Before reporting a bug, please check whether the bug is
            already "
            ,(G_ `(a (@ (href "https://issues.guix.gnu.org"))
                     "in the bug database"))
            ". See "
            ,(G_ `(a (@ (href "https://debbugs.gnu.org/Developer.html"))
                     "the developer information page"))
            " for more information on how to manipulate bug reports. "))
	(p
	 ,(link-more
           #:label (G_ "Report a bug")
	   #:url "https://lists.gnu.org/mailman/listinfo/bug-guix")))


       (div
	(@ (class "summary-box"))
        ,(G_ `(h3 (@ (id "translation")) "Translation"))
        ,(G_
          `(p
            "You can help translate the "
            ,(G_
              `(a (@ (href "https://translationproject.org/domain/guix.html"))
                  "software"))
            ", the "
            ,(G_
              `(a (@ (href "https://translationproject.org/domain/guix-packages.html"))
                  "package descriptions"))
            ", and the "
            ,(G_
              `(a (@ (href "https://translationproject.org/domain/guix-manual.html"))
                  "manual"))
            " into your language.  See the "
            ,(G_
              `(a (@ (href "https://translationproject.org/html/translators.html"))
                  "Translation Project"))
            " for information on how you can help."))
        ,(G_
          `(p
            ,(G_ `(a (@ (href ,(guix-url "packages"))) "Software packages"))
            " provided by the system may have their own translation
            tools.  Visit their websites and help translate. "))
        (p
	 ,(link-more
           #:label (G_ "Start translating")
	   #:url "https://translationproject.org/"))))


      ,(G_ `(h3 (@ (id "resources")) "Other resources for contributors"))
      ,(G_
        `(p
          "Documents, supporting material of previous talks, and
          auxiliary information useful to hackers and maintainers is
          available at "
          (a (@ (href "//git.savannah.gnu.org/cgit/guix/maintenance.git"))
             "git://git.sv.gnu.org/guix/maintenance.git")
          "."))))))
