;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates home)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps blog templates components)
  #:use-module (apps i18n)
  #:export (home-t))


(define (home-t context)
  "Return the Home page in SHTML using the data in CONTEXT."
  (theme
   #:title (C_ "webpage title"
               '("GNU's advanced distro and transactional package manager"))
   #:description
   (G_ "Guix is an advanced distribution of the GNU operating system.
   Guix is technology that respects the freedom of computer users.
   You are free to run the system for any purpose, study how it
   works, improve it, and share it with the whole world.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
   #:active-menu-item (C_ "website menu" "Overview")
   #:css (list
	  (guix-url "static/base/css/item-preview.css")
	  (guix-url "static/base/css/index.css"))
   #:content
   `(main
     ;; Featured content.
     (section
      (@ (class "featured-content"))
      ,(G_ `(h2 (@ (class "a11y-offset")) "Summary"))
      (ul
       ,(G_
         `(li
           ,(G_ `(b "Liberating."))
           " Guix is an advanced distribution of the "
           ,(G_ (link-yellow
                 #:label "GNU operating system"
                 #:url (gnu-url "gnu/about-gnu.html")))
           " developed by the "
           ,(G_ (link-yellow
                 #:label "GNU Project"
                 #:url (gnu-url)))
           "—which respects the "
           ,(G_ (link-yellow
                 #:label "freedom of computer users"
                 #:url (gnu-url "distros/free-system-distribution-\
guidelines.html")))
           ". "))

       ;; TRANSLATORS: Package Management, Features and Using the
       ;; Configuration System are section names in the English (en)
       ;; manual.
       ,(G_
         `(li
           ,(G_ `(b "Dependable."))
           " Guix "
           ,(G_ (manual-link-yellow "supports"
                                    (G_ "en")
                                    (G_ "Package-Management.html")))
           " transactional upgrades and roll-backs, unprivileged \
package management, "
           ,(G_ (manual-link-yellow "and more"
                                    (G_ "en")
                                    (G_ "Features.html")))
           ".  When used as a standalone distribution, Guix supports "
           ,(G_ (manual-link-yellow "declarative system configuration"
                                    (G_ "en")
                                    (G_ "Using-the-Configuration-System.html")))
           " for transparent and reproducible operating systems."))

       ;; TRANSLATORS: Defining Packages and System Configuration are
       ;; section names in the English (en) manual.
       ,(G_
         `(li
           ,(G_ `(b "Hackable."))
           " It provides "
           ,(G_ (link-yellow
                 #:label "Guile Scheme"
                 #:url (gnu-url "software/guile/")))
           " APIs, including high-level embedded domain-specific \
languages (EDSLs) to "
           ,(G_ (manual-link-yellow "define packages"
                                    (G_ "en")
                                    (G_ "Defining-Packages.html")))
           " and "
           ,(G_ (manual-link-yellow "whole-system configurations"
                                    (G_ "en")
                                    (G_ "System-Configuration.html")))
           ".")))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
         #:label (apply string-append
                        (C_ "button" `("DOWNLOAD v" ,(latest-guix-version) "")))
	 #:url (guix-url "download/")
	 #:light #true)
       " " ; A space for readability in non-CSS browsers.
       ,(button-big
         #:label (C_ "button" "CONTRIBUTE")
	 #:url (guix-url "contribute/")
	 #:light #true)))

     ;; Discover Guix.
     (section
      (@ (class "discovery-box"))
      ,(G_ `(h2 "Discover Guix"))

      ,(G_
        `(p
          (@ (class "limit-width centered-block"))
          "Guix comes with thousands of packages which include \
applications, system tools, documentation, fonts, and other digital \
goods readily available for installing with the "
          ,(G_ (link-yellow #:label "GNU Guix"
                            #:url (identity "#guix-in-other-distros")))
          " package manager."))

      (div
       (@ (class "screenshots-box"))
       ,@(map screenshot->shtml (context-datum context "screenshots")))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
         #:label (C_ "button" "ALL PACKAGES")
	 #:url (guix-url "packages/")
	 #:light #true))

      ,(horizontal-separator #:light #true)

      ;; Guix in different fields.
      ,(G_ `(h3 "GNU Guix in your field"))

      ,(G_
        `(p
          (@ (class "limit-width centered-block"))
          "Read some stories about how people are using GNU Guix in
their daily lives."))

      (div
       (@ (class "fields-box"))

       " " ; A space for readability in non-CSS browsers (same below).
       ,(button-big
         #:label (C_ "button" "SOFTWARE DEVELOPMENT")
         #:url (guix-url "blog/tags/software-development/")
         #:light #true)
       " "
       ,(button-big
         #:label (C_ "button" "BIOINFORMATICS")
         #:url (guix-url "blog/tags/bioinformatics/")
         #:light #true)
       " "
       ,(button-big
         #:label (C_ "button" "HIGH PERFORMANCE COMPUTING")
         #:url (guix-url "blog/tags/high-performance-computing/")
         #:light #true)
       " "
       ,(button-big
         #:label (C_ "button" "RESEARCH")
         #:url (guix-url "blog/tags/research/")
         #:light #true)
       " "
       ,(button-big
         #:label (C_ "button" "ALL FIELDS...")
         #:url (guix-url "blog/")
         #:light #true))

      ,(horizontal-separator #:light #true)

      ;; Using Guix in other distros.
      ,(G_
        `(h3
          (@ (id "guix-in-other-distros"))
          "GNU Guix in other GNU/Linux distros"))

      (div
       (@ (class "info-box"))
       (video
	(@ (class "video-preview")
	   (src "https://audio-video.gnu.org/video/misc/2016-07__GNU_Guix_Demo_2.webm")
	   (poster ,(guix-url "static/media/img/guix-demo.png"))
	   (controls "controls"))
        ,(G_
          `(p
            "Video: "
            ,(G_ (link-yellow
                  #:label "Demo of Guix in another GNU/Linux distribution"
                  #:url "https://audio-video.gnu.org/video/misc/\
2016-07__GNU_Guix_Demo_2.webm"))
            " (1 minute, 30 seconds)."))))

      (div
       (@ (class "info-box justify-left"))
       ,(G_ `(p
              "If you don't use GNU Guix as a standalone GNU/Linux \
distribution, you still can use it as a package manager on top of any \
GNU/Linux distribution. This way, you can benefit from all its conveniences."))

       ,(G_ `(p
              "Guix won't interfere with the package manager that comes \
with your distribution. They can live together.")))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
         #:label (C_ "button" "TRY IT OUT!")
	 #:url (guix-url "download/")
	 #:light #true)))

     ;; Latest Blog posts.
     (section
      (@ (class "centered-text"))
      ,(G_ `(h2 "Blog"))

      ,@(map post-preview (context-datum context "posts"))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
         #:label (C_ "button" "ALL POSTS")
	 #:url (guix-url "blog/"))))

     ;; Contact info.
     (section
      (@ (class "contact-box centered-text"))
      ,(G_ `(h2 "Contact"))

      ,@(map contact-preview (context-datum context "contact-media"))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
         #:label (C_ "button" "ALL CONTACT MEDIA")
	 #:url (guix-url "contact/")))))))
