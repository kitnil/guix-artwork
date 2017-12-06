;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates home)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps blog templates components)
  #:export (home-t))


(define (home-t context)
  "Return the Home page in SHTML using the data in CONTEXT."
  (theme
   #:title '("GNU's advanced distro and transactional package manager")
   #:description
   "GuixSD is an advanced distribution of the GNU operating system.
   GuixSD is technology that respects the freedom of computer users.
   You are free to run the system for any purpose, study how it works,
   improve it, and share it with the whole world."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "GNU Guile" "Guile Scheme" "Transactional upgrades"
     "Functional package management" "Reproducibility")
   #:active-menu-item "Overview"
   #:css (list
	  (guix-url "static/base/css/item-preview.css")
	  (guix-url "static/base/css/index.css"))
   #:content
   `(main
     ;; Featured content.
     (section
      (@ (class "featured-content"))
      (h2 (@ (class "a11y-offset")) "Summary")
      (ul
       (li
	(b "Liberating.")
	" The Guix System Distribution (GuixSD) is an advanced
        distribution of the "
	,(link-yellow
	  #:label "GNU operating system"
	  #:url (gnu-url "gnu/about-gnu.html"))
	" developed by the "
	,(link-yellow
	  #:label "GNU Project"
	  #:url (gnu-url))
	"—which respects the "
	,(link-yellow
	  #:label "freedom of computer users"
	  #:url (gnu-url "distros/free-system-distribution-guidelines.html"))
	". ")

       (li
	(b "Dependable.")
	" It comes with the "
	,(link-yellow
	  #:label "GNU Guix package manager"
	  #:url (manual-url "Package-Management.html"))
	", which in addition to standard package management features,
        supports transactional upgrades and roll-backs, unprivileged
        package management, per-user profiles, "
	,(link-yellow
	  #:label "and more"
	  #:url (manual-url "Features.html"))
	".")

       (li
	(b "Hackable.")
	" It provides "
	,(link-yellow
	  #:label "Guile Scheme"
	  #:url (gnu-url "software/guile/"))
	" APIs, including high-level embedded domain-specific
        languages (EDSLs) to "
	,(link-yellow
	  #:label "define packages"
	  #:url (manual-url "Defining-Packages.html"))
	" and "
	,(link-yellow
	  #:label "whole-system configurations"
	  #:url (manual-url "System-Configuration.html"))
	"."))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
	 #:label (string-append "DOWNLOAD v" (latest-guix-version))
	 #:url (guix-url "download/")
	 #:light #true)
       " " ; A space for readability in non-CSS browsers.
       ,(button-big
	 #:label "CONTRIBUTE"
	 #:url (guix-url "contribute/")
	 #:light #true)))

     ;; Discover GuixSD.
     (section
      (@ (class "discovery-box"))
      (h2 "Discover GuixSD")

      (p
       (@ (class "limit-width centered-block"))
       "GuixSD comes with thousands of packages which include
       applications, system tools, documentation, fonts, and other
       digital goods readily available for installing with the "
       ,(link-yellow #:label "GNU Guix" #:url "#guix-in-other-distros")
       " package manager.")

      (div
       (@ (class "screenshots-box"))
       ,@(map screenshot->shtml (context-datum context "screenshots")))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
	 #:label "ALL PACKAGES"
	 #:url (guix-url "packages/")
	 #:light #true))

      ,(horizontal-separator #:light #true)

      ;; GuixSD in different fields.
      (h3 "GuixSD and GNU Guix in your field")

      (p
       (@ (class "limit-width centered-block"))
       "Read some stories about how people are using GuixSD and GNU
       Guix in their daily lives.")

      (div
       (@ (class "fields-box"))

       " " ; A space for readability in non-CSS browsers (same below).
       ,(button-big
	 #:label "SOFTWARE DEVELOPMENT"
	 #:url (guix-url "blog/tags/software-development/")
	 #:light #true)
       " "
       ,(button-big
	 #:label "BIOINFORMATICS"
	 #:url (guix-url "blog/tags/bioinformatics/")
	 #:light #true)
       " "
       ,(button-big
	 #:label "HIGH PERFORMANCE COMPUTING"
	 #:url (guix-url "blog/tags/high-performance-computing/")
	 #:light #true)
       " "
       ,(button-big
	 #:label "RESEARCH"
	 #:url (guix-url "blog/tags/research/")
	 #:light #true)
       " "
       ,(button-big
	 #:label "ALL FIELDS..."
	 #:url (guix-url "blog/")
	 #:light #true))

      ,(horizontal-separator #:light #true)

      ;; Using Guix in other distros.
      (h3
       (@ (id "guix-in-other-distros"))
       "GNU Guix in other GNU/Linux distros")

      (div
       (@ (class "info-box"))
       (video
	(@ (class "video-preview")
	   (src "https://audio-video.gnu.org/video/misc/2016-07__GNU_Guix_Demo_2.webm")
	   (poster ,(guix-url "static/media/img/guix-demo.png"))
	   (controls "controls"))
	(p
	 "Video: "
	 ,(link-yellow
	   #:label "Demo of Guix in another GNU/Linux distribution"
	   #:url "https://audio-video.gnu.org/video/misc/2016-07__GNU_Guix_Demo_2.webm")
	 " (1 minute, 30 seconds).")))

      (div
       (@ (class "info-box justify-left"))
       (p
	"If you don't use the "
	,(link-yellow
	  #:label "Guix System Distribution"
	  #:url (guix-url))
	" for a particular reason, you still can use the "
	,(link-yellow
	  #:label "GNU Guix"
	  #:url (guix-url))
	" package manager on top of any GNU/Linux distribution. This
        way, you can benefit from all its conveniences.")

       (p
	"GNU Guix won't interfere with the package manager that comes
        with your distribution. They can leave together."))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
	 #:label "TRY IT OUT!"
	 #:url (guix-url "download/")
	 #:light #true)))

     ;; Latest Blog posts.
     (section
      (@ (class "centered-text"))
      (h2 "Blog")

      ,@(map post-preview (context-datum context "posts"))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
	 #:label "ALL POSTS"
	 #:url (guix-url "blog/"))))

     ;; Contact info.
     (section
      (@ (class "contact-box centered-text"))
      (h2 "Contact")

      ,@(map contact-preview (context-datum context "contact-media"))

      (div
       (@ (class "action-box centered-text"))
       ,(button-big
	 #:label "ALL CONTACT MEDIA"
	 #:url (guix-url "contact/")))))))
