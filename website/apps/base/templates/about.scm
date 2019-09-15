;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates about)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (about-t))


(define (about-t)
  "Return the About page in SHTML."
  (theme
   #:title (C_ "webpage title" '("About"))
   #:description
   (G_ "Guix is an advanced distribution of the GNU operating system.
   Guix is technology that respects the freedom of computer users.
   You are free to run the system for any purpose, study how it
   works, improve it, and share it with the whole world.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager") #\|)
   #:active-menu-item (C_ "website menu" "About")
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb (C_ "website menu" "About") "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      ,(G_ `(h2 "About the Project"))

      ,(G_
        `(p
          "The " ,(G_ `(em "GNU Guix")) " package and system manager is a "
          ,(G_ `(a (@ (href ,(gnu-url "philosophy/free-sw.html")))
                   "free software"))
          " project developed by volunteers around the world under the
            umbrella of the "
          ,(G_ `(a (@ (href ,(gnu-url))) "GNU Project")) ". "))

      ,(G_
        `(p
          "Guix System is an advanced distribution of the "
          ,(G_ `(a (@ (href ,(gnu-url))) "GNU operating system"))
          ".  It uses the "
          ,(G_ `(a (@ (href ,(gnu-url "software/linux-libre"))) "Linux-libre"))
          " kernel, and support for "
          ,(G_ `(a (@ (href ,(gnu-url "software/hurd"))) "the Hurd"))
          " is being worked on.  As a GNU distribution, it is committed
            to respecting and enhancing "
          ,(G_ `(a (@ (href ,(gnu-url "philosophy/free-sw.html")))
                   "the freedom of its users"))
          ".  As such, it adheres to the "
          ,(G_ `(a (@ (href ,(gnu-url "distros/free-system-distribution-guidelines.html")))
                   "GNU Free System Distribution Guidelines")) "."))

      ;; TRANSLATORS: Features and Defining Packages are section names
      ;; in the English (en) manual.
      ,(G_
        `(p
          "GNU Guix provides "
          ,(G_ (manual-href "state-of-the-art package management features"
                            (G_ "en")
                            (G_ "Features.html")))
          " such as transactional upgrades and roll-backs, reproducible
            build environments, unprivileged package management, and
            per-user profiles.  It uses low-level mechanisms from the "
          ,(G_ `(a (@ (href "https://nixos.org/nix/")) "Nix"))
          " package manager, but packages are "
          ,(G_ (manual-href "defined" (G_ "en") (G_ "Defining-Packages.html")))
          " as native "
          ,(G_ `(a (@ (href ,(gnu-url "software/guile"))) "Guile"))
          " modules, using extensions to the "
          ,(G_ `(a (@ (href "http://schemers.org")) "Scheme"))
          " language—which makes it nicely hackable."))

      ;; TRANSLATORS: Using the Configuration System, Initial RAM Disk
      ;; and Defining Services are section names in the English (en)
      ;; manual.
      ,(G_
        `(p
          "Guix takes that a step further by additionally supporting stateless,
           reproducible "
          ,(G_ (manual-href "operating system configurations"
                            (G_ "en")
                            (G_ "Using-the-Configuration-System.html")))
          ". This time the whole system is hackable in Scheme, from the "
          ,(G_ (manual-href "initial RAM disk"
                            (G_ "en")
                            (G_ "Initial-RAM-Disk.html")))
          " to the "
          ,(G_ `(a (@ (href ,(gnu-url "software/shepherd")))
                   "initialization system"))
          ", and to the "
          ,(G_ (manual-href "system services"
                            (G_ "en")
                            (G_ "Defining-Services.html")))
          "."))


      ,(G_ `(h3 (@ (id "mantainer")) "Maintainer"))

      ,(G_
        `(p
          "Guix is currently maintained by Ludovic Courtès and Ricardo
          Wurmus.  Please use the "
          ,(G_ `(a (@ (href ,(guix-url "contact/"))) "mailing lists"))
          " for contact. "))


      ,(G_ `(h3 (@ (id "license")) "Licensing"))

      ,(G_
        `(p
          "Guix is free software; you can redistribute it and/or modify
          it under the terms of the "
          ,(G_ `(a (@ (rel "license") (href ,(gnu-url "licenses/gpl.html")))
                   "GNU General Public License"))
          " as published by the Free Software Foundation; either
          version\xa03 of the License, or (at your option) any later
          version. "))))))
