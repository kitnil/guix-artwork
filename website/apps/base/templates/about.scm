;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates about)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (about-t))


(define (about-t)
  "Return the About page in SHTML."
  (theme
   #:title '("About")
   #:description
   "Guix is an advanced distribution of the GNU operating system.
    Guix is technology that respects the freedom of computer users.
    You are free to run the system for any purpose, study how it
    works, improve it, and share it with the whole world."
   #:keywords
   (list "GNU" "Linux" "Unix" "Free software" "Libre software"
	 "Operating system" "GNU Hurd" "GNU Guix package manager")
   #:active-menu-item "About"
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb "About" "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 "About the Project")

      (p
       "The " (em "GNU Guix") " package and system manager is a "
       (a (@ (href ,(gnu-url "philosophy/free-sw.html")))
	  "free software")
       " project developed by volunteers around the world under the
       umbrella of the " (a (@ (href ,(gnu-url))) "GNU Project") ". ")

      (p
       "Guix System is an advanced distribution of the "
       (a (@ (href ,(gnu-url))) "GNU operating system")
       ".  It uses the "
       (a (@ (href ,(gnu-url "software/linux-libre"))) "Linux-libre")
       " kernel, and support for "
       (a (@ (href ,(gnu-url "software/hurd"))) "the Hurd")
       " is being worked on.  As a GNU distribution, it is committed
       to respecting and enhancing "
       (a (@ (href ,(gnu-url "philosophy/free-sw.html")))
	  "the freedom of its users")
       ".  As such, it adheres to the "
       (a (@ (href ,(gnu-url "distros/free-system-distribution-guidelines.html")))
	  "GNU Free System Distribution Guidelines") ".")

      (p
       "GNU Guix provides "
       (a (@ (href ,(manual-url "Features.html")))
	  "state-of-the-art package management features")
       " such as transactional upgrades and roll-backs, reproducible
       build environments, unprivileged package management, and
       per-user profiles.  It uses low-level mechanisms from the "
       (a (@ (href "https://nixos.org/nix/")) "Nix")
       " package manager, but packages are "
       (a (@ (href ,(manual-url "Defining-Packages.html"))) "defined")
       " as native "
       (a (@ (href ,(gnu-url "software/guile"))) "Guile")
       " modules, using extensions to the "
       (a (@ (href "http://schemers.org")) "Scheme")
       " language—which makes it nicely hackable.")

      (p
       "Guix takes that a step further by additionally supporting stateless,
       reproducible "
       (a (@ (href ,(manual-url "Using-the-Configuration-System.html")))
	  "operating system configurations")
       ". This time the whole system is hackable in Scheme, from the "
       (a (@ (href ,(manual-url "Initial-RAM-Disk.html")))
	  "initial RAM disk")
       " to the "
       (a (@ (href ,(gnu-url "software/shepherd")))
	  "initialization system")
       ", and to the "
       (a (@ (href ,(manual-url "Defining-Services.html")))
	  "system services")
       ".")


      (h3 (@ (id "mantainer")) "Maintainer")

      (p
       "Guix is currently maintained by Ludovic Courtès and Ricardo
       Wurmus.  Please use the "
       (a (@ (href ,(guix-url "contact/"))) "mailing lists")
       " for contact. ")


      (h3 (@ (id "license")) "Licensing")

      (p
       "Guix is free software; you can redistribute it and/or modify
       it under the terms of the "
       (a (@ (rel "license") (href ,(gnu-url "licenses/gpl.html")))
	  "GNU General Public License")
       " as published by the Free Software Foundation; either
       version\xa03 of the License, or (at your option) any later
       version. ")))))
