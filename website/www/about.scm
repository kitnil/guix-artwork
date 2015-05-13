(define-module (www about)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (about-page))

(define (about-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "About")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)

	  (div (@ (id "content-box"))
	       (article
		(h1 "About the Project")
		(p "The "
		   (em "Guix System Distribution (GuixSD)")
		   " and the "
		   (em "GNU Guix")
		   " package manager are "
                   (a (@ (href ,(gnu-url "philosophy/free-sw.html")))
                      "free software")
                   " projects developed by
volunteers around the work under the umbrella of the "
		   (a (@ (href ,(gnu-url "")))
		      "GNU Project") ".  "
                      " This is the official web site for both projects. ")

                (p "GuixSD is a GNU/Linux distribution committed to
respecting and enhancing "
                   (a (@ (href ,(gnu-url "philosophy/free-sw.html")))
                      "the freedom of its users")
                   ".  As such, it adheres to the "
                   (a (@ (href ,(gnu-url
                                 "distros/free-system-distribution-guidelines.html")))
                      "GNU Free System Distribution Guidelines") ".")

                (p "GNU Guix provides "
                   (a (@ (href ,(base-url "manual/html_node/Features.html")))
                      "state-of-the-art package management features")
                   " such as transactional upgrades and roll-backs,
reproducible build environments, unprivileged package management, and
per-user profiles.  It uses low-level mechanisms from the "
                   (a (@ (href "https://nixos.org/nix/")) "Nix")
                   " package manager, but packages are "
                   (a (@ (href ,(base-url
                                 "manual/html_node/Defining-Packages.html")))
                      "defined")
                   " as native "
                   (a (@ (href ,(gnu-url "software/guile"))) "Guile")
                   " modules, using extensions to the "
                   (a (@ (href "http://schemers.org")) "Scheme")
                   " language—which makes it nicely hackable.")

                (p "GuixSD takes that a step further by supporting stateless,
reproducible "
                   (a (@ (href ,(base-url
                                 "manual/html_node/Using-the-Configuration-System.html")))
                      "operating system configurations")
                   ".  This time the whole system is hackable in Scheme, from
the "
                   (a (@ (href ,(base-url
                                 "manual/html_node/Initial-RAM-Disk.html")))
                      "initial RAM disk")
                   " to the "
                   (a (@ (href ,(gnu-url "software/dmd")))
                      "initialization system")
                   ", and to the "
                   (a (@ (href ,(base-url
                                 "manual/html_node/Defining-Services.html")))
                      "system services")
                   ".")

		(h2 (@ (id "mantainer")) "Maintainer")
		(p "Guix is currently being maintained by Ludovic
Courtès. Please use the "
		   (a (@ (href "#contact")) "mailing lists")
		   " for contact. ")

		(h2 (@ (id "license")) "Licensing")
		(p "Guix is free software; you can redistribute it and/or modify
it under the terms of the "
		   (a (@ (rel "license") (href ,(gnu-url "licenses/gpl.html")))
		      "GNU General Public License")
		   " as published by the Free Software Foundation; either
version\xa03 of the License, or (at your option) any later version. ")

		(h2 (@ (id "contact")) "Contact")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
		      (b "guix-devel"))
		   (small " ("
			  (a (@ (href "https://lists.gnu.org/archive/html/guix-devel"))
			     "archive")
			  ")")
		   (br)
		   "Discussion about the development of GNU Guix and the Guix
System Distribution (GuixSD). "
		   (a (@ (href "http://lists.gnu.org/archive/html/bug-guix/2013-07/msg00039.html"))
		      " Until July 2013")
		   ", the bug-Guix mailing list filled that role. ")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
		      (b "bug-guix"))
		   (small " ("
			  (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
			     "archive")
			  ")")
		   (br)
		   "Bug reports for GNU Guix and the Guix System Distribution.")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/gnu-system-discuss"))
		      (b "gnu-system-discuss"))
		   (small " ("
			  (a (@ (href "http://lists.gnu.org/archive/html/gnu-system-discuss/"))
			     "archive")
			  ") ")
		   (br)
		   "Discussion about the development of the broader GNU system.")
		(p (a (@ (href "https://lists.nongnu.org/mailman/listinfo/gnu-linux-libre"))
		      (b "gnu-linux-libre"))
		   (small " ("
			  (a (@ (href "http://lists.nongnu.org/archive/html/gnu-linux-libre/"))
			     "archive")
			  ") ")
		   (br)
		   "Workgroup for fully free GNU/Linux distributions.")
		(dl (dt "Commit notifications")
		    (dd (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-commits"))
			      "Guix-commits")
			   " receives notifications of commits to the "
			   (a (@ (href "#contribute"))
			      "version control repository")
			   "."))
		    (dt "Announcements")
		    (dd (p "Announcements about Guix and most other GNU software
are made on "
			   (a (@ (href "http://lists.gnu.org/mailman/listinfo/info-gnu"))
			      "info-gnu")
			   "("
			   (a (@ (href "http://lists.gnu.org/archive/html/info-gnu/"))
			      "archive")
			   ")."))
		    (dt "Security reports")
		    (dd (p "Security reports that should not be made immediately
public can be sent directly to the maintainer.  If there is no response to an
urgent issue, you can escalate to the general "
			   (a (@ (href "http://lists.gnu.org/mailman/listinfo/security"))
			      "security")
			   " mailing list for advice."))
		    (dt "Internet relay chat")
		    (dd (p "Some Guix users and developers hang out on the "
			   (em "#guix")
			   " channel of the Freenode IRC network ("
			   (a (@ (href "https://gnunet.org/bot/log/guix/"))
			      "logs")
			   ").")))

                (h2 (@ (id "artwork")) "Artwork")
                (p "This web site was designed by "
                   (a (@ (href "http://sirgazil.bitbucket.org/"))
                      "Luis Felipe López Acevedo")
                   ".  See the "
                   (a (@ (href ,(base-url "graphics"))) "graphics page")
                   " for information about the Guix and GuixSD logotypes.")))

	  ,(html-page-footer))))
