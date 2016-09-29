;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Initially written by Luis Felipe López Acevedo <felipe.lopez@openmailbox.org>
;;; who waives all copyright interest on this file.
;;;
;;; This file is part of GuixSD website.
;;;
;;; GuixSD website is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GuixSD website is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with GuixSD website.  If not, see <http://www.gnu.org/licenses/>.

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
volunteers around the world under the umbrella of the "
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
                   (a (@ (href ,(gnu-url "software/shepherd")))
                      "initialization system")
                   ", and to the "
                   (a (@ (href ,(base-url
                                 "manual/html_node/Defining-Services.html")))
                      "system services")
                   ".")

		(h2 (@ (id "mantainer")) "Maintainer")
		(p "Guix is currently maintained by Ludovic Courtès and
Ricardo Wurmus.  Please use the "
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

                (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/help-guix"))
                      (b "help-guix"))
                   (small " ("
                          (a (@ (href "https://lists.gnu.org/archive/html/help-guix"))
                             "archive")
                          ") ")
                   (br)
                   "Support for users of GNU Guix and
the Guix System Distribution (GuixSD).  "
                   (a (@ (href
                          "https://lists.gnu.org/archive/html/guix-devel/2015-12/msg00584.html"))
                      "Until December 2015")
                   ", the Guix-devel mailing list filled that role.")
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
                   "Bug reports for GNU Guix and the Guix System
Distribution.  Messages sent to this list populate the "
                   (a (@ (href "https://debbugs.gnu.org/cgi/pkgreport.cgi?package=guix;max-bugs=100"))
                      "bug database") ".")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-commits"))
		      (b "guix-commits"))
		   (small " ("
			  (a (@ (href "https://lists.gnu.org/archive/html/guix-commits"))
			     "archive")
			  ")")
		   (br)
                   "Notifications of commits made to the "
                   (a (@ (href ,(base-url "contribute")))
                      "Git repositories") ".")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-security"))
		      (b "guix-security"))
		   (br)
                   "This is a private mailing list that anyone can post to "
                   "to " (a (@ (href ,(base-url "security")))
                            "report security issues")
                   " in Guix itself or in "
                   "the " (a (@ (href ,(base-url "packages"))) "packages")
                   " it provides.  Posting here allows Guix developers to "
                   "address the problem before it is widely publicized.")
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-sysadmin"))
		      (b "guix-sysadmin"))
		   (br)
                   "Private mailing list for the "
                   (a (@ (href "https://hydra.gnu.org/"))
                      "build farm") " system administration.")


                ;; Non-Guix lists.

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
		(p (a (@ (href "https://lists.gnu.org/mailman/listinfo/info-gnu"))
		      (b "info-gnu"))
		   (small " ("
			  (a (@ (href "http://lists.gnu.org/archive/html/info-gnu/"))
			     "archive")
			  ") ")
		   (br)
		   "GNU software announcements.")

                (h2 (@ (id "artwork")) "Artwork")
                (p "This web site was designed by "
                   (a (@ (href "http://sirgazil.bitbucket.org/"))
                      "Luis Felipe López Acevedo")
                   ".  See the "
                   (a (@ (href ,(base-url "graphics"))) "graphics page")
                   " for information about the Guix and GuixSD logotypes.")))

	  ,(html-page-footer))))
