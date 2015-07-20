;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
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

(define-module (www contribute)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (contribute-page))

(define (contribute-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Contribute")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Contribute")
		(p "The Guix System Distribution is a large project developed
mostly by volunteers from all around the world. You are welcome to join us in
the "
		   (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel"))
		      "development mailing list")
		   " or in the "
		   (a (@ (href "http://webchat.freenode.net/?channels=%23guix"))
		      "#guix channel")
		   " in IRC Freenode. Tell us how would you like to help, and we
will do our best to guide you. ")
		(div (@ (class "text-center"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "pms"))
			      "Project Management")
			  (p "We use "
			     (a (@ (href "https://savannah.gnu.org/"))
				"Savannah")
			     " as the central point for development,
maintenance and distribution of the Guix System Distribution and GNU Guix.")
			  (p "The source files for all the components of the
project, including software, web site, documentation, and artwork, are available
in "
                             (a (@ (href
                                    "https://savannah.gnu.org/git/?group=guix"))
                                "Git repositories")
                             " at Savannah. ")
			  (a (@ (href "https://savannah.gnu.org/projects/guix")
				(class "hlink-more-dark"))
			     "Access Savannah"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "art")) "Art")
			  (p "We are always looking for artists to help us
design and improve user interfaces, and create multimedia material for
documentation, presentations, and promotional items. ")
			  (p "The artwork used in the different components of
the project is available in the "
			     (a (@ (href "http://git.savannah.gnu.org/cgit/guix/guix-artwork.git"))
				"guix-artwork")
			     " repository. ")
			  (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel")
				(class "hlink-more-dark"))
			     "Contribute"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "documentation"))
			      "Documentation")
			  (p "You can read the "
			     (a (@ (href ,(base-url "help")))
				"project documentation")
			     " already available in the system and in the
website, and help us identify any errors or omissions. Creating new manuals,
tutorials, and blog entries will also help users and developers discover what we
do. ")
			  (p "Helping improve the documentation of
the "
			     (a (@ (href ,(base-url "packages")))
				"packaged software")
			     " is another way to contribute. ")
			  (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel")
				(class "hlink-more-dark"))
			     "Start writing"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "packages")) "Packages")
			  (p "Hundreds of software, documentation, and assets
need to be packaged to make it easier for users to install their favorite tools
with the Guix package manager, and be productive using the system. ")
			  (p "Information on how to add packages to the
distribution can be found "
			     (a (@ (href ,(guix-url "manual/html_node/Packaging-Guidelines.html")))
				"in the manual")
			     ". ")
			  (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel")
				(class "hlink-more-dark"))
			     "Send a new package"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "programming")) "Programming")
			  (p "Source code is in the "
                             (a (@ (href
                                    "http://git.savannah.gnu.org/cgit/guix.git/"))
                                "main Git repository") ".  "
                             "We use "
			     (a (@ (href ,(gnu-url "software/guile")))
				"GNU Guile")
			     " as the main programming and extension language
for the components of the system. ")
			  (p "You will find it useful to browse the "
			     (a (@ (href ,(gnu-url "software/guile/manual")))
				"Guile manual")
			     " or other "
			     (a (@ (href "http://www.schemers.org/Documents/#intro-texts"))
				"introductory material about Scheme")
			     ". Also, make sure to read the "
			     (a (@ (href "http://git.savannah.gnu.org/cgit/guix.git/tree/HACKING"))
				"HACKING")
			     " files for more details on the development setup,
as well as the coding and cooperation conventions used in the project. ")
			  (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel")
				(class "hlink-more-dark"))
			     "Send a new patch"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "sysadmin"))
			      "System Administration")
			  (p "Our system infrastructure makes it possible for
all the contributors to communicate and collaborate in the project, and users to
be able to download and install packages. Help us keep the system up and running
smoothly. ")
			  (p "You can also "
			     (a (@ (href ,(base-url "donate")))
				"donate hardware or hosting")
			     " for our "
                             (a (@ (href "http://hydra.gnu.org"))
                                "build farm") ".  ")
			  (a (@ (href "http://lists.gnu.org/mailman/listinfo/guix-devel")
				(class "hlink-more-dark"))
			     "Contribute"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "testing"))
			      "Test and Bug Reports")
			  (p "Install the software and send feedback to the
community about your experience. Help the project reporting bugs.")
			  (p "Before reporting a bug, please check whether the
bug is already "
			     (a (@ (href "https://debbugs.gnu.org/guix"))
				"in the bug database")
			     ". See "
			     (a (@ (href "https://debbugs.gnu.org/Developer.html"))
				"the developer information page")
			     " for more information on how to manipulate bug
reports. ")
			  (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix")
				(class "hlink-more-dark"))
			     "Report a bug"))
		     (div (@ (class "summary-box"))
			  (h2 (@ (id "translation")) "Translation")
			  (p "You can help translate the "
                             (a (@ (href
                                    "http://translationproject.org/domain/guix.html"))
                                "software")
                             " and the "
                             (a (@ (href
                                    "http://translationproject.org/domain/guix-packages.html"))
                                "package descriptions")
                             " to your language.  "
                             "See the"
			     (a (@ (href "https://translationproject.org/html/translators.html"))
				"Translation Project")
			     " for information on how you can help.")
			  (p (a (@ (href ,(base-url "packages")))
				"Software packages")
			     " provided by the system may have their own translation
tools.  Visit their websites and help translate. ")
			  (a (@ (href "https://translationproject.org/")
				(class "hlink-more-dark"))
			     "Start translating")))
		(h2 (@ (id "resources"))
		    "Other resources for contributors")
		(p "Documents, supporting material of previous talks, and
auxiliary information useful to hackers and maintainers is available at "
		   (a (@ (href "http://git.savannah.gnu.org/cgit/guix/maintenance.git"))
		      "git://git.sv.gnu.org/guix/maintenance.git")
		   ".")))
	  ,(html-page-footer))))
