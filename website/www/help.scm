;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (www help)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (help-page))

(define (help-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Help" #:css "help.css")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Help")
		(div (@ (class "text-center"))
		     (div (@ (class "summary-box"))
			  (div (@ (class "text-center"))
			       (img (@ (src ,(image-url "guixsd-manual-icon.png"))
				       (alt ""))))
			  (h2 "GuixSD Manual")
			  (p "The documentation about the Guix System
Distribution is available online as part of the GNU Guix package manager
manual.")
			  (a (@ (href ,(guix-url "manual/html_node/GNU-Distribution.html"))
				(class "hlink-more-dark"))
			     "Read the manual"))
		     (div (@ (class "summary-box"))
			  (div (@ (class "text-center"))
			       (img (@ (src ,(image-url "guix-manual-icon.png"))
				       (alt ""))))
			  (h2 "GNU Guix Manual")
			  (p "Documentation for the GNU Guix package manager is
available online. You may also find more information about Guix by running "
			     (em "info\xa0guix")
			     ".")
			  (a (@ (href ,(guix-url "manual"))
				(class "hlink-more-dark"))
			     "Read the manual"))
		     (div (@ (class "summary-box"))
			  (div (@ (class "text-center"))
			       (img (@ (src ,(image-url "library-icon.png"))
				       (alt ""))))
			  (h2 "GNU Manuals")
			  (p "GuixSD is a distribution of the "
			     (a (@ (href ,(gnu-url ""))) "GNU operating system")
			     ". Most GNU software is documented and the
documentation is available online in various formats. ")
			  (a (@ (href ,(gnu-url "doc/doc.en.html"))
				(class "hlink-more-dark"))
			     "Browse the manuals"))
		     (div (@ (class "summary-box"))
			  (div (@ (class "text-center"))
			       (img (@ (src ,(image-url "chat-icon.png"))
				       (alt ""))))
			  (h2 "IRC Chat")
			  (p "For real-time support from the community, you can
connect to the "
			     (em "#guix")
			     " channel on irc.freenode.net. There you can get
help about anything related to both the Guix System Distribution and GNU Guix.")
			  (p "The "
			     (em "#guix")
			     " channel is logged. Previous conversations can be
browsed online. See the "
			     (a (@ (href "https://gnunet.org/bot/log/guix/"))
				"channel logs")
			     ". ")
			  (a (@ (href "https://webchat.freenode.net/?channels=%23guix")
				(class "hlink-more-dark"))
			     "Connect"))
		     (div (@ (class "summary-box"))
			  (div (@ (class "text-center"))
			       (img (@ (src ,(image-url "email-icon.png"))
				       (alt ""))))
			  (h2 "Mailing lists")
			  (p "Email support from the community is also available
through the following mailing lists. The messages sent to the lists are public
and archived online.")

			  (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/help-guix"))
				(b "help-guix"))
			     (small " ("
				    (a (@ (href "https://lists.gnu.org/archive/html/help-guix"))
				       "archive")
				    ") ")
			     (br)
			     "Support for users of GNU Guix and
the Guix System Distribution (GuixSD).")
			  (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
				(b "bug-guix"))
			     (small " ("
				    (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
				       "archive")
				    ") ")
			     (br)
			     "Bug reports for GNU Guix and the Guix System
Distribution.  Messages sent to this list populate the "
                             (a (@ (href "https://debbugs.gnu.org/cgi/pkgreport.cgi?package=guix;max-bugs=100"))
                                "bug database") ".")
			  (a (@ (href ,(base-url "about#contact"))
				(class "hlink-more-dark"))
			     "Check all the lists")))

		(h2 (@ (id "papers")) "Papers")

		(ul (li (a (@ (href "https://arxiv.org/abs/1305.4584"))
                           (i "Functional Package Management with Guix"))
			", presented at the "
			(a (@ (href "https://www-sop.inria.fr/members/Manuel.Serrano/conferences/els13.html"))
			   "2013 European Lisp Symposium (ELS)")
			", describes the rationale, design, and
implementation of Guix's packaging API.")
                    (li (a (@ (href "http://arxiv.org/abs/1506.02822"))
                           (i "Reproducible and User-Controlled Software
Environments in HPC with Guix"))
                        ", presented at the "
                        (a (@ (href "http://reppar.org/"))
                           "2015 International Workshop on Reproducibility in
Parallel Computing (RepPar)")
                        ", discusses the use of Guix in high-performance
computing (HPC).")
                    (li (a (@ (href
                               "http://nixos.org/~eelco/pubs/phd-thesis.pdf"))
                           (i "The Purely Functional Software Deployment
Model"))
                        ", 2006; this is Eelco Dolstra's seminal PhD thesis
about the Nix package manager, which "
                        (a (@ (href
                               ,(guix-url "manual/html_node/Acknowledgments.html")))
                           "Guix is based on") "."))

                (h2 (@ (id "posts")) "Blog Posts")

                ;; For posts other than announcements, things that complement
                ;; the manual.
                (ul (li (a (@ (href
                               ,(base-url "news/guixsd-system-tests.html")))
                           (i "GuixSD system tests"))
                        ", June 2016")
                    (li (a (@ (href
                               ,(base-url "news/gnome-in-guixsd.html")))
                           (i "GNOME in GuixSD"))
                        ", March 2016")
                    (li (a (@ (href
                               ,(base-url "news/timely-delivery-of-security-updates.html")))
                           (i "Timely delivery of security updates"))
                        ", March 2016")
                    (li (a (@ (href
                               ,(base-url "news/service-composition-in-guixsd.html")))
                           (i "Service composition in GuixSD"))
                        ", November 2015")
                    (li (a (@ (href
                               ,(base-url "news/reproducible-builds-a-means-to-an-end.html")))
                           (i "Reproducible builds: a means to an end"))
                        ", November 2015")
                    (li (a (@ (href
                               ,(base-url "news/container-provisioning-with-guix.html")))
                           (i "Container provisioning with Guix"))
                        ", October 2015")
                    (li (a (@ (href ,(base-url "news")))
                           "more posts")))

		(h2 (@ (id "talks")) "Talks")
		(ul (li "September 2016, "
                        (a (@ (href "http://cufp.org/2016/"))
                           "Commercial Users of Functional Programming (CUFP)")
                        ", "
                        (i "Guix: Scheme as a uniform OS admin and deployment
interface")
                        " (Ludovic Courtès): "
                        (a (@ (href ,(slides-url "guix-cufp-20160924.pdf")))
                           "slides"))
                    (li "September 2016, "
                        (a (@ (href "http://scheme2016.snow-fort.org/"))
                           "Scheme Workshop")
                        ", "
                        (i "GNU Guix: The Functional GNU/Linux Distro That’s
a Scheme Library")
                        " (Ludovic Courtès): "
                        (a (@ (href ,(slides-url
                                      "guix-scheme-workshop-20160918.pdf")))
                           "slides"))
                    (li "August 2016, "
			(a (@ (href ,(gnu-url "ghm/program.html")))
			   "GNU Hackers Meeting")
			(ul (li (i "GNU Guix is 4 years old!")
                                " (Ludovic Courtès): "
				(a (@ (href ,(slides-url "guix-ghm-20160818.pdf")))
				   "slides"))
                            (li (i "Using Guix and Emacs in perfect harmony")
                                " (Ludovic Courtès): "
                                " (demo)")
			    (li (i "Navigating the Guix subsystems")
                                " (Ludovic Courtès): "
				(a (@ (href ,(slides-url "guix-ghm-20160819.pdf")))
				   "slides"))))
                    (li "May 2016, DConf: "
                        (a (@ (href ,(slides-url
                                      "guix-dconf-20160505.pdf")))
                           "slides"))
                    (li "March 2016, "
                        (a (@ (href "https://libreplanet.org/2016/"))
                           "LibrePlanet")
                        ", " (i "Solving the Deployment Crisis with GNU Guix")
                        " (Christopher Allan Webber and David Thompson): "
                        (a (@ (href
                               ,(slides-url "guix-libreplanet-solving-the-deployment-crisis-20160319.pdf")))
                           "slides")
                        ", "
                        (a (@ (href
                               "https://media.libreplanet.org/mgoblin_media/media_entries/1419/LP_2016_03_19_Webber_-_Thompson_Solving_the_deployment_crisis_with_GNU_STREAM.webm"))
                           "video")
                        " (WebM; 44 minutes)")
                    (li "January 2016, "
                        (a (@ (href "https://fosdem.org/2016/")) "FOSDEM")
                        (ul
                         (li (i "Adding GNU/Hurd support to GNU Guix")
                             " (Manolis Ragkousis): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-hurd-20160130.pdf")))
                                "slides"))
                         (li (i "A gentle introduction to functional package
management with GNU Guix")
                             " (Ricardo Wurmus): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-intro-20160130.pdf")))
                                "slides")
                             ", "
                             (a (@ (href "https://audio-video.gnu.org/video/misc/2016-01__GNU_Guix__Gentle_Introduction_to_Functional_Package_Management.webm"))
                                "partial video")
                             " (WebM; 7 minutes) ")
                         (li (i "Your distro is a Scheme library")
                             " (Ludovic Courtès): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-scheme-20160130.pdf")))
                                "slides")
                             ", "
                             (a (@ (href "https://audio-video.gnu.org/video/misc/2016-01__GNU_Guix__Your_Distro_is_a_Scheme_Library.webm"))
                                "video")
                             " (WebM; 29 minutes) ")
                         (li (i "Foreign packages in GNU Guix")
                             " (Pjotr Prins): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-ruby-20160130.pdf")))
                                "slides")
                             ", "
                             (a (@ (href "https://audio-video.gnu.org/video/misc/2016-01__GNU_Guix__Foreign_Packages_in_GNU_Guix.webm"))
                                "video")
                             " (WebM; 19 minutes) ")
                         (li (i "Reproducible and Customizable Deployments
with GNU Guix")
                             " (Ludovic Courtès): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-deployments-20160130.pdf")))
                                "slides")
                             ", "
                             (a (@ (href "https://audio-video.gnu.org/video/misc/2016-01__GNU_Guix__Reproducible_and_Customizable_Deployments.webm"))
                                "video")
                             " (WebM; 54 minutes) ")
                         (li (i "Guix-tox, a functional version of tox")
                             " (Cyril Roelandt): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-tox-20160130.pdf")))
                                "slides"))
                         (li (i "Reproducible and User-Controlled Package
Management in HPC with GNU Guix")
                             " (Ricardo Wurmus): "
                             (a (@ (href ,(slides-url
                                           "guix-fosdem-hpc-20160131.pdf")))
                                "slides")
                             ", "
                             (a (@ (href "https://audio-video.gnu.org/video/misc/2016-01__GNU_Guix__Reproducible_and_User-controlled_Package_Management_in_HPC.webm"))
                                "video")
                             " (WebM; 5 minutes) ")))
                    (li "January 2016, Boston Linux & Unix User Group: "
                        (a (@ (href ,(slides-url "guix-blu-20160120.pdf")))
                           "slides"))
                    (li "November 2015, Inria: "
                        (a (@ (href ,(slides-url
                                      "guix-rennes-20151109.pdf")))
                           "slides") ", "
                        (a (@ (href
                               "https://gnunet.org/sites/default/files/ludo2015guix.webm"))
                           "video")
                        " (WebM; 73 minutes)")
                    (li "August 2015, "
                        (a (@ (href "http://reppar.org"))
                           "Workshop on Reproducibility in Parallel Computing
 (RepPar)")
                        ": "
                        (a (@ (href ,(slides-url
                                      "guix-reppar-20150825.pdf")))
                           "slides"))
                    (li "May 2015, "
                        (a (@ (href "http://opentechsummit.net/en.html"))
                           "OpenTechSummit")
                        ": "
                        (a (@ (href ,(slides-url "guix-opentechsummit-201505014.pdf")))
                           "slides"))
                    (li "February 2015, "
			(a (@ (href "https://fosdem.org/2015/schedule/event/the_emacs_of_distros/"))
			   "FOSDEM")
			": "
			(a (@ (href ,(slides-url "guix-fosdem-20150131.pdf")))
			   "slides")
			", "
			(a (@ (href "https://audio-video.gnu.org/video/misc/2015-01__GNU_Guix__The_Emacs_of_Distros.webm"))
			   "video")
			" (WebM; 47 minutes) ")
		    (li "August 2014, "
			(a (@ (href "https://audio-video.gnu.org/video/ghm2014/"))
			   "GNU Hackers Meeting")
			": "
			(a (@ (href ,(slides-url "guix-ghm-20140815.pdf")))
			   "slides")
			", "
			(a (@ (href "https://audio-video.gnu.org/video/ghm2014/2014-08--courtes--were-building-the-gnu-system--ghm.webm"))
			   "video")
			" (WebM; 60 minutes) ")
		    (li "July 2014, "
			(a (@ (href "https://www.open-bio.org/wiki/Codefest_2014"))
			   "Open Bioinformatics Codefest 2014")
			": "
			(a (@ (href ,(slides-url "guix-openbio-codefest-20140709.pdf")))
			   "slides"))
		    (li "February 2014, "
			(a (@ (href "https://fosdem.org/2014/schedule/event/gnuguix/"))
			   "FOSDEM")
			": "
			(a (@ (href ,(slides-url "guix-fosdem-20140201.pdf")))
			   "slides")
			", "
			(a (@ (href "https://video.fosdem.org/2014/H1302_Depage/Sunday/Growing_a_GNU_with_Guix.webm"))
			   "video")
			" (WebM; 55 minutes) ")
		    (li "August 2013, "
			(a (@ (href ,(gnu-url "ghm/2013/paris")))
			   "GNU Hackers Meeting")
			(ul (li (i "GNU Guix: Package without a
scheme!")
				", by Andreas: "
				(a (@ (href ,(slides-url "guix-ghm-andreas-20130823.pdf")))
				   "slides"))
			    (li (i "Guix, the Computing Freedom
Deployment Tool")
				", by Ludovic: "
				(a (@ (href ,(slides-url "guix-ghm-ludo-20130823.pdf")))
				   "slides")
				", "
				(a (@ (href "http://audio-video.gnu.org/video/ghm2013/Ludovic_Courtes-GNU_Guix_the_computing_freedom_deployment_tool_.webm"))
				   "video")
				" (WebM; 60 minutes, 127MB) ")))
		    (li "June 2013, "
			(a (@ (href "https://www-sop.inria.fr/members/Manuel.Serrano/conferences/els13.html"))
			   " European Lisp Symposium (ELS)")
			": "
			(a (@ (href ,(slides-url "guix-els-20130603.pdf")))
			   "slides")
			", "
			(a (@ (href "http://www.nicklevine.org/els2013/ludovic-courtes.mp3"))
			   "audio"))
		    (li "July 2012, "
			(a (@ (href ,(gnu-url "ghm/2012/ddorf")))
			   "GNU Hackers Meeting")
			": "
			(a (@ (href ,(slides-url "guix-ghm-20120721.pdf")))
			   "slides")
			", "
			(a (@ (href "https://audio-video.gnu.org/video/ghm2012/guix.ogv"))
			   "video")
			" (Ogg/"
			(a (@ (href "http://theora.org/")) "Theora")
			"; 84 minutes, 88.1MB)"))))
	  ,(html-page-footer))))
