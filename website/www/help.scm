(define-module (www help)
  #:export (help-page))

(define help-page
  '(html (@ (lang "en"))
	 (head (meta (@ (charset "utf-8")))
	       (meta (@ (name "author")
			(content "GuixSD Contributors")))
	       (meta (@ (name "description")
			(content
			 "GuixSD is GNU's advanced system
distribution. GNU is an operating system which respects the freedom of
computer users. You are free to run the system for any purpose, study
how it works, improve it, and share it with the whole world.")))
	       (meta (@ (name "keywords")
			(content
			 "GNU, FSF, Free Software Foundation, Linux,
Emacs, GCC, Unix, Free Software, Libre Software, Operating System, GNU
Kernel, GNU Hurd, GUix Package Manager, Guile Scheme, Functional
package management")))
	       (meta (@ (name "viewport")
			(content "width=device-width, initial-scale=1.0")))
	       (link (@ (type "text/css")
			(rel "stylesheet")
			(href "/software/guix/static/base/css/base.css")))
	       (link (@ (type "text/css")
			(rel "stylesheet")
			(href "/software/guix/static/base/css/help.css")))
	       (link (@ (type "image/png")
			(rel "icon")
			(href "/software/guix/static/base/img/favicon.png")))
	       (link (@ (rel "license") (href "Pending...")))
	       (title "Help - GuixSD"))
	 (body (div (@ (class "message-box msg-info"))
		    (span (@ (class "msg-label")) "Note")
		    "The Guix System Distribution (GuixSD) is alpha
software. This means it is not production-ready. It may contain bugs
and lack important features. But more than a disclaimer, this is an
invitation to join us in improving it. See "
		    (a (@ (href "/software/guix/contribute/"))
		       "Contributing")
		    ", for more information. We hope you can soon
switch to GuixSD without fear. ")
	       (div (@ (id "header-box"))
		    (a (@ (id "logo") (href "/software/guix/"))
		       (img (@ (src "/software/guix/static/base/img/GuixSD-logo.png")
			       (alt "GuixSD"))))
		    (ul (@ (id "site-nav"))
			(li (a (@ (href "/software/guix/download/"))
			       "Download"))
			(li (a (@ (href "/software/guix/package-list.html"))
			       "Packages"))
			(li (a (@ (href "/software/guix/help/")) "Help"))
			(li (a (@ (href "/software/guix/contribute/"))
			       "Contribute"))
			(li (a (@ (href "/software/guix/donate/"))
			       "Donate"))
			(li (a (@ (href "/software/guix/about/"))
			       "About"))))
	       (div (@ (id "content-box"))
		    (article
		     (h1 "Help")
		     (div (@ (class "text-center"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/guixsd-manual-icon.png")
					    (alt ""))))
			       (h2 "GuixSD Manual")
			       (p "The documentation about the Guix
System Distribution is available online as part of the GNU Guix
package manager manual.")
			       (a (@ (href "/software/guix/manual/guix.html#GNU-Distribution")
				     (class "hlink-more-dark"))
				  "Read the manual"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/guix-manual-icon.png")
					    (alt ""))))
			       (h2 "GNU Guix Manual")
			       (p "Documentation for the GNU Guix
package manager is available online. You may also find more
information about Guix by running "
				  (em "info\xa0guix")
				  ".")
			       (a (@ (href "/software/guix/manual/")
				     (class "hlink-more-dark"))
				  "Read the manual"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/library-icon.png")
					    (alt ""))))
			       (h2 "GNU Manuals")
			       (p "GuixSD is a distribution of the "
				  (a (@ (href "http://www.gnu.org/"))
				     "GNU operating system")
				  ". Most GNU software is documented
and the documentation is available online in various formats. ")
			       (a (@ (href "http://www.gnu.org/doc/doc.en.html")
				     (class "hlink-more-dark"))
				  "Browse the manuals"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/chat-icon.png")
					    (alt ""))))
			       (h2 "IRC Chat")
			       (p "For real-time support from the
community, you can connect to the "
				  (em "#guix")
				  " channel on irc.freenode.net. There
you can get help about anything related to both the Guix System
Distribution and GNU Guix.")
			       (p "The "
				  (em "#guix")
				  " channel is logged. Previous
conversations can be browsed online. See the "
				  (a (@ (href "https://gnunet.org/bot/log/guix/"))
				     "channel logs")
				  ". ")
			       (a (@ (href "http://webchat.freenode.net/?channels=%23guix")
				     (class "hlink-more-dark"))
				  "Connect"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/email-icon.png")
					    (alt ""))))
			       (h2 "Mailing lists")
			       (p "Email support from the community is
also available through the following mailing lists. The messages sent
to the lists are public and archived online.")
			       (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
				     (b "guix-devel"))
				  (small " ("
					 (a (@ (href "https://lists.gnu.org/archive/html/guix-devel"))
					    "archive")
					 ") ")
				  (br)
				  "Discussion about the development of
GNU Guix and the Guix System Distribution (GuixSD).")
			       (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
				     (b "bug-guix"))
				  (small " ("
					 (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
					    "archive")
					 ") ")
				  (br)
				  "Bug reports for GNU Guix and the
Guix System Distribution. ")
			       (a (@ (href "https://savannah.gnu.org/mail/?group=guix")
				     (class "hlink-more-dark"))
				  "Check all the lists")))
		     (h2 "Additional Documentation")
		     (ul (li (a (@ (href "http://arxiv.org/abs/1305.4584"))
				(i "Functional Package Management with Guix"))
			     ", presented at the "
			     (a (@ (href "http://www-sop.inria.fr/members/Manuel.Serrano/conferences/els13.html"))
				"2013 European Lisp Symposium (ELS)")
			     ", describes the rationale, design, and
implementation of Guix's packaging API. \t "))
		     (h2 (@ (id "talks")) "Talks")
		     (ul (li "February 2015, "
			     (a (@ (href "https://fosdem.org/2015/schedule/event/the_emacs_of_distros/"))
				"FOSDEM")
			     ": "
			     (a (@ (href "guix-fosdem-20150131.pdf"))
				"slides")
			     ", "
			     (a (@ (href "http://audio-video.gnu.org/video/misc/2015-01__GNU_Guix__The_Emacs_of_Distros.webm"))
				"video")
			     " (WebM; 47 minutes) ")
			 (li "August 2014, "
			     (a (@ (href "http://audio-video.gnu.org/video/ghm2014/"))
				"GNU Hackers Meeting")
			     ": "
			     (a (@ (href "guix-ghm-20140815.pdf"))
				"slides")
			     ", "
			     (a (@ (href "http://audio-video.gnu.org/video/ghm2014/2014-08--courtes--were-building-the-gnu-system--ghm.webm"))
				"video")
			     " (WebM; 60 minutes) ")
			 (li "July 2014, "
			     (a (@ (href "http://www.open-bio.org/wiki/Codefest_2014"))
				"Open Bioinformatics Codefest 2014")
			     ": "
			     (a (@ (href "guix-openbio-codefest-20140709.pdf"))
				"slides"))
			 (li "February 2014, "
			     (a (@ (href "https://fosdem.org/2014/schedule/event/gnuguix/"))
				"FOSDEM")
			     ": "
			     (a (@ (href "guix-fosdem-20140201.pdf"))
				"slides")
			     ", "
			     (a (@ (href "http://video.fosdem.org/2014/H1302_Depage/Sunday/Growing_a_GNU_with_Guix.webm"))
				"video")
			     " (WebM; 55 minutes) ")
			 (li "August 2013, "
			     (a (@ (href "/ghm/2013/paris"))
				"GNU Hackers Meeting")
			     (ul (li (i "GNU Guix: Package without a
scheme!")
				     ", by Andreas: "
				     (a (@ (href "guix-ghm-andreas-20130823.pdf"))
					"slides"))
				 (li (i "Guix, the Computing Freedom
Deployment Tool")
				     ", by Ludovic: "
				     (a (@ (href "guix-ghm-ludo-20130823.pdf"))
					"slides")
				     ", "
				     (a (@ (href "http://audio-video.gnu.org/video/ghm2013/Ludovic_Courtes-GNU_Guix_the_computing_freedom_deployment_tool_.webm"))
					"video")
				     " (WebM; 60 minutes, 127MB) ")))
			 (li "June 2013, "
			     (a (@ (href "http://www-sop.inria.fr/members/Manuel.Serrano/conferences/els13.html"))
				" European Lisp Symposium (ELS)")
			     ": "
			     (a (@ (href "guix-els-20130603.pdf"))
				"slides")
			     ", "
			     (a (@ (href "http://www.nicklevine.org/els2013/ludovic-courtes.mp3"))
				"audio"))
			 (li "July 2012, "
			     (a (@ (href "/ghm/2012/ddorf/"))
				"GNU Hackers Meeting")
			     ": "
			     (a (@ (href "guix-ghm-20120721.pdf"))
				"slides")
			     ", "
			     (a (@ (href "http://audio-video.gnu.org/video/ghm2012/guix.ogv"))
				"video")
			     " (Ogg/"
			     (a (@ (href "http://theora.org/")) "Theora")
			     "; 84 minutes, 88.1MB)"))))
	       (div (@ (id "footer-box"))
		    "copyleft 2015 GuixSD "
		    (a (@ (href "/software/guix/contribute/")
			  (class "hlink-yellow"))
		       "Contributors")
		    ". Made with "
		    (span (@ (class "metta")) "♥")
		    " by humans."))))
