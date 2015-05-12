(define-module (www help)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (help-page))

(define help-page
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
			  (a (@ (href ,(guix-url "manual/guix.html#GNU-Distribution"))
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
			  (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
				(b "guix-devel"))
			     (small " ("
				    (a (@ (href "https://lists.gnu.org/archive/html/guix-devel"))
				       "archive")
				    ") ")
			     (br)
			     "Discussion about the development of GNU Guix and
the Guix System Distribution (GuixSD).")
			  (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
				(b "bug-guix"))
			     (small " ("
				    (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
				       "archive")
				    ") ")
			     (br)
			     "Bug reports for GNU Guix and the Guix System
Distribution. ")
			  (a (@ (href "https://savannah.gnu.org/mail/?group=guix")
				(class "hlink-more-dark"))
			     "Check all the lists")))
		(h2 "Additional Documentation")
		(ul (li (a (@ (href "https://arxiv.org/abs/1305.4584"))
			   (i "Functional Package Management with Guix"))
			", presented at the "
			(a (@ (href "https://www-sop.inria.fr/members/Manuel.Serrano/conferences/els13.html"))
			   "2013 European Lisp Symposium (ELS)")
			", describes the rationale, design, and
implementation of Guix's packaging API. \t "))
		(h2 (@ (id "talks")) "Talks")
		(ul (li "February 2015, "
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
			(a (@ (href "guix-openbio-codefest-20140709.pdf"))
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
