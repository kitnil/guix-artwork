(define-module (www)
  #:use-module (www shared)
  #:use-module (www packages)
  #:use-module (www download)
  #:use-module (www donate)
  #:use-module (www about)
  #:use-module (www contribute)
  #:use-module (www help)
  #:use-module (sxml simple)
  #:use-module (ice-9 match)
  #:export (main-page

            %web-pages
            export-web-page
            export-web-site))

(define main-page
  `(html (@ (lang "en"))
	 ,(html-page-header "Home")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (div (@ (id "featured-box"))
		    (div (@ (class "featured-content"))
			 (h1 (@ (class "featured-heading"))
			     "The Guix System Distribution")
			 (ul (li (b "Liberating.")
				 " GuixSD is an advanced distribution of the "
				 (a (@ (href "https://gnu.org/")
				       (class "hlink-yellow"))
				    "GNU Operating System")
				 " developed by the "
				 (a (@ (href "https://gnu.org/")
				       (class "hlink-yellow"))
				    "GNU Project ")
				 "—which respects the "
				 (a (@ (href "http://www.gnu.org/philosophy/free-sw.html")
				       (class "hlink-yellow"))
				    "freedom of computer users")
				 ". ")
			     (li (b "Dependable.")
				 " The "
				 (a (@ (href "/software/guix/manual/")
				       (class "hlink-yellow"))
				    "GNU Guix")
				 " Package Manager, in addition to standard
package management features, supports transactional upgrades and roll-backs,
unprivileged package management, per-user profiles, and garbage collection.")
			     (li (b "Hackable.")
				 " It provides "
				 (a (@ (href "https://www.gnu.org/s/guile/")
				       (class "hlink-yellow"))
				    "Guile Scheme")
				 " APIs, including high-level embedded
domain-specific languages (EDSLs), to describe how packages are built and
composed."))
			 (div (@ (class "featured-actions"))
			      (a (@ (href "/software/guix/download/")
				    (class "action download"))
				 "TEST v0.8.2 (alpha)")
			      (a (@ (href "/software/guix/contribute/")
				    (class "action contribute"))
				 "CONTRIBUTE"))))
	       (div (@ (id "discovery-box"))
		    (h2 "Discover GuixSD")
		    (div (@ (class "info-box text-center"))
			 (video (@ (src "http://audio-video.gnu.org/video/misc/2015-01__GNU_Guix__The_Emacs_of_Distros.webm")
				   (poster
				    "/software/guix/static/base/img/the-emacs-of-distros.png")
				   (controls "controls")
				   (class "video-preview")))
			 (p "January 2015, The Emacs of Distros (48 minutes)")
			 (p (a (@ (href "/software/guix/help/#talks")
				  (class "hlink-more-light"))
			       "Check all talks")))
		    (div (@ (class "info-box text-left"))
			 (p (a (@ (href "/software/guix/manual/")
				  (class "hlink-yellow"))
			       "GNU Guix Documentation")
			    (br)
			    "You may also find more information about GNU Guix
by running info guix.")
			 (p (a (@ (href "http://arxiv.org/abs/1305.4584")
				  (class "hlink-yellow"))
			       "Functional Package Management with Guix")
			    (br)
			    "A paper presented at the 2013 European Lisp
Symposium (ELS), describes the rationale, design, and implementation of Guix's
packaging API. ")
			 (p (a (@ (href "http://www.gnu.org/manual/")
				  (class "hlink-yellow"))
			       "GNU Manuals Online")
			    (br)
			    "Primary documentation for official GNU packages.")
			 (p (a (@ (href "/software/guix/help/")
				  (class "hlink-more-light"))
			       "Find more documentation")))
		    (img (@ (src "/software/guix/static/base/img/h-separator-darker.png")
			    (class "h-separator")
			    (alt "")))
		    (div (@ (id "screens-box"))
			 (a (@ (href "/software/guix/screenshots/0.8.2/grub-menu.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/grub-menu-thumb.png")
				    (class "screenshot-thumb")
				    (alt "GRUB menu"))))
			 (a (@ (href "/software/guix/screenshots/0.8.2/slim.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/slim-thumb.png")
				    (class "screenshot-thumb")
				    (alt "Slim login manager"))))
			 (a (@ (href "/software/guix/screenshots/0.8.2/windowmaker+icecat+inkscape.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/windowmaker+icecat+inkscape-thumb.png")
				    (class "screenshot-thumb")
				    (alt "Windowmaker, Icecat, and Inkscape"))))
			 (a (@ (href "/software/guix/screenshots/0.8.2/user-interfaces.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/user-interfaces-thumb.png")
				    (class "screenshot-thumb")
				    (alt "mplayer and xterm"))))
			 (a (@ (href "/software/guix/screenshots/0.8.2/emacs-ui-packages.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/emacs-ui-packages-thumb.png")
				    (class "screenshot-thumb")
				    (alt "Emacs user interface to the package manager."))))
			 (a (@ (href "/software/guix/screenshots/0.8.2/emacs-ui-generations.png"))
			    (img (@ (src "/software/guix/static/base/img/screenshots/emacs-ui-generations-thumb.png")
				    (class "screenshot-thumb")
				    (alt "Emacs user interface generations.")))))
		    (p (a (@ (href "/software/guix/contribute/")
			     (class "hlink-yellow-boxed"))
			  "Help us package more software →")))
	       (div (@ (id "news-box"))
		    (h2 "News")
		    (a (@ (href "http://www.fsf.org/news/fsf-adds-guix-system-distribution-to-list-of-endorsed-distributions")
			  (class "news-entry"))
		       (h4 "FSF adds Guix System Distribution to list of
endorsed distributions")
		       (p (@ (class "news-date")) "February 3, 2015")
		       (p (@ (class "news-summary"))
			  "The Guix System Distribution is a new and growing
distro that currently ships with just over 1000 packages, already including
almost all of the programs available from the GNU Project..."))
		    (a (@ (href "https://savannah.gnu.org/forum/forum.php?forum_id=8193")
			  (class "news-entry"))
		       (h4 "GNU Guix 0.8.1 Released")
		       (p (@ (class "news-date")) "January 29, 2015")
		       (p (@ (class "news-summary"))
			  "We are pleased to announce the next alpha release of
GNU Guix, version 0.8.1. The release comes both with a source tarball, which
allows you to install it on top of a running GNU/Linux system, and a USB
installation image to install the standalone Guix System..."))
		    (a (@ (href "https://savannah.gnu.org/forum/forum.php?forum_id=8191")
			  (class "news-entry"))
		       (h4 "GNU Guix at FOSDEM")
		       (p (@ (class "news-date")) "January 27, 2015")
		       (p (@ (class "news-summary"))
			  "Guix will be present at FOSDEM in Brussels, Belgium,
with a talk entitled \"The Emacs of Distros\" this Saturday, at 3PM, in room
H.1302. The talk will give an update on developments in Guix and the Guix System
Distribution since last year..."))
		    (p (a (@ (href "https://savannah.gnu.org/news/?group=guix")
			     (class "hlink-more-dark"))
			  "More news")))
	       (div (@ (id "contact-box"))
		    (h2 "Contact")
		    (div (@ (class "info-box text-justify"))
			 (h3 "IRC Channel")
			 (p "Some Guix users and developers hang out on the
#guix channel of the Freenode IRC network. "
			    (small "(See "
				   (a (@ (href "https://gnunet.org/bot/log/guix/"))
				      "channel logs")
				   ")")
			    ".")
			 (p (@ (class "text-right"))
			    (a (@ (href "http://webchat.freenode.net/?channels=%23guix")
				  (class "button btn-blue"))
			       "Connect"))
			 (h3 "Report Bugs")
			 (p "Use the bugs mailing list to report bugs. Please
check whether the bug is already in the "
			    (a (@ (href "http://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=guix"))
			       "bug database")
			    ".")
			 (p (@ (class "text-right"))
			    (a (@ (href "mailto:bug-guix@gnu.org")
				  (class "button btn-red"))
			       "Report")))
		    (div (@ (class "info-box text-left"))
			 (h3 "Mailing Lists")
			 (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
			       (b "guix-devel"))
			    (small " ("
				   (a (@ (href "https://lists.gnu.org/archive/html/guix-devel"))
				      "archive")
				   ")")
			    (br)
			    "Discussion about the development of GNU Guix and
the Guix System Distribution (GuixSD).")
			 (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
			       (b "bug-guix"))
			    (small " ("
				   (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
				      "archive")
				   ")")
			    (br)
			    "Bug reports for GNU Guix and the Guix System
Distribution.")
			 (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/gnu-system-discuss"))
			       (b "gnu-system-discuss"))
			    (small " ( "
				   (a (@ (href "http://lists.gnu.org/archive/html/gnu-system-discuss/"))
				      "archive")
				   ")")
			    (br)
			    "Discussion about the development of
the broader GNU system.")
			 (p (a (@ (href "https://lists.nongnu.org/mailman/listinfo/gnu-linux-libre"))
			       (b "gnu-linux-libre"))
			    (small " ("
				   (a (@ (href "http://lists.nongnu.org/archive/html/gnu-linux-libre/"))
				      "archive")
				   ")")
			    (br)
			    "Workgroup for fully free GNU/Linux distributions.")
			 (p (a (@ (href "https://savannah.gnu.org/mail/?group=guix")
				  (class "hlink-more-dark"))
			       "Find all the available lists")))))
	  ,(html-page-footer))))


;;;
;;; HTML export.
;;;

(define %web-pages
  ;; Mapping of web pages to HTML file names.
  `(("index.html" ,main-page)
    ("about/index.html" ,about-page)
    ("contribute/index.html" ,contribute-page)
    ("donate/index.html" ,donate-page)
    ("download/index.html" ,download-page)
    ("help/index.html" ,help-page)
    ("packages/index.html" ,packages-page)))

(define (mkdir* directory)
  "Make DIRECTORY unless it already exists."
  (catch 'system-error
    (lambda ()
      (mkdir directory))
    (lambda args
      (unless (= EEXIST (system-error-errno args))
        (apply throw args)))))

(define (export-web-page page file)
  "Export PAGE, an SXML tree, to FILE."
  (mkdir* (dirname file))
  (call-with-output-file file
    (lambda (port)
      (sxml->xml page port))))

(define* (export-web-site #:optional (directory "."))
  "Export the whole web site as HTML files created in DIRECTORY."
  (for-each (match-lambda
              ((filename page)
               (export-web-page page (string-append directory
						    file-name-separator-string
						    filename))))
            %web-pages))
