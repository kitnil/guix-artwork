(define-module (www download)
  #:export (download-page))

(define download-page
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
			(href "/software/guix/static/base/css/download.css")))
	       (link (@ (type "image/png")
			(rel "icon")
			(href "/software/guix/static/base/img/favicon.png")))
	       (link (@ (rel "license") (href "Pending...")))
	       (title "Download - GuixSD"))
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
		     (h1 "Download")
		     (p "As of version 0.8.1, the Guix System
Distribution "
			(a (@ (href "/software/guix/manual/html_node/System-Installation.html"))
			   "can be installed")
			" on an i686 or x86_64 machine. It uses the "
			(a (@ (href "/software/linux-libre"))
			   "Linux-Libre")
			" kernel and the "
			(a (@ (href "/software/dmd")) "GNU dmd")
			" init system. Alternately, its package
manager, GNU Guix, can be installed as an additional package manager
on top of an installed Linux-based system.")
		     (div (@ (class "text-center"))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/GuixSD-package.png")
					    (alt ""))))
			       (h2 "GuixSD 0.8.2 (i686)")
			       (p "USB installer for machines with the
following minimum system requirements:")
			       (table (tbody (tr (th "Architecture")
						 (td "i686"))
					     (tr (th "Processor")
						 (td "1GHz"))
					     (tr (th "Memory")
						 (td "512MB"))
					     (tr (th "Hard Drive")
						 (td "5GB"))))
			       (p (@ (class "text-center"))
				  (a (@ (href "#")
					(class "hlink-yellow-boxed"))
				     "DOWNLOAD")
				  (br)
				  "(140MB approx.)"
				  (br)
				  (a (@ (href "#")) "Get signature"))
			       (p "See the "
				  (a (@ (href "/software/guix/manual/html_node/System-Installation.html"))
				     "installation instructions")
				  " from the manual.")
			       (p "Alternative download methods: "
				  (a (@ (href "#")) "torrent")
				  "."))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/GuixSD-package.png")
					    (alt ""))))
			       (h2 "GuixSD 0.8.2 (x86_64)")
			       (p "USB installer for machines with the
following minimum system requirements:")
			       (table (tbody (tr (th "Architecture")
						 (td "x86_64"))
					     (tr (th "Processor")
						 (td "1GHz"))
					     (tr (th "Memory")
						 (td "512MB"))
					     (tr (th "Hard Drive")
						 (td "5GB"))))
			       (p (@ (class "text-center"))
				  (a (@ (href "#")
					(class "hlink-yellow-boxed"))
				     "DOWNLOAD")
				  (br)
				  "(144MB approx.)"
				  (br)
				  (a (@ (href "#")) "Get signature"))
			       (p "See the "
				  (a (@ (href "/software/guix/manual/html_node/System-Installation.html"))
				     "installation instructions")
				  " from the manual.")
			       (p "Alternative download methods: "
				  (a (@ (href "#")) "torrent")
				  "."))
			  (div (@ (class "summary-box"))
			       (div (@ (class "text-center"))
				    (img (@ (src "/software/guix/static/base/img/Guix-package.png")
					    (alt ""))))
			       (h2 "GNU Guix 0.8.2")
			       (p "Archive distribution to install
from source on machines with the following minimum system
requirements:")
			       (table (tbody (tr (th "Architecture")
						 (td "i686, x86_64, mips64el, or armv7"))
					     (tr (th "Processor")
						 (td "1GHz"))
					     (tr (th "Memory")
						 (td "512MB"))
					     (tr (th "Hard Drive")
						 (td "5GB"))))
			       (p (@ (class "text-center"))
				  (a (@ (href "#")
					(class "hlink-yellow-boxed"))
				     "DOWNLOAD")
				  (br)
				  "(7.7MB approx.)"
				  (br)
				  (a (@ (href "#")) "Get signature"))
			       (p "See the "
				  (a (@ (href "/software/guix/manual/html_node/System-Installation.html"))
				     " installation instructions")
				  " from the manual.")
			       (p "Alternative download methods: "
				  (a (@ (href "#")) "torrent")
				  ". ")))
		     (p "Source code for the Guix System Distribution
USB installation images as well as GNU Guix can be found on the GNU
ftp server for "
			(em "alpha")
			" releases: "
			(a (@ (href "http://alpha.gnu.org/gnu/guix/"))
			   "http://alpha.gnu.org/gnu/guix/")
			" (via HTTP) and "
			(a (@ (href "ftp://alpha.gnu.org/gnu/guix/"))
			   "ftp://alpha.gnu.org/gnu/guix/")
			" (via FTP). ")))
	       (div (@ (id "footer-box"))
		    "copyleft 2015 GuixSD "
		    (a (@ (href "/software/guix/contribute/")
			  (class "hlink-yellow"))
		       "Contributors")
		    ". Made with "
		    (span (@ (class "metta")) "♥")
		    " by humans."))))
