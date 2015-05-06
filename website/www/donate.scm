(define-module (www donate)
  #:export (donate))

(define donate
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
			(href "/software/guix/static/base/css/article.css")))
	       (link (@ (type "image/png")
			(rel "icon")
			(href "/software/guix/static/base/img/favicon.png")))
	       (link (@ (rel "license") (href "Pending...")))
	       (title "Donate - GuixSD"))
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
		     (h1 "Donate")
		     (p "The "
			(a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
			   "build farm")
			" of the Guix System Distribution runs on
donated hardware and hosting. Please consider helping the project with
donations. ")
		     (p "As the distribution grows (see the "
			(a (@ (href "/software/guix/packages/"))
			   "package list")
			"), so do the computing and storage needs. We
are looking for donations of hardware and optionally hosting for the
following kinds of machines (they should be usable with exclusively
free software): ")
		     (ul (li "x86_64 machines, with on the order of
1\xa0TiB of storage and 4\xa0GiB of RAM;")
			 (li "armv7 machines that would allow us to
provide pre-built binaries for the "
			     (a (@ (href "https://savannah.gnu.org/forum/forum.php?forum_id=8175"))
				"recently-added ARM port")
			     ";")
			 (li "mips64el machines to strengthen this port."))
		     (p "Please get in touch with us through the "
			(a (@ (href "/software/guix/about/#contact"))
			   "usual channels")
			"or using the guix-hardware@gnu.org private
alias to discuss any opportunities. ")
		     (h2 (@ (id "hardware-donors"))
			 "Thanks to the donors!")
		     (p "The table below summarizes hardware and
hosting donations that make the "
			(a (@ (href "http://hydra.gnu.org"))
			   "build farm")
			" for the Guix System Distribution a
reality.")
		     (table (thead (tr (th "machine")
				       (th "system")
				       (th "donors")))
			    (tbody (tr (td "hydra.gnu.org")
				       (td "build farm front-end")
				       (td (ul (li (a (@ (href "http://www.fsf.org/"))
						      "Free Software \t          Foundation")))))
				   (tr (td "hydra.gnunet.org")
				       (td "x86_64-linux, i686-linux")
				       (td (ul (li (a (@ (href "https://gnunet.org/fsnsg"))
						      "Free Secure
Network Systems Group")
						   " at the "
						   (a (@ (href "http://www.tum.de/"))
						      "Technische
Universität München")))))
				   (tr (td "chapters.gnu.org")
				       (td "x86_64-linux, i686-linux")
				       (td (ul (li (a (@ (href "http://es.gnu.org"))
						      "GNU\xa0España")
						   " (hardware)")
					       (li (a (@ (href "http://fsffrance.org/index.en.html"))
						      "FSF\xa0France")
						   " (hosting)"))))
				   (tr (td "wildebeest")
				       (td "x86_64-linux, i686-linux")
				       (td (ul (li "anonymous"))))
				   (tr (td "librenote")
				       (td "mips64el-linux")
				       (td (ul (li "Daniel Clark (hardware)")
					       (li "Mark H. Weaver (hosting)"))))))))
	       (div (@ (id "footer-box"))
		    "copyleft 2015 GuixSD "
		    (a (@ (href "/software/guix/contribute/")
			  (class "hlink-yellow"))
		       "Contributors")
		    ". Made with "
		    (span (@ (class "metta")) "♥")
		    " by humans."))))
