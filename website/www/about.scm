(define-module (www about)
  #:export (about-page))

(define about-page
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
	       (title "About - GuixSD"))
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
		     (h1 "About the Project")
		     (p "The "
			(em "Guix System Distribution (GuixSD)")
			" and the "
			(em "GNU Guix")
			" package manager are free software projects
developed by the "
			(a (@ (href "http://www.gnu.org/"))
			   "GNU Project")
			" and independent volunteers from various
parts of the world. This is the official website for both projects. ")
		     (blockquote
		      (p (strong
			  "Free software means the users have the
freedom to run, copy, distribute, study, change and improve the
software."))
		      (p "Free software is a matter of liberty, not
price. To understand the concept, you should think of \"free\" as in
\"free speech\", not as in \"free beer\". ")
		      (p "More precisely, free software means users of
a program have the "
			 (a (@ (href "/philosophy/free-sw.html"))
			    "four essential freedoms")
			 ":")
		      (ul (li "The freedom to run the program as you
wish, for any purpose (freedom 0).")
			  (li "The freedom to study how the program
works, and adapt it to your needs (freedom 1). Access to the source
code is a precondition for this.")
			  (li "The freedom to redistribute copies so
you can help your neighbor (freedom 2).")
			  (li "The freedom to improve the program, and
release your improvements to the public, so that the whole community
benefits (freedom 3). Access to the source code is a precondition for
this."))
		      (p "\xa0gnu.org"))
		     (h2 (@ (id "mantainer")) "Maintainer")
		     (p "Guix is currently being maintained by Ludovic
Courtès. Please use the "
			(a (@ (href "#contact")) "mailing lists")
			" for contact. ")
		     (h2 (@ (id "license")) "Licensing")
		     (p "Guix is free software; you can redistribute
it and/or modify it under the terms of the "
			(a (@ (rel "license") (href "/licenses/gpl.html"))
			   "GNU General Public License")
			" as published by the Free Software Foundation; either version\xa03 of the License, or (at your option) any later version. ")
		     (h2 (@ (id "contact")) "Contact")
		     (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-devel"))
			   (b "guix-devel"))
			(small " ("
			       (a (@ (href "https://lists.gnu.org/archive/html/guix-devel"))
				  "archive")
			       ")")
			(br)
			"Discussion about the development of GNU Guix
and the Guix System Distribution (GuixSD). "
			(a (@ (href "http://lists.gnu.org/archive/html/bug-guix/2013-07/msg00039.html"))
			   " Until July 2013")
			", the bug-Guix mailing list filled that
role. ")
		     (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/bug-guix"))
			   (b "bug-guix"))
			(small " ("
			       (a (@ (href "https://lists.gnu.org/archive/html/bug-guix"))
				  "archive")
			       ")")
			(br)
			" Bug reports for GNU Guix and the Guix System Distribution. ")
		     (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/gnu-system-discuss"))
			   (b "gnu-system-discuss"))
			(small " ("
			       (a (@ (href "http://lists.gnu.org/archive/html/gnu-system-discuss/"))
				  "archive")
			       ") ")
			(br)
			"Discussion about the development of the
broader GNU system. ")
		     (p (a (@ (href "https://lists.nongnu.org/mailman/listinfo/gnu-linux-libre"))
			   (b "gnu-linux-libre"))
			(small " ("
			       (a (@ (href "http://lists.nongnu.org/archive/html/gnu-linux-libre/"))
				  "archive")
			       ") ")
			(br)
			"Workgroup for fully free GNU/Linux
distributions.")
		     (dl (dt "Commit notifications")
			 (dd (p (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-commits"))
				   "Guix-commits")
				" receives notifications of commits to the "
				(a (@ (href "#contribute"))
				   "version control repository")
				"."))
			 (dt "Announcements")
			 (dd (p "Announcements about Guix and most
other GNU software are made on "
				(a (@ (href "http://lists.gnu.org/mailman/listinfo/info-gnu"))
				   "info-gnu")
				" ("
				(a (@ (href "http://lists.gnu.org/archive/html/info-gnu/"))
				   "archive")
				")."))
			 (dt "Security reports")
			 (dd (p "Security reports that should not be
made immediately public can be sent directly to the maintainer.  If
there is no response to an urgent issue, you can escalate to the
general "
				(a (@ (href "http://lists.gnu.org/mailman/listinfo/security"))
				   "security")
				" mailing list for advice."))
			 (dt "Internet relay chat")
			 (dd (p "Some Guix users and developers hang
out on the "
				(em "#guix")
				" channel of the Freenode IRC
network ("
				(a (@ (href "https://gnunet.org/bot/log/guix/"))
				   "logs")
				").")))))
	       (div (@ (id "footer-box"))
		    "copyleft 2015 GuixSD "
		    (a (@ (href "/software/guix/contribute/")
			  (class "hlink-yellow"))
		       "Contributors")
		    ". Made with "
		    (span (@ (class "metta")) "♥")
		    " by humans."))))
