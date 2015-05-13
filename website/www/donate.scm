(define-module (www donate)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (donate-page))

(define (donate-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Home")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Donate")
		(p "The "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
		      "build farm")
		   " of the Guix System Distribution runs on donated hardware
and hosting. Please consider helping the project with donations. ")
		(p "As the distribution grows (see the "
		   (a (@ (href ,(base-url "packages")))
		      "package list")
		   "), so do the computing and storage needs. We are looking for
donations of hardware and optionally hosting for the following kinds of
machines (they should be usable with exclusively free software): ")
		(ul (li "x86_64 machines, with on the order of 1\xa0TiB of
storage and 4\xa0GiB of RAM;")
		    (li "armv7 machines that would allow us to provide pre-built
binaries for the "
			(a (@ (href "https://savannah.gnu.org/forum/forum.php?forum_id=8175"))
			   "recently-added ARM port")
			";")
		    (li "mips64el machines to strengthen this port."))
		(p "Please get in touch with us through the "
		   (a (@ (href ,(base-url "about/#contact")))
		      "usual channels")
		   " or using the guix-hardware@gnu.org private alias to discuss
any opportunities. ")
		(h2 (@ (id "hardware-donors")) "Thanks to the donors!")
		(p "The table below summarizes hardware and hosting donations
that make the "
		   (a (@ (href "http://hydra.gnu.org"))
		      "build farm")
		   " for the Guix System Distribution a
reality.")
		(table (thead (tr (th "machine")
				  (th "system")
				  (th "donors")))
		       (tbody (tr (td "hydra.gnu.org")
				  (td "build farm front-end")
				  (td (ul (li (a (@ (href "https://www.fsf.org/"))
						 "Free Software Foundation")))))
			      (tr (td "hydra.gnunet.org")
				  (td "x86_64-linux, i686-linux")
				  (td (ul (li (a (@ (href "https://gnunet.org/fsnsg"))
						 "Free Secure Network Systems
Group")
					      " at the "
					      (a (@ (href "https://www.tum.de/"))
						 "Technische Universität
München")))))
			      (tr (td "chapters.gnu.org")
				  (td "x86_64-linux, i686-linux")
				  (td (ul (li (a (@ (href "https://es.gnu.org"))
						 "GNU\xa0España")
					      " (hardware)")
					  (li (a (@ (href "https://fsffrance.org/index.en.html"))
						 "FSF\xa0France")
					      " (hosting)"))))
			      (tr (td "wildebeest")
				  (td "x86_64-linux, i686-linux")
				  (td (ul (li "anonymous"))))
			      (tr (td "librenote")
				  (td "mips64el-linux")
				  (td (ul (li "Daniel Clark (hardware)")
					  (li "Mark H. Weaver (hosting)"))))
                              (tr (td "hydra-slave0")
				  (td "mips64el-linux")
				  (td (ul (li (a (@ (href "https://www.fsf.org/"))
						 "Free Software Foundation")))))))))
	  ,(html-page-footer))))
