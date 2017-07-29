;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates donate)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (donate-t))


(define (donate-t)
  "Return the Donate page in SHTML."
  (theme
   #:title '("Donate")
   #:description
   "We are looking for donations of hardware and optionally hosting
   for machines (they should be usable with exclusively free
   software)."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Donations")
   #:active-menu-item "Donate"
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb "Donate" "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 "Donate")

      (p
       "The "
       (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
	  "build farm")
       " of the Guix System Distribution runs on donated hardware and"
       " hosting. As the distribution grows (see the "
       (a (@ (href ,(guix-url "packages/"))) "package list")
       "), so do the computing and storage needs.")

      (p
       "We have "
       (a (@ (href "https://savannah.gnu.org/forum/forum.php?forum_id=8423"))
	  "started a fundraising campaign")
       " to strengthen our build farm, with "
       (a (@ (href "https://www.fsf.org/blogs/community/fsf-announces-support-for-gnu-guix"))
	  "support from the Free Software Foundation (FSF)")
       ".  Please consider helping out by making a donation on this
       FSF-hosted page:")

      (p
       (@ (class "centered-text"))
       ,(button-big
	 #:label "♥ DONATE!"
	 #:url "https://my.fsf.org/civicrm/contribute/transact?reset=1&id=50"))

      (h3
       (@ (id "hardware-and-hosting"))
       "Hardware and Hosting")

      (p
       "We are also looking for donations of hardware and optionally
        hosting for the following kinds of machines (they should be
        usable with exclusively free software): ")

      (ul
       (li "x86_64 machines, with on the order of 1\xa0TiB of storage
            and 4\xa0GiB of RAM;")
       (li "armv7 machines (such as the Novena) to more quickly test
            and provide binaries for the armhf-linux port;")
       (li "mips64el machines to strengthen this port."))

      (p
       "Please get in touch with us through the "
       (a (@ (href ,(guix-url "contact/"))) "usual channels")
       " or using the " (b "guix-hardware@gnu.org") " private alias to
        discuss any opportunities. ")


      (h3
       (@ (id "hardware-donors"))
       "Thanks to the donors!")

      (p
       "The table below summarizes hardware and hosting donations that
        make the " (a (@ (href "http://hydra.gnu.org")) "build farm")
	" for the Guix System Distribution a reality.")

      (div
       (@ (class "table-box"))
       (table
	(thead
	 (tr (th "machine")
	     (th "system")
	     (th "donors")))
	(tbody
	 (tr
	  (td "hydra.gnu.org")
	  (td "build farm front-end")
	  (td
	   (ul
	    (li
	     (a (@ (href "https://www.fsf.org/"))
		"Free Software Foundation")))))
	 (tr
	  (td "bayfront.guixsd.org")
	  (td "new build farm front-end (WIP)")
	  (td
	   (ul
	    (li
	     (a (@ (href ,(guix-url "news/growing-our-build-farm.html")))
		"Igalia")))))
	 (tr
	  (td "hydra.gnunet.org")
	  (td "x86_64-linux, i686-linux")
	  (td (ul (li (a (@ (href "https://gnunet.org/fsnsg"))
			 "Free Secure Network Systems Group")
		      " at the "
		      (a (@ (href "https://www.tum.de/"))
			 "Technische Universität München")))))
	 (tr
	  (td "chapters.gnu.org")
	  (td "x86_64-linux, i686-linux")
	  (td
	   (ul
	    (li (a (@ (href "https://es.gnu.org"))
		   "GNU\xa0España") " (hardware)")
	    (li (a (@ (href "https://fsffrance.org/index.en.html"))
		   "FSF\xa0France")
		" (hosting)"))))
	 (tr
	  (td "librenote")
	  (td "mips64el-linux")
	  (td (ul (li "Daniel Clark (hardware)")
		  (li "Mark H Weaver (hosting)"))))
	 (tr
	  (td "hydra-slave0")
	  (td "mips64el-linux")
	  (td
	   (ul
	    (li (a (@ (href "https://www.fsf.org/"))
		   "Free Software Foundation")))))
	 (tr
	  (td "guix.sjd.se")
	  (td "x86_64-linux, i686-linux")
	  (td
	   (ul
	    (li (a (@ (href "http://josefsson.org"))
		   "Simon Josefsson")))))
	 (tr
	  (td "hydra-slave1")
	  (td "armhf-linux")
	  (td
	   (ul
	    (li "Steve Sprang (hardware)")
	    ;; XXX: Eventually move to the FSF?
	    (li "Mark H Weaver (hosting)"))))
	 (tr
	  (td "hydra-slave2")
	  (td "armhf-linux")
	  (td
	   (ul
	    (li (a (@ (href "http://harmoninstruments.com/"))
		   "Harmon Instruments")
		" (hardware)")
	    ;; XXX: Eventually move to the FSF?
	    (li "Mark H Weaver (hosting)"))))
	 (tr
	  (td "hydra-slave3")
	  (td "armhf-linux")
	  (td
	   (ul
	    (li (a (@ (href "http://www.kosagi.com/w/index.php?title=Novena_Main_Page"))
		   "Kosagi (Sutajio Ko-Usagi Pte Ltd)")
		" (hardware)")
	    (li "Mark H Weaver (hosting)"))))
	 (tr
	  (td "redhill")
	  (td "armhf-linux")
	  (td
	   (ul
	    (li (a (@ (href "http://www.kosagi.com/w/index.php?title=Novena_Main_Page"))
		   "Kosagi (Sutajio Ko-Usagi Pte Ltd)")
		" (hardware)")
	    (li "Andreas Enge (hosting)")))))))))))
