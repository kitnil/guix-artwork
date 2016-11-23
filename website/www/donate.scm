;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
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

(define-module (www donate)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (donate-page))

(define fsf
  `(a (@ (href "https://www.fsf.org/")) "Free Software Foundation"))

(define (donate-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Donate")
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
and hosting.  "
                   "As the distribution grows (see the "
                   (a (@ (href ,(base-url "packages")))
                      "package list")
                   "), so do the computing and storage needs.")
                (p "We have "
                   (a (@ (href
                          "https://savannah.gnu.org/forum/forum.php?forum_id=8423"))
                      "started a fundraising campaign")
                   " to strengthen our build farm, with "
                   (a (@ (href
                          "https://www.fsf.org/blogs/community/fsf-announces-support-for-gnu-guix"))
                      "support from the Free Software Foundation (FSF)")
                   ".  Please consider helping out by making a donation on
this FSF-hosted page:")
                (p (a (@ (href "https://my.fsf.org/civicrm/contribute/transact?reset=1&id=50")
                         (class "button btn-blue"))
                      "♥ DONATE!"))

                (p "We are also looking for
donations of hardware and optionally hosting for the following kinds of
machines (they should be usable with exclusively free software): ")
		(ul (li "x86_64 machines, with on the order of 1\xa0TiB of
storage and 4\xa0GiB of RAM;")
		    (li "armv7 machines (such as the Novena) to more quickly
test and provide binaries for the armhf-linux port;")
		    (li "mips64el machines to strengthen this port."))
		(p "Please get in touch with us through the "
		   (a (@ (href ,(base-url "about/#contact")))
		      "usual channels")
		   " or using the " (b "guix-hardware@gnu.org")
                   " private alias to discuss any opportunities. ")
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
				  (td (ul (li ,fsf))))
                              (tr (td "bayfront.guixsd.org")
                                  (td "new build farm front-end (WIP)")
                                  (td (ul (li (a (@ (href
                                                     ,(base-url
                                                       "news/growing-our-build-farm.html")))
                                                 "Igalia")))))
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
			      (tr (td "librenote")
				  (td "mips64el-linux")
				  (td (ul (li "Daniel Clark (hardware)")
					  (li "Mark H Weaver (hosting)"))))
                              (tr (td "hydra-slave0")
				  (td "mips64el-linux")
				  (td (ul (li ,fsf))))
                              (tr (td "guix.sjd.se")
                                  (td "x86_64-linux, i686-linux")
                                  (td (ul (li (a (@ (href
                                                     "http://josefsson.org"))
                                                 "Simon Josefsson")))))
                              (tr (td "hydra-slave1")
                                  (td "armhf-linux")
                                  (td (ul (li "Steve Sprang (hardware)")
                                          ;; XXX: Eventually move to the FSF?
                                          (li "Mark H Weaver (hosting)"))))
                              (tr (td "hydra-slave2")
                                  (td "armhf-linux")
                                  (td (ul (li (a (@ (href "http://harmoninstruments.com/"))
                                                 "Harmon Instruments")
                                              " (hardware)")
                                          ;; XXX: Eventually move to the FSF?
                                          (li "Mark H Weaver (hosting)"))))
                              (tr (td "redhill")
                                  (td "armhf-linux")
                                  (td (ul (li (a (@ (href "http://www.kosagi.com/w/index.php?title=Novena_Main_Page"))
                                                 "Kosagi (Sutajio Ko-Usagi Pte Ltd)")
                                              " (hardware)")
                                          (li "Andreas Enge (hosting)"))))))))
	  ,(html-page-footer))))
