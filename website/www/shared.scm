;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (www shared)
  #:use-module (www utils)
  #:export (latest-guix-version
            html-page-header
	    html-page-description
	    html-page-links
	    html-page-footer))

(define latest-guix-version
  (make-parameter "0.8.2"))

(define* (html-page-header title #:key (css "article.css"))
  `(head (meta (@ (charset "utf-8")))
	 (meta (@ (name "author")
		  (content "GuixSD Contributors")))
	 (meta (@ (name "description")
		  (content
		   "GuixSD is GNU's advanced system distribution. GNU is an
operating system which respects the freedom of computer users. You are free to
run the system for any purpose, study how it works, improve it, and share it
with the whole world.")))
	 (meta (@ (name "keywords")
		  (content
		   "GNU, FSF, Free Software Foundation, Linux, Emacs, GCC,
Unix, Free Software, Libre Software, Operating System, GNU Kernel, GNU Hurd,
Guix Package Manager, Guile Scheme, Transactional upgrades,
Functional package management,")))
	 (meta (@ (name "viewport")
		  (content "width=device-width, initial-scale=1.0")))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href ,(css-url "base.css"))))
	 (link (@ (type "text/css")
		  (rel "stylesheet")
		  (href ,(css-url css))))
	 (link (@ (type "image/png")
		  (rel "icon")
		  (href ,(image-url "favicon.png"))))
	 (link (@ (rel "license") (href "Pending...")))
	 (title ,(string-append title " — GuixSD"))))

(define (html-page-description)
  `(div (@ (class "message-box msg-info"))
	(span (@ (class "msg-label")) "Note ")
	"The Guix System Distribution (GuixSD) is alpha software, "
        "which means it is "
        (a (@ (href ,(base-url "manual/html_node/System-Installation.html#Limitations")))
           "not production-ready")
        ".  But you can "
	(a (@ (href ,(base-url "contribute"))) "help") "!"))

(define (html-page-links)
  `(div (@ (id "header-box"))
	(a (@ (id "logo") (href ,(base-url "")))
	   (img (@ (src ,(image-url "GuixSD-logo.png"))
		   (alt "GuixSD"))))
	(ul (@ (id "site-nav"))
	    (li (a (@ (href ,(base-url "download"))) "Download"))
	    (li (a (@ (href ,(guix-url "package-list.html"))) "Packages"))
	    (li (a (@ (href ,(base-url "help"))) "Help"))
	    (li (a (@ (href ,(base-url "contribute"))) "Contribute"))
	    (li (a (@ (href ,(base-url "donate"))) "Donate"))
	    (li (a (@ (href ,(base-url "about"))) "About")))))

(define (html-page-footer)
  `(div (@ (id "footer-box"))
	"Made with " (span (@ (class "metta")) "♥")
	" by humans and powered by "
	(a (@ (href ,(gnu-url "software/guile")) (class "hlink-yellow"))
	   "GNU Guile") ".  "
        (a (@ (href "http://git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/website")
              (class "hlink-yellow"))
           "Source code")
        " under the "
	(a (@ (href ,(gnu-url "licenses/agpl-3.0.html")) (class "hlink-yellow"))
	   "GNU AGPL") "."))
