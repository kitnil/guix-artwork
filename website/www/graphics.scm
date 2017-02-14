;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2016 Luis Felipe López Acevedo <felipe.lopez@openmailbox.org>
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

(define-module (www graphics)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (graphics-page))

(define (graphics-page)
  `(html (@ (lang "en"))
         ,(html-page-header "Graphics")
         (body
	  ,(html-page-description)
          ,(html-page-links)

          (div (@ (id "content-box"))
               (article
                (h1 "Graphics")
                (p "For questions regarding the graphics listed in this page, "
		   "please contact "
                   (a (@ (href "https://lists.gnu.org/mailman/listinfo/help-guix"))
                      ("help-guix@gnu.org"))
                   ".")
		(p
		 (@ (class "text-center"))
		 (img (@ (src ,(base-url "static/base/img/Guix.png"))
			 (alt "GNU Guix logotype")))
		 (img (@ (src ,(base-url "static/base/img/GuixSD.png"))
			 (alt "Guix System Distribution logotype"))))
                (p "The GNU Guix and the Guix System Distribution (GuixSD) "
                   "logotypes were designed by Luis Felipe López Acevedo. "
		   "They are available under the following terms:")
		(blockquote
		 (p "Copyright © 2015 Luis Felipe López Acevedo <felipe.lopez@openmailbox.org>")
		 (p "Permission is granted to copy, distribute and/or modify "
		    "this work under the terms of the "
		    (a (@ (href "http://creativecommons.org/licenses/by-sa/4.0/"))
		       "Creative Commons Attribution-ShareAlike 4.0 International License")
		    "."))
		(p "The source files for these logotypes, their variants, and "
		   "other artwork used in the different components of the GNU "
		   "Guix project are available in the "
		   (a (@ (href "//git.savannah.gnu.org/cgit/guix/guix-artwork.git"))
		      "guix-artwork")
		   " repository, including the previous GNU Guix logotype "
		   "designed by Nikita Karetnikov in 2013 and "
		   (a (@ (href "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25205"))
		      "superseded")
		   " by the golden GNU in 2016.")))

          ,(html-page-footer))))
