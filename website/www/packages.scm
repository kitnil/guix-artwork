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

(define-module (www packages)
  #:use-module (www shared)
  #:export (packages-page))

(define (packages-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Packages")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Packages")
		(p "The Guix System Distribution provides 1,500+ packages
transparently "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master#tabs-status"))
		      "available as pre-built binaries")
		   ". This is a complete lists of the packages. Our "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
		      "continuous integration system")
		   " shows their current build status.")))
	  ,(html-page-footer))))
