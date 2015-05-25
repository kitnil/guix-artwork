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

(define-module (www utils)
  #:export (current-url-root
	    gnu.org-root

	    base-url
	    gnu-url
	    guix-url
	    static-base-url
	    css-url
	    image-url
	    thumb-url
	    screenshot-url
	    slides-url))


;;;
;;; URL variables.
;;;

(define current-url-root
  ;; Website local url prefix.
  (make-parameter "/software/guix"))

(define gnu.org-root
  ;; GNU's website url prefix.
  (make-parameter ""))


;;;
;;; URL linking.
;;;

(define (base-url location)
  (string-append (current-url-root) "/" location))

(define (gnu-url location)
  (string-append (gnu.org-root) "/" location))

(define (guix-url location)
  (string-append (gnu-url "software/guix/") location))

(define (static-base-url)
  (base-url "static/base/"))

(define (css-url file)
  (string-append (static-base-url) "css/" file))

(define (image-url file)
  (string-append (static-base-url) "img/" file))

(define (thumb-url file)
  (string-append (image-url "screenshots/thumbs/") file))

(define (screenshot-url version file)
  (string-append (guix-url "screenshots/") version "/" file))

(define (slides-url file)
  (guix-url file))
