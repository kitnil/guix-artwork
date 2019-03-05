;;; GNU Guix web site
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

(define-module (apps aux web)
  #:use-module (srfi srfi-1)
  #:export (slugify
	    url-path-join))


;;;
;;; Variables.
;;;

(define char-set:slug
  (char-set-union char-set:letter+digit (char-set #\-)))



;;;
;;; Procedures.
;;;

(define (slugify text)
  "Return TEXT as a slug.

   Reserved characters for Internationalized Resource Identifiers
   (IRIs) and common reserved characters for file names are removed
   using the SLUG_FORBIDDEN constant as reference.

   TEXT (string)
     Some text. For example: Biology, Human anatomy.

   RETURN VALUE (string)
     A slug-like string. For example: biology, human-anatomy."
  (string-join
   (map (lambda (s) (string-filter char-set:slug s))
	(string-split (string-downcase text) char-set:whitespace))
   "-"))


(define (url-path-join . parts)
  "Return a URL path composed of the given PARTS.

   PARTS (strings)
     A succession of strings that represent parts of a URL path.

     To indicate an absolute path, use an empty string as the first
     part. For example:

     (url-path-join '' 'docs' 'manual')
     => '/docs/manual'

     To end the path with a slash, use an empty string as the last
     part. For example:

     (url-path-join '' 'docs' 'manual' '')
     => '/docs/manual/'

   RETURN VALUE (string)
     A string representing a URL path."
  (cond ((equal? parts '("")) "/") ; Root directory
	(else (string-join parts "/"))))
