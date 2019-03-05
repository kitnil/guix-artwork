;;; GNU Guix web site
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2013 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Initially written by sirgazil who waives all copyright interest on this
;;; file.
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.


(define-module (apps packages data)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:export (all-packages
	    alphabet))


(define alphabet
  (list "0-9" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
	"N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))


(define %package-list
  (delay
    ;; Note: Dismiss packages found in $GUIX_PACKAGE_PATH.
    (let ((packages
           (sort (parameterize ((%package-module-path (last-pair
                                                       (%package-module-path))))
                   (fold-packages (lambda (package lst)
                                    (if (package-superseded package)
                                        lst
                                        (cons (or (package-replacement package)
                                                  package)
                                              lst)))
                                  '()))
                 (lambda (p1 p2)
                   (string<? (package-name p1)
                             (package-name p2))))))
      (cond ((null? packages) '())
            ((getenv "GUIX_WEB_SITE_LOCAL") (list-head packages 300))
            (else packages)))))

(define (all-packages)
  "Return the list of all Guix package objects, sorted by name.

   If GUIX_WEB_SITE_LOCAL=yes, return only 300 packages for
   testing the website."
  (force %package-list))
