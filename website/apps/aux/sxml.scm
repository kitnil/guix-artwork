;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Initially written by sirgazil who waives all copyright interest on
;;; this file.
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

(define-module (apps aux sxml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (sxml->string*))


(define (sxml->string* tree)
  "Flatten tree by dismissing tags and attributes, and return the resulting
string."
  (define (sxml->strings tree)
    (match tree
      (((? symbol?) ('@ _ ...) body ...)
       (append-map sxml->strings body))
      (((? symbol?) body ...)
       (append-map sxml->strings body))
      ((? string?)
       (list tree))
      ((lst ...)
       (sxml->strings `(div ,@lst)))))

  (string-concatenate (sxml->strings tree)))
