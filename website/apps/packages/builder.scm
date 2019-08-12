;;; GNU Guix web site
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Nicolò Balzarotti <nicolo@nixo.xyz>
;;;
;;; Initially written by sirgazil
;;; who waives all copyright interest on this file.
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

(define-module (apps packages builder)
  #:use-module (apps aux lists)
  #:use-module (apps aux system)
  #:use-module (apps base utils)
  #:use-module (apps packages data)
  #:use-module (apps packages templates detailed-index)
  #:use-module (apps packages templates index)
  #:use-module (apps packages templates detailed-package-list)
  #:use-module (apps packages templates package)
  #:use-module (apps packages templates package-list)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (haunt html)
  #:use-module (haunt page)
  #:use-module (haunt utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (json)
  #:export (builder))


;;;
;;; Application builder.
;;;

(define (builder site posts)
  "Return the list of web resources that compose the app.

   This procedure is a Haunt builder procedure.

   SITE (<site>)
     A site object that defines all the properties of the website. See
     Haunt <site> objects for more information.

   POSTS (list of <post>)
     A list of post objects that represent articles from the blog. See
     Haunt <post> objects for more information.

   RETURN (list of <page>)
     A list of page objects that represent the web resources of the
     application. See Haunt <page> objects for more information."
  (flatten
   (list
    (index-builder)
    (packages-json-builder)
    (packages-builder)
    (package-list-builder))))



;;;
;;; Helper builders.
;;;

(define %max-packages-on-index
  ;; Maximum number of packages shown on /packages.
  30)

(define (packages-json-builder)
  "Return a JSON page listing all packages."
  (define (package->json package)
    `(("name"     . ,(package-name package))
      ("version"  . ,(package-version package))
      ("synopsis" . ,(package-synopsis package))
      ("homepage" . ,(package-home-page package))))
  (make-page "packages.json"
	         (list->vector (map package->json (all-packages)))
             (lambda args
               (apply scm->json (append args '(#:pretty #t))))))

(define (index-builder)
  "Return a Haunt page listing some random packages."
  (define (sample n from)
    (map (lambda (id) (list-ref from id))
         (list-tabulate n (lambda _ (random (length from))))))
  (let ((context (list (cons "packages"
                             (sample %max-packages-on-index
                                     (all-packages)))
		       (cons "total"
			     (length (all-packages))))))
    (make-page "packages/index.html" (index-t context) sxml->html)))


(define (detailed-index-builder)
  "Return a Haunt page listing some random packages."
  ;; TODO: Pass ~30 random Guix packages.
  (let ((context (list (cons "packages"
                             (take-at-most (all-packages)
                                           %max-packages-on-index)))))
    (make-page "packages/index.html"
               (detailed-index-t context (length (all-packages)))
               sxml->html)))


(define (detailed-package-list-builder)
  "Return a list of grouped Haunt pages listing Guix packages.

   Each group is a list of page objects corresponding to paginated
   packages starting with a specific letter."
  (let ((package-groups (packages/group-by-letter (all-packages))))
    (map
     (lambda (package-group)
       (let* ((letter (car package-group))
	      (context
	       (list
		(cons "letter" letter))))
	 (paginate #:dataset (cdr package-group)
		   #:limit 100
		   #:base-path (path-join "packages" letter)
		   #:template detailed-package-list-t
		   #:context context
		   #:writer sxml->html)))
     package-groups)))


(define (packages-builder)
  "Return a list of Haunt pages for each Guix package."
  (map
   (lambda (package)
     (let ((context (list (cons "package" package))))
       (make-page
	(path-join (package-url-path package) "index.html")
	(package-t context)
	sxml->html)))
   (all-packages)))


(define (package-list-builder)
  "Return a list of grouped Haunt pages listing Guix packages.

   Each group is a list of page objects corresponding to paginated
   packages starting with a specific letter."
  (let ((package-groups (packages/group-by-letter (all-packages))))
    (map
     (lambda (package-group)
       (let* ((letter (car package-group))
	      (context
	       (list
		(cons "letter" letter))))
	 (paginate #:dataset (cdr package-group)
		   #:limit 100
		   #:base-path (path-join "packages" letter)
		   #:template package-list-t
		   #:context context
		   #:writer sxml->html)))
     package-groups)))
