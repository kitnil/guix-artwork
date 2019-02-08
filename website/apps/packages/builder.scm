;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;;
;;; Initially written by sirgazil
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
   (if (getenv "GUIX_WEB_SITE_INFO")
       (list
        (index-builder)
        (packages-builder)
        (package-list-builder))

       ;; These provisional builders are used because of a limitation of
       ;; the CVS repository used for deploying the website. The idea is
       ;; to have "package list" and "package detail" pages as proposed
       ;; in Guix bug #25227. This, however, would generate thousands of
       ;; pages that could choke the current CVS repository.
       ;;
       ;; When this limitation is gone, the above builders should be
       ;; used instead. They should generate pages as those described
       ;; in the proposal.
       (list
        (detailed-index-builder)
        (detailed-package-list-builder)))))



;;;
;;; Helper builders.
;;;

(define %max-packages-on-index
  ;; Maximum number of packages shown on /packages.
  30)

(define (index-builder)
  "Return a Haunt page listing some random packages."
  ;; TODO: Pick random packages.
  (let ((context (list (cons "packages"
                             (take-at-most (all-packages)
                                           %max-packages-on-index)))))
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
