;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

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
    ;; TODO: Remove these builders when the bug described below is fixed.
    (detailed-index-builder)
    (detailed-package-list-builder)
    ;; -----------------------------------------------------------------

    ;; BUG: These builders are commented out because of a bug using dots
    ;; in directory names:
    ;;
    ;; https://bitbucket.org/sirgazil/guixsd-website/issues/47/
    ;;
    ;; However, they build the pages that implement the latest design
    ;; proposed for the website in Guix(SD) bug #26006.
    ;;
    ;;(index-builder)
    ;;(packages-builder)
    ;;(package-list-builder)
    )))



;;;
;;; Helper builders.
;;;

(define (index-builder)
  "Return a Haunt page listing some random packages."
  ;; TODO: Pass ~30 random Guix packages.
  (let ((context (list (cons "packages" (all-packages)))))
    (make-page "packages/index.html" (index-t context) sxml->html)))


(define (detailed-index-builder)
  "Return a Haunt page listing some random packages."
  ;; TODO: Pass ~30 random Guix packages.
  (let ((context (list (cons "packages" (all-packages)))))
    (make-page "packages/index.html" (detailed-index-t context) sxml->html)))


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
