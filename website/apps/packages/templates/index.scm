;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates index)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps packages templates components)
  #:use-module (srfi srfi-19)
  #:export (index-t))


(define (index-t context)
  "Return an SHTML representation of the index page."
  (let ((packages (context-datum context "packages"))
	(total    (context-datum context "total")))
    (theme
     #:title (list "Packages")
     #:description
     "List of packages available through GNU Guix."
     #:keywords
     (list "GNU" "Linux" "Unix" "Free software" "Libre software"
	   "Operating system" "GNU Hurd" "GNU Guix package manager"
	   "GNU Guile" "Guile Scheme" "Transactional upgrades"
	   "Functional package management" "Reproducibility")
     #:active-menu-item "Packages"
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/item-preview.css")
	   (guix-url "static/packages/css/letter-selector.css"))
     #:crumbs
     (list (crumb "Packages" (guix-url "packages/")))
     #:content
     `(main
       (section
	(@ (class "page centered-text"))
	(h2 "Packages")

	(p
	 (@ (class "limit-width centered-block"))
	 "GNU Guix provides " ,(number* total) " packages transparently "
	 (a (@ (href "https://hydra.gnu.org/jobset/gnu/master#tabs-status"))
	    "available as pre-built binaries")
	 ". These pages provide a complete list of the packages.  Our "
	 (a (@ (href "https://hydra.gnu.org/jobset/gnu/master"))
	    "continuous integration system")
	 " shows their current build status "
	 "(updated " ,(date->string (current-date) "~B ~e, ~Y") ").")

	(div
	 (@ (class "sheet"))
	 ,(letter-selector)
	 ,@(map package-preview packages)
	 ,(letter-selector)))))))
