;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates detailed-package-list)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps packages templates components)
  #:export (detailed-package-list-t))


(define (detailed-package-list-t context)
  "Return an SHTML page listing the packages in the CONTEXT."
  (let ((letter (context-datum context "letter"))
	(page-number
	    (number->string (context-datum context "page-number")))
	(total-pages
	 (number->string (context-datum context "total-pages"))))
    (theme
     #:title (list (string-append "Page " page-number) letter "Packages")
     #:description
     "List of packages available through GNU Guix."
     #:keywords
     '("GNU" "Linux" "Unix" "Free software" "Libre software"
       "Operating system" "GNU Hurd" "GNU Guix package manager"
       "GNU Guile" "Guile Scheme" "Transactional upgrades"
       "Functional package management" "Reproducibility")
     #:active-menu-item "Packages"
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/item-preview.css")
	   (guix-url "static/packages/css/letter-selector.css")
	   (guix-url "static/packages/css/package-list.css"))
     #:scripts
     (list (guix-url "static/packages/js/build-status.js"))
     #:crumbs
     (list (crumb "Packages" (guix-url "packages/"))
	   (crumb letter (guix-url (url-path-join "packages"
						  letter
						  "")))
	   (crumb (string-append "Page " page-number)
		  (guix-url (url-path-join "packages"
					   "page"
					   page-number
					   ""))))
     #:content
     `(main
       (section
	(@ (class "page centered-text"))
	(h2 "Packages — " ,letter
	    ,(page-indicator (string->number page-number)
			     (string->number total-pages)))

	(div
	 (@ (class "sheet sheet-padded justify-left"))
	 ,(letter-selector letter)
	 ,@(map detailed-package-preview (context-datum context "items"))
	 ,(letter-selector letter)
	 ,(page-selector (string->number total-pages)
			 (string->number page-number)
			 (guix-url (url-path-join "packages" letter)))))))))
