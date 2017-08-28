;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates package)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps packages templates components)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (guix gnu-maintenance)
  #:export (package-t))


(define (package-t context)
  "Return an SHTML representation of a package page."
  (let* ((package (context-datum context "package"))
	 (package-id (string-append (package-name package)
				    " "
				    (package-version package)))
	 (lint-issues (package-lint-issues package)))
    (theme
     #:title (list package-id "Packages")
     #:description (package-synopsis-shtml package)
     #:keywords
     '("GNU" "Linux" "Unix" "Free software" "Libre software"
       "Operating system" "GNU Hurd" "GNU Guix package manager"
       "GNU Guile" "Guile Scheme" "Transactional upgrades"
       "Functional package management" "Reproducibility")
     #:active-menu-item "Packages"
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/packages/css/package.css"))
     #:crumbs
     (list (crumb "Packages" (guix-url "packages/"))
	   (crumb package-id
		  (guix-url (package-url-path package))))
     #:content
     `(main
       (article
	(@ (class "page centered-block limit-width"))
	(h2 ,package-id " "
	    (span
	     (@ (class "synopsis"))
	     ,(package-synopsis-shtml package)))

	(p ,(if (gnu-package? package)
                '(it "This is a GNU package.  ")
                "")
           ,(package-description-shtml package))

	(ul
	 (@ (class "package-info"))
	 (li (b "Website: ")
	     (a (@ (href ,(package-home-page package)))
		,(package-home-page package)))
	 (li (b "License: "))
	 (li (b "Build status: "))
	 (li (b "Patches: "))
	 (li (b "Package source code: ")))

	;; Lint issues.
	,(if (null? lint-issues)
	     ""
	     `((h3 "Lint issues")
	       (p
		,(issue-count->shtml (length lint-issues)) ". "
		"See " (a (@ (href "#")) "package definition")
		" in Guix source code.")

	       ,@(map lint-issue->shtml lint-issues))))))))
