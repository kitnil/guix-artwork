;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates package)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps packages templates components)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix packages)
  #:export (package-t))


(define (package-t context)
  "Return an SHTML representation of a package page."
  (let* ((package (context-datum context "package"))
	 (package-id (string-append (package-name package)
				    " "
				    (package-version package)))
	 (lint-issues (package-lint-issues package)))
    (theme
     #:title (C_ "webpage title" (list package-id "Packages"))
     #:description (package-synopsis-shtml package)
     #:keywords
     (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
     #:active-menu-item (C_ "website menu" "Packages")
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/packages/css/package.css"))
     #:crumbs
     (list (crumb (C_ "website menu" "Packages") (guix-url "packages/"))
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

        ;; 'gnu-package?' might fetch stuff from the network.  Assume #f if
        ;; that doesn't work.
	(p ,(if (false-if-exception (gnu-package? package))
                (G_ '(it "This is a GNU package.  "))
                "")
           ,(package-description-shtml package))

	(ul
	 (@ (class "package-info"))
         ,(G_ `(li ,(G_ `(b "Website: "))
                   (a (@ (href ,(package-home-page package)))
                      ,(package-home-page package))))
         ,(G_ `(li ,(G_ `(b "License: "))
                   ,(license->shtml (package-license package))))
         ,(G_ `(li ,(G_ `(b "Package source: "))
                   ,(location->shtml (package-location package))))
         ,(G_ `(li ,(G_ `(b "Patches: "))
                   ,(patches->shtml (package-patches package))))
         ,(G_ `(li ,(G_ `(b "Builds: "))
                   ,(supported-systems->shtml package))))

	;; Lint issues.
	,(if (null? lint-issues)
	     ""
             (G_ `(,(G_ `(h3 "Lint issues"))
                   ,(G_
                     `(p
                       ""
                       ,(issue-count->shtml
                         (length lint-issues))
                       ". "
                       "See " ,(G_ `(a (@ (href "#")) "package definition"))
                       " in Guix source code."))

                   ,@(map lint-issue->shtml lint-issues)))))))))
