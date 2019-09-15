;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages templates detailed-index)
  #:use-module (apps aux web)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:use-module (apps packages templates components)
  #:use-module (srfi srfi-19)
  #:export (detailed-index-t))


(define* (detailed-index-t context #:optional total)
  "Return SHTML index page for the package app.  TOTAL is the total number of
packages to advertise."
  (let ((packages (context-datum context "packages")))
    (theme
     #:title (C_ "webpage title" (list "Packages"))
     #:description
     (G_ "List of packages available through GNU Guix.")
     #:keywords
     (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
     #:active-menu-item (C_ "website menu" "Packages")
     #:css
     (list (guix-url "static/base/css/page.css")
	   (guix-url "static/base/css/item-preview.css")
	   (guix-url "static/packages/css/letter-selector.css")
	   (guix-url "static/packages/css/package-list.css"))
     #:crumbs
     (list (crumb (C_ "website menu" "Packages") (guix-url "packages/")))
     #:content
     `(main
       (section
	(@ (class "page centered-text"))
        ,(G_ `(h2 "Packages"))

        ,(G_
          `(p
            (@ (class "limit-width centered-block"))
            "GNU Guix provides " ,(number* (or total (length packages)))
            " packages transparently "
            ,(G_
              `(a (@ (href "https://hydra.gnu.org/jobset/gnu/master#tabs-status"))
                  "available as pre-built binaries"))
            ". These pages provide a complete list of the packages.  Our "
            ,(G_
              `(a (@ (href "https://hydra.gnu.org/jobset/gnu/master"))
                  "continuous integration system"))
            " shows their current build status "
            "(updated " ,(date->string (current-date)
                                       (C_ "SRFI-19 date->string format"
                                           "~B ~e, ~Y")) ")."))

	(div
	 (@ (class "sheet sheet-padded justify-left"))
	 ,(letter-selector)
	 ,@(map detailed-package-preview packages)
	 ,(letter-selector)))))))
