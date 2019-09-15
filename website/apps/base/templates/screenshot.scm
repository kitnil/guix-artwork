;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates screenshot)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (screenshot-t))


(define (screenshot-t context)
  "Return an SHTML page for the screenshot in the CONTEXT."
  (let ((shot (context-datum context "screenshot"))
	(shots (context-datum context "screenshots")))
    (theme
     #:title (list (screenshot-title shot) (C_ "webpage title" "Screenshots"))
     #:description (screenshot-caption shot)
     #:keywords
     (string-split ;TRANSLATORS: |-separated list of webpage keywords
      (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
     #:active-menu-item (C_ "website menu" "Overview")
     #:css (list (guix-url "static/base/css/index.css")
		 (guix-url "static/base/css/screenshots.css"))
     #:content
     `(main
       (section
	(@ (class "light-text centered-text noise-bg"))
	(h2
	 (@ (class "a11y-offset"))
	 ,(screenshot-title shot))

	(div
	 (@ (class "screenshot-viewer"))
	 (img
	  (@ (class "responsive-image centered-block")
	     (src ,(screenshot-image shot))
	     (alt ,(screenshot-caption shot)))))

	(div
	 (@ (class "screenshots-box top-shadow-bg"))
	 ,@(map screenshot->shtml shots)))))))
