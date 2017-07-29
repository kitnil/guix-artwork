;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates screenshot)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (screenshot-t))


(define (screenshot-t context)
  "Return an SHTML page for the screenshot in the CONTEXT."
  (let ((shot (context-datum context "screenshot"))
	(shots (context-datum context "screenshots")))
    (theme
     #:title (list (screenshot-title shot) "Screenshots")
     #:description (screenshot-caption shot)
     #:keywords
     '("GNU" "Linux" "Unix" "Free software" "Libre software"
       "Operating system" "GNU Hurd" "GNU Guix package manager"
       "GNU Guile" "Guile Scheme" "Transactional upgrades"
       "Functional package management" "Reproducibility")
     #:active-menu-item "Overview"
     #:css (list (guix-url "static/base/css/index.css"))
     #:content
     `(main
       (section
	(@ (class "light-text centered-text noise-bg"))
	(h2
	 (@ (class "a11y-offset"))
	 ,(screenshot-title shot))

	(img
	 (@ (class "responsive-image")
	    (src ,(screenshot-image shot))
	    (alt ,(screenshot-caption shot))))

	(div
	 (@ (class "screenshots-box top-shadow-bg"))
	 ,@(map screenshot->shtml shots)))))))
