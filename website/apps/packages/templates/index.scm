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
  #:export (index-t))


(define (index-t context)
  "Return an SHTML representation of the index page."
  (theme
   #:title (list "Packages")
   #:description
   "List of packages available for the Guix System Distribution
   (GuixSD) and foreign GNU/Linux distributions through the GNU
   Guix package manager."
   #:keywords
   (list "GNU" "Linux" "Unix" "Free software" "Libre software"
	 "Operating system" "GNU Hurd" "GNU Guix package manager"
	 "GNU Guile" "Guile Scheme" "Transactional upgrades"
	 "Functional package management" "Reproducibility")
   #:active-menu-item "Packages"
   #:css
   (list (guix-url "static/base/css/page.css")
	 (guix-url "static/base/css/item-preview.css")
	 (guix-url "static/base/css/sidebar.css"))
   #:crumbs
   (list (crumb "Packages" (guix-url "packages/")))
   #:content
   `(main
     (section
      (@ (class "page centered-text"))
      (h2 "Packages")

      (div
       (@ (class "sheet"))
       ,@(map package-preview (context-datum context "packages")))

      ,(sidebar)))))
