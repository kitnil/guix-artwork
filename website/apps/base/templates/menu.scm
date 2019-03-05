;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates menu)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base utils)
  #:export (menu-t))


(define (menu-t)
  "Return the Menu page in SHTML."
  (theme
   #:title '("Menu")
   #:description "Website menu."
   #:keywords '("GNU" "Linux" "Unix" "Free software" "Libre software"
		"Operating system" "GNU Hurd" "GNU Guix package manager"
		"GNU Guile" "Guile Scheme" "Transactional upgrades"
		"Functional package management" "Reproducibility")
   #:active-menu-item "Menu"
   #:css (list (guix-url "static/base/css/menu.css"))))
