;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates menu)
  #:use-module (apps base templates components)
  #:use-module (apps base templates theme)
  #:use-module (apps base utils)
  #:use-module (apps i18n)
  #:export (menu-t))


(define (menu-t)
  "Return the Menu page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Menu"))
   #:description (G_ "Website menu.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|GNU Guile|Guile \
Scheme|Transactional upgrades|Functional package \
management|Reproducibility") #\|)
   #:active-menu-item (C_ "website menu" "Menu")
   #:css (list (guix-url "static/base/css/menu.css"))))
