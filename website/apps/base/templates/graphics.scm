;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps base templates graphics)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:export (graphics-t))


(define (graphics-t)
  "Return the Graphics page in SHTML."
  (theme
   #:title '("Graphics")
   #:description
   "Information about images used for the graphical identity of
   GNU Guix and Guix System (formerly “GuixSD”)."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Branding" "Logo")
   #:active-menu-item "About"
   #:css (list
	  (guix-url "static/base/css/page.css"))
   #:crumbs (list (crumb "Graphics" "./"))
   #:content
   `(main
     (section
      (@ (class "page centered-block limit-width"))
      (h2 "Graphics")

      (p
       "For questions regarding the graphics listed in this page,
       please contact "
       (a (@ (href "https://lists.gnu.org/mailman/listinfo/help-guix"))
	  ("help-guix@gnu.org"))
       ".")
      (p
       (@ (class "centered-text"))
       (img (@ (src ,(guix-url "static/base/img/Guix.png"))
	       (alt "GNU Guix logotype"))))
      (p
       "The standalone Guix, formerly known as the “Guix System
       Distribution” or GuixSD, had its own logo, which is now
       deprecated.")

      (p
       "The GNU Guix and GuixSD
       logotypes were designed by Luis Felipe López Acevedo
       (a.k.a. sirgazil).  They are available under the following
       terms:")
      (blockquote
       (p "Copyright © 2015 Luis Felipe López Acevedo")
       (p
	"Permission is granted to copy, distribute and/or modify this
        work under the terms of the "
	(a (@ (href "https://creativecommons.org/licenses/by-sa/4.0/"))
	   "Creative Commons Attribution-ShareAlike 4.0 International License")
	"."))
      (p
       "The source files (SVG) for these logotypes, their variants, and
       other artwork used in the different components of the GNU Guix
       project are available in the "
       (a (@ (href "//git.savannah.gnu.org/cgit/guix/guix-artwork.git/tree/logo"))
	  "guix-artwork")
       " repository, including the previous GNU Guix logotype designed
       by Nikita Karetnikov in 2013 and "
       (a (@ (href "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25205"))
	  "superseded")
       " by the golden GNU in 2016.")))))
