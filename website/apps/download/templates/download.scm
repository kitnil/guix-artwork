;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps download templates download)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps download templates components)
  #:export (download-t))


(define (download-t context)
  "Return the Download page in SHTML."
  (theme
   #:title '("Download")
   #:description
   "Installers and source files for the Guix System distribution
   (GuixSD), and the GNU Guix package manager. GNU Guix can be
   installed on different GNU/Linux distributions."
   #:keywords
   '("GNU" "Linux" "Unix" "Free software" "Libre software"
     "Operating system" "GNU Hurd" "GNU Guix package manager"
     "Installer" "Source code" "Package manager")
   #:active-menu-item "Download"
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/download.css"))
   #:crumbs (list (crumb "Download" "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      (h2 "Download")

      (p
       (@ (class "centered-block limit-width"))
       "As of version " ,(latest-guix-version)
       ", the Guix System Distribution "
       (a
	(@ (href ,(manual-url "System-Installation.html")))
	"can be installed")
       " on an i686 or x86_64 machine. It uses the "
       (a (@ (href ,(gnu-url "software/linux-libre"))) "Linux-Libre")
       " kernel and the "
       (a (@ (href ,(gnu-url "software/shepherd"))) "GNU Shepherd")
       " init system. Alternately, its package manager, GNU Guix,
       can be installed as an additional package manager on top of an
       installed Linux-based system.")

      (div
       (@ (class "centered-text"))
       ,@(map download (context-datum context "downloads")))

      (p
       (@ (class "centered-block limit-width"))
       "Source code for the Guix System Distribution USB installation
       images as well as GNU Guix can be found on the GNU ftp server
       for " (em "alpha") " releases: "
       (a (@ (href "http://alpha.gnu.org/gnu/guix/"))
	  "http://alpha.gnu.org/gnu/guix/")
       " (via HTTP) and "
       (a (@ (href "ftp://alpha.gnu.org/gnu/guix/"))
	  "ftp://alpha.gnu.org/gnu/guix/")
       " (via FTP). ")))))
