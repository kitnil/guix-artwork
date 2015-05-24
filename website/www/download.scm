(define-module (www download)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (download-page))

(define %usb-image-description
  "USB installer of the Guix System Distribution.")

(define %usb-image-manual
  "manual/html_node/System-Installation.html")

(define %guixsd-image
  "GuixSD-package.png")

(define %binary-tarball-description
  "Self-contained tarball providing binaries for Guix and for all its
dependencies.")

(define %binary-tarball-manual
  "manual/html_node/Binary-Installation.html")

(define %source-tarball-description
  "Source code distribution.")

(define %source-tarball-manual
  "manual/html_node/Requirements.html")

(define %guix-image
  "Guix-package.png")

(define (ftp-url file)
  (string-append "ftp://alpha.gnu.org/gnu/guix/" file))

(define (guixsd-files archs)
  (map (lambda (arch)
         (cons arch (ftp-url (string-append "guixsd-usb-install-"
                                            (latest-guix-version) "." arch
                                            "-linux.xz"))))
       archs))

(define (guix-files archs)
  (map (lambda (arch)
         (cons arch (ftp-url (string-append "guix-binary-" (latest-guix-version)
                                            "." arch "-linux.tar.xz"))))
       archs))

(define (guix-source-files variants)
  (map (lambda (variant)
         (cons variant (ftp-url (string-append "guix-" (latest-guix-version)
                                               ".tar.gz"))))
       variants))

(define* (download-box title
                       #:key files description image manual)
  `(div (@ (class "download-box"))
        (img (@ (src ,(image-url image)) (alt "")))
        (h2 ,title)
        (p ,description)
        (p "Download options:")
        ,(map (lambda (file)
                `(a (@ (href ,(cdr file)) (class "hlink-download"))
                   ,file))
              files)
        (p "Signatures: "
           ,(map (lambda (file)
                   `(a (@ (href ,(string-append (cdr file) ".sig"))
                          (class "hlink-signature"))
                      ,(string-append (car file))))
             files))
        (p "See the "
           (a (@ (href ,(guix-url manual)))
              "installation instructions")
           " from the manual.")))

(define (download-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Download" #:css "download.css")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Download")
		(p "As of version " ,(latest-guix-version)
                   ", the Guix System Distribution "
		   (a (@ (href ,(guix-url "manual/html_node/System-Installation.html")))
		      "can be installed")
		   " on an i686 or x86_64 machine. It uses the "
		   (a (@ (href ,(gnu-url "software/linux-libre")))
		      "Linux-Libre")
		   " kernel and the "
		   (a (@ (href ,(gnu-url "software/dmd"))) "GNU dmd")
		   " init system. Alternately, its package manager, GNU Guix,
can be installed as an additional package manager on top of an installed
Linux-based system.")

		(div (@ (class "text-center"))
             ,(download-box (string-append "GuixSD " (latest-guix-version))
                            #:files (guixsd-files '("x86_64" "i686"))
                            #:description %usb-image-description
                            #:manual %usb-image-manual
                            #:image %guixsd-image)
             ,(download-box (string-append "GNU Guix " (latest-guix-version)
                                           " Binary")
                            #:files (guix-files '("x86_64" "i686" "mips64el"
                                                  "armhf"))
                            #:description %binary-tarball-description
                            #:manual %binary-tarball-manual
                            #:image %guix-image)
             ,(download-box (string-append "GNU Guix " (latest-guix-version)
                                           " Source")
                            #:files (guix-source-files '("tarball"))
                            #:description %source-tarball-description
                            #:manual %source-tarball-manual
                            #:image %guix-image))

		(p "Source code for the Guix System Distribution USB
installation images as well as GNU Guix can be found on the GNU ftp server for "
		   (em "alpha")
		   " releases: "
		   (a (@ (href "http://alpha.gnu.org/gnu/guix/"))
		      "http://alpha.gnu.org/gnu/guix/")
		   " (via HTTP) and "
		   (a (@ (href "ftp://alpha.gnu.org/gnu/guix/"))
		      "ftp://alpha.gnu.org/gnu/guix/")
		   " (via FTP). ")))
	  ,(html-page-footer))))
