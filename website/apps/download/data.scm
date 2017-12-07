;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps download data)
  #:use-module (apps base utils)
  #:use-module (apps download types)
  #:export (system-downloads))


;;;
;;; Data.
;;;


(define system-downloads
  (list
   (download
    #:title (string-append "GuixSD " (latest-guix-version))
    #:description
    `(div
      (div
       (@ (class "message-box msg-info"))
       (span (@ (class "msg-label")) "Note ")
       "The Guix System Distribution (GuixSD) is beta software, "
       "which means it is "
       (a
	(@ (href ,(manual-url "Limitations.html")))
	"not production-ready")
       ".  But you can "
       (a (@ (href ,(guix-url "contribute"))) "help") "!")
      (p "USB installer of the Guix System Distribution."))
    #:image (guix-url "static/base/img/GuixSD-package.png")
    #:base-url (string-append "https://alpha.gnu.org/gnu/guix/guixsd-usb-install-"
			      (latest-guix-version) ".")
    #:variants (list (variant "x86_64" "x86_64-linux.xz")
		     (variant "i686" "i686-linux.xz"))
    #:manual (manual-url "System-Installation.html"))

   (download
    #:title (string-append "GuixSD " (latest-guix-version) " QEMU Image")
    #:description
    `(div
      (p "QCOW2 virtual machine (VM) image."))
    #:image (guix-url "static/base/img/QEMU-package.png")
    #:base-url (string-append "https://alpha.gnu.org/gnu/guix/guixsd-vm-image-"
			      (latest-guix-version) ".")
    #:variants (list (variant "x86_64" "x86_64-linux.xz"))
    #:manual (manual-url "Installing-GuixSD-in-a-VM.html"))

   (download
    #:title (string-append "GNU Guix " (latest-guix-version) " Binary")
    #:description
    '(p
      "Self-contained tarball providing binaries for Guix and for
       all its dependencies.")
    #:image (guix-url "static/base/img/Guix-package.png")
    #:base-url (string-append "https://alpha.gnu.org/gnu/guix/guix-binary-"
			      (latest-guix-version) ".")
    #:variants (list (variant "x86_64" "x86_64-linux.tar.xz")
		     (variant "i686" "i686-linux.tar.xz")
		     (variant "armhf" "armhf-linux.tar.xz")
                     (variant "aarch64" "aarch64-linux.tar.xz"))
    #:manual (manual-url "Binary-Installation.html"))

   (download
    #:title (string-append "GNU Guix " (latest-guix-version) " Source")
    #:description '(p "Source code distribution.")
    #:image (guix-url "static/base/img/src-package.png")
    #:base-url (string-append "https://alpha.gnu.org/gnu/guix/guix-"
			      (latest-guix-version) ".")
    #:variants (list (variant "tarball" "tar.gz"))
    #:manual (manual-url "Requirements.html"))))
