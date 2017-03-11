;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Initially written by Luis Felipe López Acevedo <felipe.lopez@openmailbox.org>
;;; who waives all copyright interest on this file.
;;;
;;; This file is part of GuixSD website.
;;;
;;; GuixSD website is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GuixSD website is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with GuixSD website.  If not, see <http://www.gnu.org/licenses/>.

(define-module (www download)
  #:use-module (www utils)
  #:use-module (www shared)
  #:use-module (ice-9 match)
  #:export (download-page))

(define %usb-image-description
  `(div (div (@ (class "message-box msg-info"))
             (span (@ (class "msg-label")) "Note ")
             "The Guix System Distribution (GuixSD) is beta software, "
             "which means it is "
             (a (@ (href ,(base-url "manual/html_node/Limitations.html")))
                "not production-ready")
             ".  But you can "
             (a (@ (href ,(base-url "contribute"))) "help") "!")
        (p "USB installer of the Guix System Distribution.")))

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

(define %guix-src-image
  "src-package.png")

(define %vm-image-description
  "Virtual machine (QEMU) image of GuixSD.")

(define %vm-image-manual
  "manual/html_node/Running-GuixSD-in-a-VM.html")

(define %vm-image-image
  "QEMU-package.png")

(define (ftp-url file)
  (string-append "ftp://alpha.gnu.org/gnu/guix/" file))

(define (https-url file)
  (string-append "https://alpha.gnu.org/gnu/guix/" file))

(define (guixsd-files archs)
  (map (lambda (arch)
         (cons arch (https-url (string-append "guixsd-usb-install-"
                                            (latest-guix-version) "." arch
                                            "-linux.xz"))))
       archs))

(define (guixsd-vm-image-files archs)
  (map (lambda (arch)
         (cons arch (https-url (string-append "guixsd-vm-image-"
                                              (latest-guix-version) "." arch
                                              "-linux.xz"))))
       archs))

(define (guix-files archs)
  (map (lambda (arch)
         (cons arch (https-url (string-append "guix-binary-" (latest-guix-version)
                                            "." arch "-linux.tar.xz"))))
       archs))

(define (guix-source-files variants)
  (map (lambda (variant)
         (cons variant (https-url (string-append "guix-" (latest-guix-version)
                                               ".tar.gz"))))
       variants))

(define* (download-box title
                       #:key files description image manual)
  `(div (@ (class "download-box"))
        (img (@ (src ,(image-url image)) (alt "")))
        (h2 ,title)
        (p ,description)
        (p "Download options:")
        ,(map (match-lambda
                ((title . url)
                 `(a (@ (href ,url) (class "hlink-download"))
                     ,title)))
              files)
        (p "Signatures: "
           ,(map (match-lambda
                   ((title . url)
                    `(a (@ (href ,(string-append url ".sig"))
                           (class "hlink-signature"))
                        ,title)))
                 files))
        (p (a (@ (href ,(guix-url manual)))
              "Installation instructions")
           ".")))

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
		   (a (@ (href ,(gnu-url "software/shepherd"))) "GNU Shepherd")
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
                            #:files (guix-files '("x86_64" "i686" "armhf"))
                            #:description %binary-tarball-description
                            #:manual %binary-tarball-manual
                            #:image %guix-image)
             ,(download-box (string-append "GNU Guix " (latest-guix-version)
                                           " Source")
                            #:files (guix-source-files '("tarball"))
                            #:description %source-tarball-description
                            #:manual %source-tarball-manual
                            #:image %guix-src-image)
             ,(download-box (string-append "GuixSD " (latest-guix-version)
                                           " Virtual Machine Image")
                            #:files (guixsd-vm-image-files '("x86_64"))
                            #:description %vm-image-description
                            #:manual %vm-image-manual
                            #:image %vm-image-image))

		(p "Source code for the Guix System Distribution USB
installation images as well as GNU Guix can be found on the GNU ftp server for "
		   (em "alpha")
		   " releases: "
		   (a (@ (href "https://alpha.gnu.org/gnu/guix/"))
		      "https://alpha.gnu.org/gnu/guix/")
		   " (via HTTPS) and "
		   (a (@ (href "ftp://alpha.gnu.org/gnu/guix/"))
		      "ftp://alpha.gnu.org/gnu/guix/")
		   " (via FTP). ")))
	  ,(html-page-footer))))
