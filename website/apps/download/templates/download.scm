;;; GNU Guix web site
;; Copyright © 2017 ng0 <ng0@infotropique.org>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

(define-module (apps download templates download)
  #:use-module (apps base templates theme)
  #:use-module (apps base types)
  #:use-module (apps base utils)
  #:use-module (apps download templates components)
  #:use-module (apps i18n)
  #:export (download-t))


(define (download-t context)
  "Return the Download page in SHTML."
  (theme
   #:title (C_ "webpage title" '("Download"))
   #:description
   (G_ "Installers and source files for GNU Guix.  GNU Guix can be
   installed on different GNU/Linux distributions.")
   #:keywords
   (string-split ;TRANSLATORS: |-separated list of webpage keywords
    (G_ "GNU|Linux|Unix|Free software|Libre software|Operating \
system|GNU Hurd|GNU Guix package manager|Installer|Source code|\
Package manager") #\|)
   #:active-menu-item (C_ "website menu" "Download")
   #:css (list
	  (guix-url "static/base/css/page.css")
	  (guix-url "static/base/css/download.css"))
   #:crumbs (list (crumb (C_ "website menu" "Download") "./"))
   #:content
   `(main
     (section
      (@ (class "page"))
      ,(G_ `(h2 "Download"))

      ,(G_
        `(p
          (@ (class "centered-block limit-width"))
          "As of version " ,(latest-guix-version)
          ", the standalone Guix System "
          ,(G_ `(a
                 (@ (href ,(manual-url "System-Installation.html")))
                 "can be installed"))
          " on an i686, x86_64, ARMv7, or AArch64 machine.  It uses the "
          ,(G_ `(a (@ (href ,(gnu-url "software/linux-libre"))) "Linux-Libre"))
          " kernel and the "
          ,(G_ `(a (@ (href ,(gnu-url "software/shepherd"))) "GNU Shepherd"))
          " init system. Alternately, GNU Guix
          can be installed as an additional package manager on top of an
          installed Linux-based system."))

      (div
       (@ (class "centered-text"))
       ,@(map download (context-datum context "downloads")))

      ,(G_
        `(p
          (@ (class "centered-block limit-width"))
          "Source code and binaries for the Guix System distribution ISO
          image as well as GNU Guix can be found on the GNU servers at "
          (a (@ (href "https://ftp.gnu.org/gnu/guix/"))
             "https://ftp.gnu.org/gnu/guix/")
          ".  Older releases can still be found on "
          (a (@ (href "https://alpha.gnu.org/gnu/guix/"))
             "alpha.gnu.org") "."))))))
