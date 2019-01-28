;;; GuixSD website --- GNU's advanced distro website
;; Copyright © 2017 ng0 <ng0@infotropique.org>
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.
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
       ", the standalone Guix System "
       (a
	(@ (href ,(manual-url "System-Installation.html")))
	"can be installed")
       " on an i686, x86_64, ARMv7, or AArch64 machine.  It uses the "
       (a (@ (href ,(gnu-url "software/linux-libre"))) "Linux-Libre")
       " kernel and the "
       (a (@ (href ,(gnu-url "software/shepherd"))) "GNU Shepherd")
       " init system. Alternately, GNU Guix
       can be installed as an additional package manager on top of an
       installed Linux-based system.")

      (div
       (@ (class "centered-text"))
       ,@(map download (context-datum context "downloads")))

      (p
       (@ (class "centered-block limit-width"))
       "Source code and binaries for the Guix System distribution ISO
       image as well as GNU Guix can be found on the GNU servers at "
       (a (@ (href "https://alpha.gnu.org/gnu/guix/"))
	  "https://alpha.gnu.org/gnu/guix/") ".")))))
