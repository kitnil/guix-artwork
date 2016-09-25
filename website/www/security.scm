;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
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

(define-module (www security)
  #:use-module (www utils)
  #:use-module (www shared)
  #:export (security-page))

(define (security-page)
  `(html (@ (lang "en"))
         ,(html-page-header "Security")
         ,(html-page-links)
         (div (@ (id "content-box"))
              (article
               (h1 "Security")
               (h2 "How to report security issues")
               (p "To report sensitive security issues in Guix itself or the packages it "
                  "provides, you can write to the private mailing list "
                  (a (@ (href "https://lists.gnu.org/mailman/listinfo/guix-security"))
                     ("guix-security@gnu.org"))
                     ".  This list is monitored by a small team of Guix "
                     "developers.")
               (h2 "Release signatures")
               (p "Releases of Guix and GuixSD are signed using the OpenPGP "
                  "key with the fingerprint "
                  "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5.  "
                  "Users should "
                  (a (@ (href ,(base-url "manual/html_node/Binary-Installation.html")))
                     "verify")
                  " their downloads before extracting or running them.")
               (h2 "Security updates")
               (p "When security vulnerabilities are found in Guix or the "
                  "packages provided by Guix, we will provide "
                  (a (@ (href ,(base-url "manual/html_node/Security-Updates.html")))
                     "security updates")
                  " quickly and with minimal disruption for users.")
               (p "Guix uses a \"rolling release\" model.  All security "
                  "bug-fixes are pushed directly to the master branch.  There"
                  " is no \"stable\" branch that only receives security fixes.")
               ,(html-page-footer)))))
