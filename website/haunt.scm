;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2015 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GuixSD website.
;;;
;;; GuixSD website is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GuixSD website is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GuixSD website.  If not, see <http://www.gnu.org/licenses/>.

;; This is a build file for Haunt.

(use-modules (haunt site)
             (haunt reader)
             (haunt page)
             (haunt html)
             (haunt utils)
             (haunt builder assets)
             (ice-9 match)
             (www)
             (www utils))

(define %local-test?
  ;; True when we're testing locally, as opposed to producing things to
  ;; install to gnu.org.
  (or (getenv "GUIX_WEB_SITE_LOCAL")
      (member "serve" (command-line))))           ;'haunt serve' command

(when %local-test?
  ;; The URLs produced in these pages are only meant for local consumption.
  (format #t "~%Producing Web pages for local tests *only*!~%~%"))

(site #:title "GNU's advanced distro and transactional package manager"
      #:domain "//www.gnu.org/software/guix"
      #:default-metadata
      '((author . "GuixSD Contributors")
        (email  . "guix-devel@gnu.org"))
      #:readers (list sxml-reader)
      #:builders
      `(,@(map (match-lambda
                 ((file-name contents)
                  (lambda (site posts)
                    (parameterize ((current-url-root (if %local-test?
                                                         ""
                                                         (current-url-root)))
                                   (gnu.org-root (if %local-test?
                                                     "https://www.gnu.org"
                                                     (gnu.org-root))))
                      (make-page file-name (contents) sxml->html)))))
               %web-pages)
        ,(static-directory "static")))
