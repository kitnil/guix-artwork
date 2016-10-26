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
             (haunt builder blog)
             (haunt builder atom)
             (ice-9 match)
             (srfi srfi-1)
             (www)
             (www utils)
             (www news))

(define %local-test?
  ;; True when we're testing locally, as opposed to producing things to
  ;; install to gnu.org.
  (or (getenv "GUIX_WEB_SITE_LOCAL")
      (member "serve" (command-line))))           ;'haunt serve' command

(when %local-test?
  ;; The URLs produced in these pages are only meant for local consumption.
  (format #t "~%Producing Web pages for local tests *only*!~%~%"))

(define-syntax-rule (with-url-parameters body ...)
  "Run BODY in a context where URL parameters honor %LOCAL-TEST?."
  (parameterize ((current-url-root (if %local-test?
                                       ""
                                       (current-url-root)))
                 (gnu.org-root (if %local-test?
                                   "https://www.gnu.org"
                                   (gnu.org-root))))
    body ...))

(define (parameterized-procedure proc)
  (lambda args
    (with-url-parameters
     (apply proc args))))

(define (parameterized-theme thm)
  (theme #:name (theme-name thm)
         #:layout (parameterized-procedure (theme-layout thm))
         #:post-template (parameterized-procedure (theme-post-template thm))
         #:collection-template (parameterized-procedure
                                (theme-collection-template thm))))

(site #:title "GNU's advanced distro and transactional package manager"
      #:domain "//www.gnu.org/software/guix"
      #:default-metadata
      '((author . "GuixSD Contributors")
        (email  . "guix-devel@gnu.org"))
      #:readers (list sxml-reader)
      #:builders
      `(,(lambda (site posts)                     ;the main page
           (with-url-parameters
            (make-page "guix.html" (main-page site posts)
                       sxml->html)))
        ,@(filter-map (match-lambda
                        (("guix.html" _)          ;handled above
                         #f)
                        ((file-name contents)
                         (lambda (site posts)
                           (with-url-parameters
                            (make-page file-name (contents) sxml->html)))))
                      %web-pages)
        ,(blog #:theme (parameterized-theme %news-haunt-theme)
               #:prefix "news")
        ,(atom-feed #:file-name "news/feed.xml"
                    #:blog-prefix "news")
        ,(static-directory "static")))
