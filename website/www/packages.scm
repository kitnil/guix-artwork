;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2013 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
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

(define-module (www packages)
  #:use-module (www utils)
  #:use-module (www shared)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gnu-maintenance)
  #:use-module ((guix download) #:select (%mirrors))
  #:use-module ((guix build download) #:select (maybe-expand-mirrors))
  #:use-module (gnu packages)
  #:use-module (sxml simple)
  #:use-module (sxml fold)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (packages-page))

(define lookup-gnu-package
  (let ((gnu (official-gnu-packages)))
    (lambda (name)
      "Return the package description for GNU package NAME, or #f."
      (find (lambda (package)
              (equal? (gnu-package-name package) name))
            gnu))))

(define (list-join lst item)
  "Join the items in LST by inserting ITEM between each pair of elements."
  (let loop ((lst    lst)
             (result '()))
    (match lst
      (()
       (match (reverse result)
         (()
          '())
         ((_ rest ...)
          rest)))
      ((head tail ...)
       (loop tail
             (cons* head item result))))))

(define (package->sxml package previous description-ids remaining)
  "Return 3 values: the SXML for PACKAGE added to all previously collected
package output in PREVIOUS, a list of DESCRIPTION-IDS and the number of
packages still to be processed in REMAINING.  Also Introduces a call to the
JavaScript prep_pkg_descs function as part of the output of PACKAGE, every
time the length of DESCRIPTION-IDS, increasing, is 15 or when REMAINING,
decreasing, is 1."
  (define (location-url loc)
    (string-append "http://git.savannah.gnu.org/cgit/guix.git/tree/"
                   (location-file loc) "#n"
                   (number->string (location-line loc))))

  (define (source-url package)
    (let ((loc (package-location package)))
      (and loc (location-url loc))))

  (define (license package)
    (define ->sxml
      (match-lambda
       ((lst ...)
        `(div ,(map ->sxml lst)))
       ((? license? license)
        (let ((uri (license-uri license)))
          (case (and=> (and uri (string->uri uri)) uri-scheme)
            ((http https)
             `(div (a (@ (href ,uri)
                         (title "Link to the full license"))
                      ,(license-name license))))
            (else
             `(div ,(license-name license) " ("
                   ,(license-comment license) ")")))))
       (#f "")))

    (->sxml (package-license package)))

  (define (patches package)
    (define patch-url
      (match-lambda
       ((? string? patch)
        (string-append
         "http://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/patches/"
         (basename patch)))
       ((? origin? patch)
        (uri->string
         (first (maybe-expand-mirrors (string->uri
                                       (match (origin-uri patch)
                                         ((? string? uri) uri)
                                         ((head . tail) head)))
                                      %mirrors))))))

    (define patch-name
      (match-lambda
       ((? string? patch)
        (basename patch))
       ((? origin? patch)
        (match (origin-uri patch)
          ((? string? uri) (basename uri))
          ((head . tail) (basename head))))))

    (define (snippet-link snippet)
      (let ((loc (or (package-field-location package 'source)
                     (package-location package))))
        `(a (@ (href ,(location-url loc))
               (title "Link to patch snippet"))
            "snippet")))

    (and (origin? (package-source package))
         (let ((patches (origin-patches (package-source package)))
               (snippet (origin-snippet (package-source package))))
           (and (or (pair? patches) snippet)
                `(div "patches: "
                      ,(let loop ((patches patches)
                                  (number  1)
                                  (links   '()))
                         (match patches
                           (()
                            (let* ((additional (and snippet
                                                    (snippet-link snippet)))
                                   (links      (if additional
                                                   (cons additional links)
                                                   links)))
                              (list-join (reverse links) ", ")))
                           ((patch rest ...)
                            (loop rest
                                  (+ 1 number)
                                  (cons `(a (@ (href ,(patch-url patch))
                                               (title ,(string-append
                                                        "Link to "
                                                        (patch-name patch))))
                                            ,(number->string number))
                                        links))))))))))

  (define (status package)
    (define (url system)
      `(a (@ (href ,(string-append "http://hydra.gnu.org/job/gnu/master/"
                                   (package-full-name package) "."
                                   system))
             (title "View the status of this architecture's build at Hydra"))
          ,system))

    `(div "status: "
          ,(list-join (map url
                           (lset-intersection
                            string=?
                            %hydra-supported-systems
                            (package-transitive-supported-systems package)))
                      " ")))

  (define (package-logo name)
    (and=> (lookup-gnu-package name)
           gnu-package-logo))

  (define (insert-tr description-id js?)
    (define (insert-js-call description-ids)
      "Return an sxml call to prep_pkg_descs, with up to 15 elements of
description-ids as formal parameters."
      `(script
	,(format #f "prep_pkg_descs(~a)"
		 (string-append "'"
				(string-join description-ids "', '")
				"'"))))

    (let ((description-ids (cons description-id description-ids)))
      `(tr (td ,(if (gnu-package? package)
                    `(img (@ (src ,(gnu-url "/graphics/gnu-head-mini.png"))
                             (alt "Part of GNU")
                             (title "Part of GNU")))
                    ""))
           (td (a (@ (href ,(source-url package))
                     (title "Link to the Guix package source code"))
                  ,(package-name package) " "
                  ,(package-version package)))
           (td (span ,(package-synopsis package))
               (div (@ (id ,description-id))
                    ,(match (package-logo (package-name package))
                       ((? string? url)
                        `(img (@ (src ,url)
                                 (height "35")
                                 (class "package-logo")
                                 (alt ("Logo of " ,(package-name package))))))
                       (_ #f))
                    (p ,(package-description package))
                    ,(license package)
                    (a (@ (href ,(package-home-page package))
                          (title "Link to the package's website"))
                       ,(package-home-page package))
                    ,(status package)
                    ,(patches package)
                    ,(if js?
                         (insert-js-call description-ids)
                         ""))))))

  (let ((description-id (symbol->string
                         (gensym (package-name package)))))
    (cond ((= remaining 1)              ; Last package in packages
           (values
            (reverse                              ; Fold has reversed packages
             (cons (insert-tr description-id 'js) ; Prefix final sxml
                   previous))
            '()                            ; No more work to do
            0))                            ; End of the line
          ((= (length description-ids) 15) ; Time for a JS call
           (values
            (cons (insert-tr description-id 'js)
                  previous)    ; Prefix new sxml
            '()                ; Reset description-ids
            (1- remaining)))   ; Reduce remaining
          (else                ; Insert another row, and build description-ids
           (values
            (cons (insert-tr description-id #f)
                  previous)                       ; Prefix new sxml
            (cons description-id description-ids) ; Update description-ids
            (1- remaining))))))                   ; Reduce remaining

(define (packages->sxml packages)
  "Return an SXML table describing PACKAGES."
  `(div
    (table (@ (id "packages"))
           (tr (th "GNU?")
               (th "Package version")
               (th "Package details"))
           ,@(fold-values package->sxml packages '() '() (length packages)))
    (a (@ (href "#content-box")
          (title "Back to top.")
          (id "top"))
       "^")))


(define (packages-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Packages" #:css "packages.css" #:js "packages.js")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)
	  (div (@ (id "content-box"))
	       (article
		(h1 "Packages")
		(p "The Guix System Distribution provides 1,500+ packages
transparently "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master#tabs-status"))
		      "available as pre-built binaries")
		   ". This is a complete lists of the packages. Our "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
		      "continuous integration system")
		   " shows their current build status.")
		,(let ((packages (sort (fold-packages cons '())
				       (lambda (p1 p2)
					 (string<? (package-name p1)
						   (package-name p2))))))
		   (packages->sxml packages))))
	  ,(html-page-footer))))
