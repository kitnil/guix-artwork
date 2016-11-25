;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix ui) #:select (guix-warning-port))
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gnu-maintenance)
  #:use-module ((guix download) #:select (%mirrors))
  #:use-module ((guix build download) #:select (maybe-expand-mirrors))
  #:use-module (guix scripts lint)
  #:use-module (gnu packages)
  #:use-module (sxml simple)
  #:use-module (sxml fold)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:export (packages-page
            issues-page))

(define lookup-gnu-package
  (let ((gnu (delay (official-gnu-packages))))
    (lambda (name)
      "Return the package description for GNU package NAME, or #f."
      (find (lambda (package)
              (equal? (gnu-package-name package) name))
            (force gnu)))))

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

(define (location-url loc)
  (string-append "http://git.savannah.gnu.org/cgit/guix.git/tree/"
                 (location-file loc) "#n"
                 (number->string (location-line loc))))

(define (source-url package)
  (let ((loc (package-location package)))
    (and loc (location-url loc))))

(define (package->sxml package+anchor previous description-ids remaining)
  "Return 3 values: the SXML for PACKAGE added to all previously collected
package output in PREVIOUS, a list of DESCRIPTION-IDS and the number of
packages still to be processed in REMAINING.  Also Introduces a call to the
JavaScript prep_pkg_descs function as part of the output of PACKAGE, every
time the length of DESCRIPTION-IDS, increasing, is 15 or when REMAINING,
decreasing, is 1."
  (define-values (package anchor)
    (car+cdr package+anchor))

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
      `(div (img (@ (src ,(image-url "status-icons/undefined.png"))
                    (id ,(string-append "icon-" (package-full-name package) "." system))
                    (class "status-icon")
                    (alt "Unknown")
                    (title "Unknown")))
            (a (@ (href ,(string-append "http://hydra.gnu.org/job/gnu/master/"
                                   (package-full-name package) "."
                                   system))
             (title "View the status of this architecture's build at Hydra"))
          ,system)))

    `(div ,(list-join (map url
                           (lset-intersection
                            string=?
                            %hydra-supported-systems
                            (package-transitive-supported-systems package)))
                      " ")))

  (define (package-description-shtml package)
    "Return a SXML representation of PACKAGE description field with HTML
vocabulary."
    ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
    ;; those string ports are Unicode-capable.
    (with-fluids ((%default-port-encoding "UTF-8"))
      (and=> (package-description package)
             (compose stexi->shtml texi-fragment->stexi))))

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
                    `(img (@ (src ,(gnu-url "graphics/gnu-head-mini.png"))
                             (alt "Part of GNU")
                             (title "Part of GNU")))
                    ""))
           "\n"
           (td (a (@ (href ,(source-url package))
                     (title "Link to the Guix package source code"))
                  ,(package-name package) " "
                  ,(package-version package)))
           "\n"
           (td (span ,(package-synopsis package)
                     (a (@ (name ,anchor))))
               (div (@ (id ,description-id))
                    ,(match (package-logo (package-name package))
                       ((? string? url)
                        `(img (@ (src ,url)
                                 (height "35")
                                 (class "package-logo")
                                 (alt ("Logo of " ,(package-name package))))))
                       (_ #f))
                    "\n"
                    (p ,(package-description-shtml package))
                    "\n"
                    ,(license package)
                    (a (@ (href ,(package-home-page package))
                          (title "Link to the package's website"))
                       ,(package-home-page package))
                    ,(patches package)
                    (br)
                    "\n"
                    ,(status package)
                    "\n"
                    ,(if js?
                         (insert-js-call description-ids)
                         "")))
           "\n")))

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

(define (packages->anchors packages)
  "Return a one-argument procedure that, given package from the PACKAGES
list, returns a unique anchor for it.

Anchors are assigned such that the package name is the anchor of the latest
version of the package; older versions of the package, if any, have an anchor
of the form \"PACKAGE-X.Y.Z\"."
  (define anchor
    (let ((mapping (fold (lambda (package result)
                           (vhash-cons (package-name package) package
                                       result))
                         vlist-null
                         packages)))
      (lambda (package)
        ;; Return the anchor for PACKAGE.
        (match (vhash-fold* cons '() (package-name package) mapping)
          ((one)
           ;; There's only one version of PACKAGE, so use its name as the
           ;; anchor.
           (package-name package))
          ((several ..1)
           ;; There are several versions of PACKAGE.
           (let ((latest (reduce (lambda (v1 v2)
                                   (if (version>? v1 v2)
                                       v1 v2))
                                 (package-version package)
                                 (map package-version several))))
             ;; When PACKAGE is the latest version, use its name as the anchor;
             ;; otherwise use the full NAME-VERSION form.
             (if (string=? (package-version package) latest)
                 (package-name package)
                 (package-full-name package))))))))

  ;; Precompute the package → anchor mapping.
  (let ((anchors (fold (lambda (package result)
                         (vhash-consq package (anchor package) result))
                       vlist-null
                       packages)))
    (lambda (package)
      (match (vhash-assq package anchors)
        ((_ . anchor) anchor)))))

(define (packages->sxml packages)
  "Return an SXML table describing PACKAGES."
  (define package-anchor
    ;; Assignment of anchors to packages.
    (packages->anchors packages))

  `(div
    (table (@ (id "packages"))
           (tr (th "GNU?")
               (th "Package version")
               (th "Package details"))
           ,@(fold-values package->sxml
                          (zip packages (map package-anchor packages))
                          '() '() (length packages)))
    (a (@ (href "#content-box")
          (title "Back to top.")
          (id "top"))
       "^")))

(define (number* number)
  "Return NUMBER correctly formatting according to English conventions."
  (number->locale-string number 0
                         (or (false-if-exception
                              (make-locale LC_ALL "en_US.utf8"))
                             (make-locale LC_ALL "en_US.UTF-8"))))


;;;
;;; Issues reported by 'lint'.
;;;

(define %fast-issue-checkers
  ;; Runs in less than a minute for all the packages.
  (remove (lambda (checker)
            (case (lint-checker-name checker)
              ((home-page source derivation) #t)
              (else #f)))
          %checkers))

(define %issue-checkers
  ;; List of checkers used by default.
  %fast-issue-checkers)

(define (lint-checker-report checker package)
  "Return the report generated by CHECKER for PACKAGE as a string.  If the
result is the empty string, it means that CHECKER had nothing to complain about."
  (call-with-output-string
    (lambda (port)
      (parameterize ((guix-warning-port port))
        ((lint-checker-check checker) package)))))

(define (package-issues package checkers)
  "Report issues for PACKAGE based on reports generated by CHECKERS.  Each
issue is a CHECKER/REPORT tuple."
  (let ((reports (map (cut lint-checker-report <> package)
                      checkers)))
    (remove (match-lambda
              ((_  "") #t)
              (_       #f))
            (zip checkers reports))))

(define (issues->sxml package issues)
  "Return an SXML tree representing ISSUES for PACKAGE, where ISSUES is a
list of checker/report tuples."
  (if (null? issues)
      '(p "Nothing to declare!")
      (let ((count  (length issues)))
	`(div
	  ,@(map
	     (match-lambda
	      ((checker report)
	       `(div
		 (@ (class "issue"))
		 (p (@ (class "issue-type")) ,(lint-checker-name checker) ":")
		 (pre ,(string-trim-right report)))))
	     issues)))))

(define* (package->issue-sxml package
                              #:key
                              (anchor (package-full-name package))
                              (checkers %issue-checkers))
  "Return an SXML representation of PACKAGE containing all the reports
generated by CHECKERS."
  (let ((issues (package-issues package checkers))
	(name (string-append (package-name package) " "
			     (package-version package))))
    `(div
      (@ (class "issues-list"))
      (h2
       (@ (id ,anchor))
       ,name
       (a
	(@ (class "anchor-link") (href ,(string-append "#" anchor))
	   (title "Link to this section"))
	"§"))
      (p
       ;; Issue count
       ,(issue-count->sxml (length issues)) ". "
       "See " (a (@ (href ,(source-url package))) "package definition ")
       "in Guix source code.")

      ,(issues->sxml package issues))))

(define* (packages->issue-sxml packages #:key (checkers %issue-checkers))
  "Return an SXML tree representing the reports generated by CHECKERS for
PACKAGES."
  (define total (length packages))
  (define processed 0)
  (define (report-progress)
    (format (current-error-port) "~5,1f% of all the packages linted\r"
            (* 100. (/ processed total)))
    (force-output (current-error-port))
    (set! processed (+ 1 processed)))

  (define package-anchor
    (packages->anchors packages))

  `(div
    ,@(map (lambda (package)
	     (report-progress)
	     (package->issue-sxml package
				  #:anchor (package-anchor package)
				  #:checkers checkers))
	   packages)))


;;;
;;; Pages.
;;;

(define (all-packages)
  "Return the list of all package objects, sorted by name."
  (sort (fold-packages (lambda (package lst)
                         (cons (or (package-replacement package)
                                   package)
                               lst))
                       '())
        (lambda (p1 p2)
          (string<? (package-name p1)
                    (package-name p2)))))

(define (packages-page)
  `(html (@ (lang "en"))
	 ,(html-page-header "Packages" #:css "packages.css" #:js "packages.js")
	 (body
	  ,(html-page-description)
	  ,(html-page-links)

	  (div (@ (id "content-box"))
	       (article
		(h1 "Packages")
		(p "GNU Guix provides "
                   ,(number* (fold-packages (lambda (p n) (+ 1 n)) 0))
                   " packages transparently "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master#tabs-status"))
		      "available as pre-built binaries")
		   ". This is a complete list of the packages.  Our "
		   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
		      "continuous integration system")
		   " shows their current build status.")
		,(packages->sxml (all-packages))

                (p "Updated " ,(date->string (current-date) "~B ~e, ~Y")
                   ".")))

	  ,(html-page-footer))))

(define* (issues-page #:key (checkers %issue-checkers))
  `(html
    (@ (lang "en"))
    ,(html-page-header "Package Issues" #:css "packages.css")
    (body
     ,(html-page-description)
     ,(html-page-links)

     (div
      (@ (id "content-box"))
      (article
       (h1 "Package Issues")
       (p "Everybody's got issues! This page lists problems reported by "
	  (a
	   (@ (href ,(base-url "manual/html_node/Invoking-guix-lint.html")))
	   (code "guix lint")) " ("
	   "Updated " ,(date->string (current-date) "~B ~e, ~Y") ").")

       ,(packages->issue-sxml (all-packages) #:checkers checkers)))

     ,(html-page-footer))))


;;;
;;; SXML Components
;;;

(define (issue-count->sxml count)
  "Return and SXML representation of COUNT."
  `(,(if (> count 0) 'mark 'span)
    ,(number->string count)
    ,(if (= count 1) " issue" " issues")))
