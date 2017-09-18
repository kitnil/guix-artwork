;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2013 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix monads)
  #:use-module (guix grafts)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (%mirrors))
  #:use-module ((guix build download) #:select (maybe-expand-mirrors))
  #:use-module (guix build utils)
  #:use-module (guix scripts lint)
  #:use-module (guix scripts challenge)
  #:use-module (guix scripts substitute)
  #:use-module (gnu packages)
  #:use-module (sxml simple)
  #:use-module (sxml match)
  #:use-module (sxml fold)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:export (%groups
            package-pages
            paginated-packages-page
            issues-page
            reproducibility-page
            packages->json))

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

(define git-description
  (delay
   (let* ((guix (find (lambda (p)
                        (file-exists? (string-append p "/guix/config.scm")))
                      %load-path))
          (pipe (with-directory-excursion guix
                  (open-pipe* OPEN_READ "git" "describe")))
          (desc (read-line pipe))
          (git? (close-pipe pipe)))
     (and git? desc))))

(define (location-url loc)
  (string-append "//git.savannah.gnu.org/cgit/guix.git/tree/"
                 (location-file loc)
                 (or (and=> (force git-description)
                            (cut string-append "?id=" <>))
                     "")
                 "#n" (number->string (location-line loc))))

(define (source-url package)
  (let ((loc (package-location package)))
    (and loc (location-url loc))))

(define (texi->shtml str)
  "Return a SXML representation of STR with HTML vocabulary."
  ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
  ;; those string ports are Unicode-capable.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (and=> str (compose stexi->shtml texi-fragment->stexi))))

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
         "//git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/patches/"
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
                    (alt "unknown")
                    (title "unknown")))
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
           (td (a (@ (id ,anchor)
		     (href ,(source-url package))
                     (title "Link to the Guix package source code"))
                  ,(package-name package) " "
                  ,(package-version package)))
           "\n"
           (td (span ,(sxml-match
                       (texi->shtml (package-synopsis package))
                       ((div (p ,body ...)) body) ;strip <p>
                       (,x x)))
               (div (@ (id ,description-id))
                    ,(match (package-logo (package-name package))
                       ((? string? url)
                        `(img (@ (src ,url)
                                 (height "35")
                                 (class "package-logo")
                                 (alt ("Logo of " ,(package-name package))))))
                       (_ #f))
                    "\n"
                    ,(texi->shtml (package-description package))
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

(define* (packages->json #:optional (packages (all-packages)))
  "Return a JSON string representing PACKAGES."
  (define (package->alist package)
    `((name . ,(package-name package))
      (version . ,(package-version package))
      (cpe-name . ,(or (assoc-ref (package-properties package) 'cpe-name)
                       (package-name package)))
      (cpe-version . ,(or (assoc-ref (package-properties package)
                                     'cpe-version)
                          (package-version package)))
      (home-page . ,(package-home-page package))))

  (scm->json-string (map package->alist packages)
                    #:pretty #t))

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
       "See " (a (@ (href ,(source-url package))) "package definition")
       " in Guix source code.")

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
;;; Reproducibility issues reported by 'challenge'.
;;;

(define %substitute-servers
  ;; List of remote substitute servers against which we are comparing.
  '("https://berlin.guixsd.org"
    "https://bayfront.guixsd.org"))

(define (local-nar-url item)
  (string-append "https://mirror.hydra.gnu.org/nar/"
                 (basename item)))

(define (comparison-report->sxml report)
  "Return the HTML for REPORT."
  (let ((item (comparison-report-item report)))
    `(li (tt ,item)
         (ol
          ,(if (comparison-report-inconclusive? report)
               "No idea if this one is reproducible."
               `(li (a (@ (href ,(local-nar-url item))))
                    (tt ,(bytevector->base32-string
                          (comparison-report-local-sha256 report)))
                    ,@(map (lambda (narinfo)
                             `(li (a (@ (href ,(uri->string
                                                (narinfo-uri narinfo)))))
                                  (tt ,(bytevector->base32-string
                                        (narinfo-hash->sha256
                                         (narinfo-hash narinfo))))))
                           (comparison-report-narinfos report))))))))

(define* (package->reproducibility-sxml package reports
                                        #:key anchor)
  "Return an SXML representation of REPORTS for PACKAGE."
  (let ((name (string-append (package-name package) " "
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
       ,@(if (every comparison-report-inconclusive? reports)
             '("No idea if it's reproducible.  ")
             (list (issue-count->sxml
                    (count comparison-report-mismatch? reports))
                   ". "))
       "See " (a (@ (href ,(source-url package))) "package definition")
       " in Guix source code.")

      ,(and (any comparison-report-mismatch? reports)
            `(div (@ (class "issue"))
                  (pre (ul ,@(map comparison-report->sxml reports))))))))

(define* (packages->reproducibility-sxml packages
                                         #:key (servers %substitute-servers))
  "Return an SXML tree representing the discrepancies found in the outputs of
PACKAGES on SERVERS."
  (define total (length packages))

  (define package-anchor
    (packages->anchors packages))

  (define (one-of lst)
    (lambda (report)
      (member (comparison-report-item report) lst)))

  (define (add-package-outputs package mapping)
    ;; Add PACKAGE to MAPPING, a vhash that maps packages to outputs.
    (mlet* %store-monad ((drv     (package->derivation package))
                         (outputs ->  (match (derivation->output-paths drv)
                                        (((_ . outputs) ...)
                                         outputs))))
      (foldm %store-monad
             (lift2 (cut vhash-consq package <> <>) %store-monad)
             mapping
             outputs)))

  (mlet* %store-monad ((mapping  (foldm %store-monad add-package-outputs
                                        vlist-null packages))
                       (items -> (vlist-fold (lambda (item result)
                                               (match item
                                                 ((_ . output)
                                                  (cons output result))))
                                             '()
                                             mapping))
                       (reports  (compare-contents items
                                                   %substitute-servers)))
    (define (->sxml package)
      (let* ((outputs (vhash-foldq* cons '() package mapping))
             (reports (filter (one-of outputs) reports)))
        (package->reproducibility-sxml package reports
                                       #:anchor
                                       (package-anchor package))))

    (let ((total-items  (length items))
          (mismatches   (count comparison-report-mismatch? reports))
          (inconclusive (count comparison-report-inconclusive? reports)))
      (return `(div "Considered " ,total
                    " packages, corresponding to "
                    ,total-items
                    " " (tt "/gnu/store") " items, for "
                    (tt ,(%current-system)) ".\n"

                    "Out of these, "
                    ,(issue-count->sxml mismatches)
                    " were found ("
                    ,(inexact->exact
                      (round (* 100. (/ mismatches total-items))))
                    "%).  There are "
                    ,inconclusive " items ("
                    ,(inexact->exact
                      (round (* 100. (/ inconclusive total-items))))
                    "%) for which we could not conclude.\n\n"

                    ,@(map ->sxml packages))))))


;;;
;;; Pages.
;;;

(define %groups
  ;; List of package groups.
  (cons "0-9"
        (map string
             '(#\a #\b #\c #\d #\e #\f #\g #\h
               #\i #\j #\k #\l #\m #\n #\o #\p
               #\q #\r #\s #\t #\u #\v #\w #\x
               #\y #\z))))

(define (group-file-name group)
  (guix-url (string-append "packages/" group ".html")))

(define (group-name group)
  (string-upcase group))

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

(define packages-by-grouping
  (lambda* (packages #:optional (grouping 'all))
    "Return an alphabetically sorted list of the subset of PACKAGES limited
to those matching GROUPING.  GROUPING can be 'all for all packages,
the string '0-9' for all packages starting with digits, or a string of
a single, lower-case letter for a list of all packages starting with
that letter."
    (match grouping
      ('all (all-packages))
      ("0-9" (filter (compose (cut char-set-contains? char-set:digit <>)
                              first string->list package-name)
                     packages))
      (letter (filter (lambda (package)
                        (string=? (string-take (package-name package) 1)
                                  letter))
                      packages)))))

(define (paginated-packages-page packages grouping)
  "Return a packages page that contains only content for the subset of
PACKAGES that matches GROUPING (either the string '0-9' or a string of one
letter)."
  (packages-page (packages-by-grouping packages grouping)
                 (string-upcase grouping)))

(define* (packages-page #:optional
                        (packages (remove package-superseded (all-packages)))
                        (grouping "All"))
  `(html (@ (lang "en"))
         ,(html-page-header "Packages" #:css "packages.css" #:js "packages.js")
         (body
          ,(html-page-description)
          ,(html-page-links)

          (div (@ (id "content-box"))
               (article
                (h1 ,(string-append "Packages [" grouping "]"))
                (p "GNU Guix provides "
                   ,(number* (fold-packages (lambda (p n) (+ 1 n)) 0))
                   " packages transparently "
                   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master#tabs-status"))
                      "available as pre-built binaries")
                   ". These pages provide a complete list of the packages.
  Our "
                   (a (@ (href "http://hydra.gnu.org/jobset/gnu/master"))
                      "continuous integration system")
                   " shows their current build status "
                   "(updated " ,(date->string (current-date) "~B ~e, ~Y") ").")
                (p "You can browse packages indexed by their first letter.")
                (ul
                 ,@(map (lambda (group)
                          `(li (@ (id ,(string-append group "-link"))
                                  (class "package-index-link"))
                               (a (@ (href ,(group-file-name group)))
                                  ,(group-name group))))
                        %groups))
                ,(packages->sxml packages)))

          ,(html-page-footer))))

(define* (package-pages #:optional
                        (packages (remove package-superseded (all-packages))))
  "Return a list of (FILE PAGE) tuples, where each FILE is an HTML file name
and PAGE is the corresponding SXML."
  `(,@(map (lambda (group)
             (list (string-append group ".html")
                   (paginated-packages-page packages group)))
           %groups)
    ;; Note: We do not build the page for grouping "All" because the poor CVS
    ;; server at gnu.org runs out of memory when it encounters such a big
    ;; page (!).
    ("index.html" ,(paginated-packages-page packages "a"))))

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
	   "updated " ,(date->string (current-date) "~B ~e, ~Y") ").")

       ,(packages->issue-sxml (all-packages) #:checkers checkers)))

     ,(html-page-footer))))

(define* (reproducibility-page)
  `(html
    (@ (lang "en"))
    ,(html-page-header "Package Reproducibility" #:css "packages.css")
    (body
     ,(html-page-description)
     ,(html-page-links)

     (div
      (@ (id "content-box"))
      (article
       (h1 "Package Reproducibility")
       (p "Which of our packages is not "
          (a (@ (href "https://reproducible-builds.org"))
             "reproducible") "?  "
          "This page lists problems reported by "
	  (a
	   (@ (href ,(base-url "manual/html_node/Invoking-guix-challenge.html")))
	   (code "guix challenge")) " comparing two independent "
           "build machines ("
           "updated " ,(date->string (current-date) "~B ~e, ~Y") ").")

       ,(parameterize ((%graft? #f))
          (with-store store
            (run-with-store store
              (packages->reproducibility-sxml (all-packages)))))))

     ,(html-page-footer))))

;;;
;;; SXML Components
;;;

(define (issue-count->sxml count)
  "Return and SXML representation of COUNT."
  `(,(if (> count 0) 'mark 'span)
    ,(number->string count)
    ,(if (= count 1) " issue" " issues")))
