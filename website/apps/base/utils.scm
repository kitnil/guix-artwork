;;; GNU Guix web site
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2013 Alex Sassmannshausen <alex.sassmannshausen@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Initially written by sirgazil who waives all copyright interest on this
;;; file.
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

(define-module (apps base utils)
  #:use-module (apps aux lists)
  #:use-module (apps aux system)
  #:use-module (apps base types)
  #:use-module (apps i18n)
  #:use-module (haunt page)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (gnu-url
	    guix-git-tree-url
	    guix-irc-log-url
	    guix-url
	    latest-guix-version
            locale-display-name
	    manual-url
            manual-url-with-language
	    number*
	    paginate))


;;;
;;; Guix variables.
;;;

(define guix-irc-log-url "http://logs.guix.gnu.org/guix")

(define guix-root-url-path
  (make-parameter "/")) ; Path to GNU Guix site at guix.gnu.org

(define latest-guix-version
  (make-parameter "1.0.1"))

(define (locale-display-name)
  "Return the display name of the current locale."
  ;; TRANSLATORS: The locale’s display name; please include a country
  ;; code like in English (US) *only* if there are multiple
  ;; Translation Project teams for the same language.
  (let ((str '(G_ "English (US)")))
    (gettext (cadr str))))



;;;
;;; URL linking.
;;;

(define* (gnu-url #:optional (path ""))
  "Append PATH to GNU.org URL.

   PATH (string)
     An optional relative URL path to a resource. For example:
     'software/guile/'.

   RETURN VALUE (string)
     A URL. For example: https://gnu.org/software/guile/."
  (string-append "https://gnu.org/" path))


(define* (guix-git-tree-url #:optional (subpath ""))
  "Append SUBPATH to the URL of the GNU Guix git repository tree.

   SUBPATH (string)
     An optional relative URL path to a node, line or code, etc., in the
     tree. For example: 'gnu/packages/games.scm';
     'gnu/packages/games.scm#n4111'.

   RETURN VALUE (string)
     A URL path. For example:
     https://git.savannah.gnu.org/cgit/guix.git/tree/gnu/packages/games.scm#n4111."
  (string-append "https://git.savannah.gnu.org/cgit/guix.git/tree/" subpath))


(define* (guix-url #:optional (subpath "") #:key (localize #t))
  "Append SUBPATH to GNU Guix root URL path (see guix-root-url-path).

   SUBPATH (string)
     An optional relative URL path to a resource in the GNU Guix path.
     For example: 'packages/icecat-XYZ/'.

   LOCALIZE (boolean)
     Whether to call localize-url on the URL path.

   RETURN VALUE (string)
     A URL path. For example: /software/guix/packages/icecat-XYZ/."
  ((if localize localize-url identity)
   ;; If we are trying out the website locally, use "/" as the root.
   ;; Otherwise use guix-root-url-path for deployment to gnu.org.
   (if (getenv "GUIX_WEB_SITE_LOCAL")
       (string-append "/" subpath)
       (string-append (guix-root-url-path) subpath))))


(define* (manual-url #:optional (subpath "")
                     #:key (language "en"))
  "Append SUBPATH to the GNU Guix manual URL path.

   SUBPATH (string)
     An optional relative URL path to a section of the manual.
     For example: 'System-installation.html'.

   RETURN VALUE (string)
     A URL path. For example:
     /software/guix/manual/en/html_node/System-installation.html."
  (string-append
   (guix-url (string-append (string-append "manual/" language
                                           "/html_node/")
                            subpath))))

(define* (manual-url-with-language _ language #:optional (subpath ""))
  "Shorthand for manual-url without keywords for prettier output
PO files when marked for translation.  It can be marked for translation
as:

  (G_ (manual-url-with-language (G_ \"en\") (G_ \"Some-section.html\")))

   LANGUAGE (string)
     Normalized language for the Guix manual as produced by
'doc/build.scm' in the Guix source tree, i.e. \"en\" for the English
manual.

   SUBPATH (string)
     Like manual-url.

   RETURN VALUE (string)
     A URL path. For example:
     /software/guix/manual/en/html_node/System-installation.html."
  ;; The _ argument is a placeholder for an arg added by G_, cf. i18n-howto.txt.
  (manual-url subpath #:language language))



;;;
;;; Helper procedures.
;;;

(define (number* number)
  "Return NUMBER correctly formatting according to English conventions."
  (number->locale-string number 0
                         (or (false-if-exception
                              (make-locale LC_ALL "en_US.utf8"))
                             (make-locale LC_ALL "en_US.UTF-8"))))


(define* (paginate #:key dataset (limit 30) base-path template (context '()) writer)
  "Distribute the objects of the DATASET in pages.

   DATASET (list)
     A list with any kind of object.

   LIMIT (integer)
     The maximum number of objects that should appear in a page.

     The limit is optional. If not provided, it defaults to 30.

   BASE-PATH (string)
     A system path relative to the website directory where all the
     pages will be written to. For example: 'blog' or 'blog/tags'.

     In the latter example, pages would be written to files in a path
     like 'blog/tags/page/PAGE_NUMBER/index.html'.

   TEMPLATE (procedure)
     A procedure that accepts a context and returns an SXML tree.

   CONTEXT (context)
     A context object as defined in (apps base types). The context
     holds additional data to insert into the TEMPLATE.

     The context is optional, and will always be extended to include
     the following data that can be used in the TEMPLATE:

     items (list)
       The list of items to insert into the page.

     total-pages (integer)
       The number of pages generated to distribute all items.

     page-number (integer)
       The number of the page.

   WRITER
     A procedure that writes the page into a given format. See Haunt's
     'sxml->html' writer in the (haunt html) module, for example.

   RETURN VALUE (list)
     A list of <page> objects as defined in (haunt page) module."
  (let* ((grouped-data (list-group dataset limit))
	 (total-pages (cons "total-pages" (length grouped-data))))
    ;; Read the following like (cons Page ListOfPages):
    (cons
     ;; Page
     ;; This is the cover of the pages. For example, the resource
     ;; located in a path such as /blog/, which is identical to the
     ;; resource available in /blog/page/1/.
     (let* ((page-number (cons "page-number" 1))
	    (path (path-join base-path "index.html"))
	    (items
             (match grouped-data
               (()
                (cons "items" '()))
               ((head _ ...)
                (cons "items" head))))
	    (new-context
	     (append context
		     (list items page-number total-pages))))

       (make-page path (template new-context) writer))
     ;; ListOfPages
     ;; This is a list of pages that are the actual ordered pages
     ;; located in paths such as /blog/page/NUMBER/.
     (map
      (lambda (index)
	(let* ((page-number (cons "page-number" (+ index 1)))
	       (path (path-join base-path
				"page"
				(number->string (+ index 1))
				"index.html"))
	       (items (cons "items" (list-ref grouped-data index)))
	       (new-context
		(append context (list items page-number total-pages))))
	  (make-page path (template new-context) writer)))

      (iota (length grouped-data))))))
