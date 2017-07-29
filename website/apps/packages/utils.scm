;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;;
;;; Initially written by sirgazil
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

(define-module (apps packages utils)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps packages data)
  #:use-module (apps packages types)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (texinfo)
  #:use-module (texinfo html)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:export (package-description-shtml
            package-synopsis-shtml

            location->ilink
	    package-build-issues
	    package-issues?
	    package-lint-issues
	    package-patches
	    package-url-path
	    packages/group-by-letter))


;;;
;;; Helper procedures.
;;;

(define (texinfo->shtml texi)
  "Parse TEXI, a string, and return the corresponding SHTML."
  ;; 'texi-fragment->stexi' uses 'call-with-input-string', so make sure
  ;; those string ports are Unicode-capable.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (stexi->shtml (texi-fragment->stexi texi))))

(define (package-description-shtml package)
  "Return a SXML representation of PACKAGE description field with HTML
vocabulary."
  (and=> (package-description package) texinfo->shtml))

(define (package-synopsis-shtml package)
  "Return a SXML representation of PACKAGE synopsis field with HTML
vocabulary."
  (and=> (package-synopsis package)
         (lambda (synopsis)
           ;; Strip the paragraph that 'texinfo->shtml' adds.
           (match (texinfo->shtml synopsis)
             (('div ('p text ...))
              text)))))

(define git-description
  (delay
   (let* ((guix (find (lambda (p)
                        (file-exists? (string-append p "/guix/config.scm")))
                      %load-path))
          (pipe (with-directory-excursion guix
                  (open-pipe* OPEN_READ "git" "describe")))
          (desc (read-line pipe))
          (git? (close-pipe pipe)))
     (and (zero? git?) desc))))

(define (location->ilink loc)
  "Convert the given location LOC into an Ilink.

   LOC (<location>)
     A location object as defined in the GNU Guix API reference.

   RETURN (<ilink>)
     An Ilink object as defined in (apps packages types)."
  (ilink (basename (location-file loc))
	 (guix-git-tree-url
	  (string-append (location-file loc)
                         (or (and=> (force git-description)
                                    (cut string-append "?id=" <>))
                             "")
                         "#n"
                         (number->string (location-line loc))))))


;;; TODO: Stub. Implement.
;;; https://bitbucket.org/sirgazil/guixsd-website/issues/45/
(define (package-build-issues package)
  "Return the list of build issues for the given PACKAGE.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference.

   RETURN (list)
     A list of <location> objects as defined in (apps packages types)
     that represent build issues."
  (list))


;;; TODO: Add unit tests.
;;; https://bitbucket.org/sirgazil/guixsd-website/issues/44/
(define (package-issues? package)
  "Return true if the PACKAGE has lint or build issues.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference."
  (or (not (null? (package-lint-issues package)))
      (not (null? (package-build-issues package)))))


;;; TODO: Stub. Implement.
;;; https://bitbucket.org/sirgazil/guixsd-website/issues/43/
(define (package-lint-issues package)
  "Return the list of lint issues for the given PACKAGE.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference.

   RETURN (list)
     A list of <lint-issue> objects as defined in (apps packages types)."
  (list))


;;; TODO: Stub. Implement.
;;; https://bitbucket.org/sirgazil/guixsd-website/issues/42/
(define (package-patches package)
  "Return the list of patches for the given PACKAGE.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference.

   RETURN (list)
     A list of <link> objects as defined in (apps packages types)
     representing patches."
  (list))


(define (package-url-path package)
  "Return a URL path for the PACKAGE in the form packages/NAME-VERSION/.

   PACKAGE (<package>)
     A package object as defined in the GNU Guix API reference."
  (url-path-join "packages"
		 (string-append (package-name package)
				"-"
				(package-version package))))


(define (packages/group-by-letter packages)
  "Return a list of alphabetically grouped packages.

  PACKAGES (list)
    A list of package objects as defined in the GNU Guix API reference.

  RETURN (list)
    A list of lists of packages where each list corresponds to the
    packages whose name starts with a specific letter."
  (define (starts-with-digit? package)
    (char-set-contains? char-set:digit
                        (string-ref (package-name package) 0)))

  (define (starts-with-letter? letter)
    (let ((letter (string-downcase letter)))
      (lambda (package)
        (string-prefix? letter (package-name package)))))

  (map (lambda (letter)
         (match letter
           ("0-9"
            (cons letter (filter starts-with-digit? packages)))
           (_
            (cons letter
                  (filter (starts-with-letter? letter) packages)))))
       alphabet))
