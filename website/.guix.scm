;;; GuixSD website --- GNU's advanced distro website
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

;; Run 'guix build -f guix.scm' to build the web site.

(use-modules (guix) (gnu)
             (guix modules)
             (guix git-download))

(define source
  (local-file "." "guix-web-site"
              #:recursive? #t
              #:select? (git-predicate ".")))

(define-syntax let-package
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((var name) rest ...) body ...)
     (let ((var (specification->package name)))
       (let-package (rest ...) body ...)))))

(let-package ((guix "guix")
              (guile-json "guile-json")
              (haunt "haunt")
              (guile-commonmark "guile-commonmark")
              (guile-syntax-highlight "guile-syntax-highlight"))
  (with-imported-modules (source-module-closure
                          '((guix build utils)))
    #~(begin
        (use-modules (guix build utils))

        (copy-recursively #$source ".")

        ;; For Haunt.
        (setenv "GUILE_LOAD_PATH"
                (string-join (list #+guile-commonmark #+guile-json
                                   #+guile-syntax-highlight
                                   #+guix)
                             "/share/guile/site/2.2:"
                             'suffix))
        (setenv "GUILE_LOAD_COMPILED_PATH"
                #+(file-append guix "/lib/guile/2.2/site-ccache"))

        ;; So we can read/write UTF-8 files.
        (setenv "GUIX_LOCPATH"
                #+(file-append (specification->package "glibc-utf8-locales")
                               "/lib/locale"))
        (setenv "LC_ALL" "en_US.utf8")

        (and (zero? (system* #+(file-append haunt "/bin/haunt")
                             "build"))
             (begin
               (mkdir-p #$output)
               (copy-recursively "site" #$output))))))

;; Local Variables:
;; eval: (put 'let-package 'scheme-indent-function 1)
;; End:
