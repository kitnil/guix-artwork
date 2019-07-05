;;; GNU Guix web site
;;; Copyright © 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

;; Run 'guix build -f .guix.scm' to build the web site.

(use-modules (guix) (gnu)
             (guix modules)
             (guix git-download)
             (ice-9 match))

(define this-directory
  (dirname (current-filename)))

(define source
  (local-file this-directory "guix-web-site"
              #:recursive? #t
              #:select? (git-predicate this-directory)))

(define (package+propagated-inputs package)
  (match (package-transitive-propagated-inputs package)
    (((labels packages) ...)
     (cons package packages))))

(define build
  (with-extensions (append (package+propagated-inputs
                            (specification->package "guix"))
                           (package+propagated-inputs
                            (specification->package "guile-syntax-highlight")))
    (with-imported-modules (source-module-closure
                            '((guix build utils)))
      #~(begin
          (use-modules (guix build utils))

          (copy-recursively #$source ".")

          ;; For Haunt.
          (setenv "GUILE_LOAD_PATH" (string-join %load-path ":"))
          (setenv "GUILE_LOAD_COMPILED_PATH"
                  (string-join %load-compiled-path ":"))

          ;; So we can read/write UTF-8 files.
          (setenv "GUIX_LOCPATH"
                  #+(file-append (specification->package "glibc-utf8-locales")
                                 "/lib/locale"))
          (setenv "LC_ALL" "en_US.utf8")

          ;; Use a sane default.
          (setenv "XDG_CACHE_HOME" "/tmp/.cache")

          ;; Choose the layout for guix.gnu.org.
          (setenv "GUIX_WEB_SITE_INFO" "t")

          (when (zero? (system* #+(file-append (specification->package "haunt")
                                               "/bin/haunt")
                                "build"))
            (mkdir-p #$output)
            (copy-recursively "/tmp/gnu.org/software/guix" #$output))))))

(computed-file "guix-web-site" build)

;; Local Variables:
;; eval: (put 'let-package 'scheme-indent-function 1)
;; End:
