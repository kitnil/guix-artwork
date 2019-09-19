;;; GNU Guix web site
;;; Initially written by sirgazil who waives all
;;; copyright interest on this file.

(use-modules ((apps base builder) #:prefix base:)
	     ((apps blog builder) #:prefix blog:)
	     ((apps download builder) #:prefix download:)
             (apps i18n)
	     ((apps packages builder) #:prefix packages:)
	     (haunt asset)
             (haunt builder assets)
             (haunt reader)
	     (haunt reader commonmark)
             (haunt site)
             (ice-9 rdelim)
             (srfi srfi-1))

(site #:title "GNU Guix"
      #:domain "https://guix.gnu.org"
      #:build-directory "/tmp/gnu.org/software/guix"
      #:readers (list sxml-reader html-reader commonmark-reader)
      #:builders (builders->localized-builders
                  (list base:builder
                        blog:builder
                        download:builder
                        packages:builder
                        (static-directory "static"))))
