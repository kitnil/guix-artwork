;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps packages utils)
  #:use-module (apps aux web)
  #:use-module (apps base utils)
  #:use-module (apps packages data)
  #:use-module (apps packages types)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (location->ilink
	    package-build-issues
	    package-issues?
	    package-lint-issues
	    package-patches
	    package-url-path
	    packages/group-by-letter))


;;;
;;; Helper procedures.
;;;

(define (location->ilink loc)
  "Convert the given location LOC into an Ilink.

   LOC (<location>)
     A location object as defined in the GNU Guix API reference.

   RETURN (<ilink>)
     An Ilink object as defined in (apps packages types)."
  (ilink (basename (location-file loc))
	 (guix-git-tree-url
	  (url-path-join (location-file loc)
			 (string-append "#n"
					(number->string (location-line loc)))))))


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


;;; TODO: Dummy. Implement it.
;;; (https://bitbucket.org/sirgazil/guixsd-website/issues/38/)
(define (packages/group-by-letter packages)
  "Return a list of alphabetically grouped packages.

  PACKAGES (list)
    A list of package objects as defined in the GNU Guix API reference.

  RETURN (list)
    A list of lists of packages where each list corresponds to the
    packages whose name starts with a specific letter."
  (cond ((null? packages) '())
	(else
	 (map (lambda (letter) (cons letter packages)) alphabet))))
