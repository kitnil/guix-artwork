;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps aux system)
  #:export (path-join))


;;;
;;; Procedures.
;;;

(define (path-join . parts)
  "Return a system path composed of the given PARTS.

   PARTS (strings)
     A succession of strings representing parts of a file system path.

     To indicate an absolute path, use an empty string as the first
     part. For example:

     (path-join '' 'docs' 'manual')
     => '/docs/manual'

     To end the path with a slash, use an empty string as the last
     part. For example:

     (path-join '' 'docs' 'manual' '')
     => '/docs/manual/'

   RETURN VALUE (string)
     A string representing a file system path."
  (cond ((equal? parts '("")) "/") ; Root directory
	(else (string-join parts file-name-separator-string))))
