(define-module (www utils)
  #:export (current-url-root
	    gnu.org-root

	    base-url
	    gnu-url
	    guix-url
	    static-base-url
	    css-url
	    image-url
	    thumb-url
	    screenshot-url
	    slides-url))


;;;
;;; URL variables.
;;;

(define current-url-root
  ;; Website local url prefix.
  (make-parameter "/software/guix"))

(define gnu.org-root
  ;; GNU's website url prefix.
  (make-parameter ""))


;;;
;;; URL linking.
;;;

(define (base-url location)
  (string-append (current-url-root) "/" location))

(define (gnu-url location)
  (string-append (gnu.org-root) "/" location))

(define (guix-url location)
  (string-append (gnu-url "software/guix/") location))

(define (static-base-url)
  (base-url "static/base/"))

(define (css-url file)
  (string-append (static-base-url) "css/" file))

(define (image-url file)
  (string-append (static-base-url) "img/" file))

(define (thumb-url file)
  (string-append (image-url "screenshots/") file))

(define (screenshot-url version file)
  (string-append (guix-url "screenshots/") version "/" file))

(define (slides-url file)
  (guix-url file))
