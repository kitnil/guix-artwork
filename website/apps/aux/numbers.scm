;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps aux numbers)
  #:use-module (srfi srfi-1)
  #:export (minus-one
	    plus-one
	    range))


(define (minus-one n)
  "Return N-1."
  (- n 1))


(define (plus-one n)
  "Return N+1."
  (+ n 1))


(define (range a b)
  "Return the list of integers in the range [A, B].

   A (integer)

   B (integer)

   RETURN VALUE (list of integers)
     For example, for the range [-2, 3], return
     (list -2 -1 0 1 2 3)."
  (cond ((zero? (- a b)) (cons a (list)))
	(else (cons a (range (plus-one a) b)))))
