;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (apps aux lists)
  #:use-module (apps aux numbers)
  #:use-module (srfi srfi-1)
  #:export (list-group
	    list-slice
	    rest
	    separate))


(define (list-group los limit)
  (map (lambda (index)
	 (list-slice los index (+ index limit)))
       ;; TODO: Use a skip-count procedure instead of iota.
       (iota (ceiling (/ (length los) limit)) 0 limit)))


(define* (list-slice los index-a #:optional (index-b #false))
  (let ((start index-a)
	(end (if (or (not index-b) (> index-b (length los)))
		 (- (length los) 1)
		 (- index-b 1))))
    (map (lambda (index)
	   (list-ref los index))
	 (range index-a end))))


(define (rest los)
  (cond ((<= (length los) 1) (list))
	(else (list-tail los 1))))


(define (separate los separator)
  "Return a list with the elements of LOS separated by SEPARATOR.

   LOS (list)
     A list of s-expressions.

   SEPARATOR (s-expression)
     Any s-expression that will be added between the elements of the
     given list.

   RETURN VALUE (list)
     A list of s-expressions."
  (cond ((or (null? los) (= (length los) 1)) los)
	(else
	 (cons (first los)
	       (cons separator (separate (rest los) separator))))))
