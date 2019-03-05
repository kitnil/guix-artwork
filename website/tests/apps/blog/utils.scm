;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps blog utils)
  #:use-module (apps blog utils)
  #:use-module (haunt skribe utils)
  #:use-module (haunt post)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-blog-utils")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)


(test-group
 "[procedure] post-groups->tag-list"

 (test-equal
  "Return an empty list if there are no grouped posts."
  '()
  (post-groups->tag-list '()))

 (test-equal
  "Return the list of tag names from the grouped posts."
  '("Scheme API" "Unit tests" "Releases")
  (post-groups->tag-list
   (list
    (cons "Scheme API" '())
    (cons "Unit tests" '())
    (cons "Releases" '())))))


(test-group
 "[procedure] post-url-path"

 (test-equal
  "Return the correct URL path to the post."
  "blog/2017/hello-world"
  (post-url-path (make-post "hello.md"
			    `((title . "Hello World!")
			      (date . ,(make-date* 2017 03 21))) '()))))


(test-group
 "[procedure] posts/latest"

 (test-equal
  "Return an empty list when there are no posts."
  '()
  (posts/latest '() 5))

 (test-equal
  "Return all posts when there are less posts than the required number."
  1
  (length (posts/latest (list (make-post "hello.md" '() '())) 3)))

 (test-equal
  "Return the required number of posts."
  2
  (length (posts/latest
	   (list
	    (make-post "hello.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "hola.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "bye.md" `((date . ,(make-date* 2017 03 21))) '()))
	   2)))

 (test-equal
  "Return all posts when there are less posts than the required number."
  1
  (length (posts/latest (list (make-post "hello.md" '() '())) 3)))

 (test-equal
  "Return the required number of posts sorted in reverse chronological order."
  (list
   (make-post "bye.md" `((date . ,(make-date* 2017 03 20))) '())
   (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 09))) '())
   (make-post "hola.md" `((date . ,(make-date* 2017 01 17))) '()))
  (posts/latest
   (list
    (make-post "hello.md" `((date . ,(make-date* 2015 12 01))) '())
    (make-post "hola.md" `((date . ,(make-date* 2017 01 17))) '())
    (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 09))) '())
    (make-post "bye.md" `((date . ,(make-date* 2017 03 20))) '()))
   3)))


(test-group
 "[procedure] tag-first?"

 (test-assert
  "The tag 'Alpha' goes before 'Gamma'."
  (tag-first? "Alpha" "Gamma"))

 (test-assert
  "The tag 'Zapato' does not go before 'Abeja'."
  (not (tag-first? "Zapato" "Abeja"))))


(test-group
 "[procedure] tag-system-path"

 (test-equal
  "Return the system path to the tag relative to the website dir."
  "blog/tags/programming-interfaces"
  (tag-system-path "Programming interfaces")))


(test-group
 "[procedure] tag-url-path"

 (test-equal
  "Return the URL path to the tag."
  "blog/tags/scheme-api"
  (tag-url-path "Scheme API")))


(test-end SUITE_NAME)
