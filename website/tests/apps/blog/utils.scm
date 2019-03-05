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
  (post-groups->tag-list '())
  '())

 (test-equal
  "Return the list of tag names from the grouped posts."
  (post-groups->tag-list
   (list
    (cons "Scheme API" '())
    (cons "Unit tests" '())
    (cons "Releases" '())))
  '("Scheme API" "Unit tests" "Releases")))


(test-group
 "[procedure] post-url-path"

 (test-equal
  "Return the correct URL path to the post."
  (post-url-path (make-post "hello.md"
			    `((title . "Hello World!")
			      (date . ,(make-date* 2017 03 21))) '()))
  "blog/2017/hello-world"))


(test-group
 "[procedure] posts/latest"

 (test-equal
  "Return an empty list when there are no posts."
  (posts/latest '() 5)
  '())

 (test-equal
  "Return all posts when there are less posts than the required number."
  (length (posts/latest (list (make-post "hello.md" '() '())) 3))
  1)

 (test-equal
  "Return the required number of posts."
  (length (posts/latest
	   (list
	    (make-post "hello.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "hola.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 21))) '())
	    (make-post "bye.md" `((date . ,(make-date* 2017 03 21))) '()))
	   2))
  2)

 (test-equal
  "Return all posts when there are less posts than the required number."
  (length (posts/latest (list (make-post "hello.md" '() '())) 3))
  1)

 (test-equal
  "Return the required number of posts sorted in reverse chronological order."
  (posts/latest
   (list
    (make-post "hello.md" `((date . ,(make-date* 2015 12 01))) '())
    (make-post "hola.md" `((date . ,(make-date* 2017 01 17))) '())
    (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 09))) '())
    (make-post "bye.md" `((date . ,(make-date* 2017 03 20))) '()))
   3)
  (list
   (make-post "bye.md" `((date . ,(make-date* 2017 03 20))) '())
   (make-post "konchiwa.md" `((date . ,(make-date* 2017 03 09))) '())
   (make-post "hola.md" `((date . ,(make-date* 2017 01 17))) '()))))


(test-group
 "[procedure] tag-first?"

 (test-equal
  "The tag 'Alpha' goes before 'Gamma'."
  (tag-first? "Alpha" "Gamma")
  #true)

 (test-equal
  "The tag 'Zapato' does not go before 'Abeja'."
  (tag-first? "Zapato" "Abeja")
  #false))


(test-group
 "[procedure] tag-system-path"

 (test-equal
  "Return the system path to the tag relative to the website dir."
  (tag-system-path "Programming interfaces")
  "blog/tags/programming-interfaces"))


(test-group
 "[procedure] tag-url-path"

 (test-equal
  "Return the URL path to the tag."
  (tag-url-path "Scheme API")
  "blog/tags/scheme-api"))


(test-end SUITE_NAME)
