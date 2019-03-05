;;; GNU Guix web site
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps aux lists)
  #:use-module (apps aux lists)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-aux-lists")


;;;
;;; Helper variables.
;;;

(define fruit-bag (list "uva" "mora" "mango" "kiwi"))


;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)

(test-group
 "[procedure] list-group"
 (test-equal
  "Grouping elements of an empty list results in an empty list."
  (list)
  (list-group (list) 5))
 (test-equal
  "Group a list of four in sets of two."
  (list (list "Onnet" "Twoson") (list "Threed" "Summers"))
  (list-group (list "Onnet" "Twoson" "Threed" "Summers") 2))
 (test-equal
  "Group a list of five in sets of three."
  (list (list "Onnet" "Twoson" "Threed") (list "Summers" "Scaraba"))
  (list-group (list "Onnet" "Twoson" "Threed" "Summers" "Scaraba") 3)))


(test-group
 "[procedure] list-slice"
 (test-equal
  "Slice from index A to index B."
  (list "uva" "mora")
  (list-slice fruit-bag 0 2))
 (test-equal
  "Slice from index A to index B out of range."
  (list "mora" "mango" "kiwi")
  (list-slice fruit-bag 1 7))
 (test-equal
  "Slice from index A."
  (list "mango" "kiwi")
  (list-slice fruit-bag 2)))


(test-group
 "[procedure] rest"
 (test-equal
  "Empty list results in itself."
  (list)
  (rest (list)))
 (test-equal
  "Rest of single-element list is empty list."
  (list)
  (rest (list "Hello")))
 (test-equal
  "Rest of list of elements is the list but its first element."
  (list "Hola" "Ei")
  (rest (list "Hello" "Hola" "Ei"))))


(test-group
 "[procedure] separate"

 (test-equal
  "Don't add separators to empty lists."
  (list)
  (separate (list) "|"))

 (test-equal
  "Don't add separators to one-element lists."
  (list "mango")
  (separate (list "mango") "|"))

 (test-equal
  "Separate the elements of a list."
  (list "mango" "|" "kiwi" "|" "papaya" "|" "lemon")
  (separate (list "mango" "kiwi" "papaya" "lemon") "|")))


(test-end SUITE_NAME)
