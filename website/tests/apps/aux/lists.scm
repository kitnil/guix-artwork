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
  (list-group (list) 5)
  (list))
 (test-equal
  "Group a list of four in sets of two."
  (list-group (list "Onnet" "Twoson" "Threed" "Summers") 2)
  (list (list "Onnet" "Twoson") (list "Threed" "Summers")))
 (test-equal
  "Group a list of five in sets of three."
  (list-group (list "Onnet" "Twoson" "Threed" "Summers" "Scaraba") 3)
  (list (list "Onnet" "Twoson" "Threed") (list "Summers" "Scaraba"))))


(test-group
 "[procedure] list-slice"
 (test-equal
  "Slice from index A to index B."
  (list-slice fruit-bag 0 2)
  (list "uva" "mora"))
 (test-equal
  "Slice from index A to index B out of range."
  (list-slice fruit-bag 1 7)
  (list "mora" "mango" "kiwi"))
 (test-equal
  "Slice from index A."
  (list-slice fruit-bag 2)
  (list "mango" "kiwi")))


(test-group
 "[procedure] rest"
 (test-equal
  "Empty list results in itself."
  (rest (list))
  (list))
 (test-equal
  "Rest of single-element list is empty list."
  (rest (list "Hello"))
  (list))
 (test-equal
  "Rest of list of elements is the list but its first element."
  (rest (list "Hello" "Hola" "Ei"))
  (list "Hola" "Ei")))


(test-group
 "[procedure] separate"

 (test-equal
  "Don't add separators to empty lists."
  (separate (list) "|")
  (list))

 (test-equal
  "Don't add separators to one-element lists."
  (separate (list "mango") "|")
  (list "mango"))

 (test-equal
  "Separate the elements of a list."
  (separate (list "mango" "kiwi" "papaya" "lemon") "|")
  (list "mango" "|" "kiwi" "|" "papaya" "|" "lemon")))


(test-end SUITE_NAME)
