;;; GuixSD website --- GNU's advanced distro website
;;; Initially written by sirgazil who waves all
;;; copyright interest on this file.

(define-module (tests apps packages utils)
  #:use-module (apps packages types)
  #:use-module (apps packages utils)
  #:use-module (guix packages)
  #:use-module (srfi srfi-64))


;;;
;;; Constants.
;;;

(define SUITE_NAME "apps-packages-utils")



;;;
;;; Test suite.
;;;

(test-begin SUITE_NAME)

;;; FIXME: Rewrite with real Guix packages in mind.
;;;
;; (test-group
;;  "[procedure] package-issues?"

;;  (test-equal
;;   "Return false if the package has no lint nor build issues."
;;   (package-issues? (package #:name "arau"))
;;   #false)

;;  (test-equal
;;   "Return true if the package has lint issues."
;;   (package-issues? (package #:name "arau"
;; 			    #:lint-issues '((lint-issue "A" "...")
;; 					    (lint-issue "B" "...")
;; 					    (lint-issue "C" "..."))))
;;   #true)

;;  (test-equal
;;   "Return true if the package has build issues."
;;   (package-issues? (package #:name "kiwi" #:build-issues '(""))) ; FIXME: Pass a real issue object.
;;   #true))


;;; FIXME: Rewrite with real Guix packages in mind.
;;;
;; (test-group
;;  "[procedure] package-url-path"

;;  (test-equal
;;   "Return the correct URL path to the package."
;;   (package-url-path (package #:name "arau" #:version "1.0.0"))
;;   "packages/arau-1.0.0"))


;;; FIXME: Rewrite with real Guix packages in mind.
;;;
;; (test-group
;;  "[procedure] packages/group-by-letter"

;;  (test-equal
;;   "Return an empty list if there are no packages."
;;   (packages/group-by-letter '())
;;   '())

;;  (test-equal
;;   "Group packages by letter."
;;   (packages/group-by-letter (list (package #:name "agua")
;; 				  (package #:name "azul")
;; 				  (package #:name "fuego")
;; 				  (package #:name "tierra")))
;;   (list
;;    (cons "0-9" '())
;;    (cons "A" (list (package #:name "agua") (package #:name "azul")))
;;    (cons "B" '())
;;    (cons "C" '())
;;    (cons "D" '())
;;    (cons "E" '())
;;    (cons "F" (list (package #:name "fuego")))
;;    (cons "G" '())
;;    (cons "H" '())
;;    (cons "I" '())
;;    (cons "J" '())
;;    (cons "K" '())
;;    (cons "L" '())
;;    (cons "M" '())
;;    (cons "N" '())
;;    (cons "O" '())
;;    (cons "P" '())
;;    (cons "Q" '())
;;    (cons "R" '())
;;    (cons "S" '())
;;    (cons "T" (list (package #:name "tierra")))
;;    (cons "U" '())
;;    (cons "V" '())
;;    (cons "W" '())
;;    (cons "X" '())
;;    (cons "Y" '())
;;    (cons "Z" '()))))


(test-end SUITE_NAME)
