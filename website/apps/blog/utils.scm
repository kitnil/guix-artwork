;;; GNU Guix web site
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of the GNU Guix web site.
;;;
;;; The GNU Guix web site is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; The GNU Guix web site is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with the GNU Guix web site.  If not, see <http://www.gnu.org/licenses/>.

(define-module (apps blog utils)
  #:use-module (apps aux lists)
  #:use-module (apps aux web)
  #:use-module (haunt post)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:use-module (syntax-highlight lexers)
  #:export (post-groups->tag-list
	    post-url-path
	    posts/latest
	    syntax-highlight
	    tag-first?
	    tag-system-path
	    tag-url-path))


(define (post-groups->tag-list groups)
  "Return a list of Haunt tags from the list of grouped posts.

   GROUPS (association list)
     An association list of tags mapped to posts, as returned by the
     posts/group-by-tag procedure from (haunt post) module."
  (cond ((null? groups) '())
	(else
	 (cons (car (first groups))
	       (post-groups->tag-list (rest groups))))))


(define (post-url-path post)
  "Return a URL path for the POST in the form blog/YYYY/POST-SLUG.

   POST (<post>)
     A post object as defined in (haunt post) module."
  (url-path-join "blog"
		 (date->string (post-date post) "~Y")
		 (post-slug post)))


(define (posts/latest posts n)
  "Return the latest N posts from the given list of posts."
  (let ((latest-posts (posts/reverse-chronological posts)))
    (cond
     ((null? posts) '())
     ((<= (length posts) n) latest-posts)
     (else (list-head latest-posts n)))))


(define (tag-first? tag-a tag-b)
  "Return true if TAG-A goes first than TAG-B alphabetically.

   This predicate is used for sorting tags.

   TAG-A, TAG-B (string)
     A tag as used by Haunt posts. For example: 'User interface'."
  (string<? (string-downcase tag-a) (string-downcase tag-b)))


(define (tag-system-path tag)
  "Return a system path for the TAG in the form blog/tags/TAG-SLUG.

   The path is relative to the website directory.

   TAG (string)
     A tag as used by Haunt posts. For example: 'Scheme API'."
  (string-append "blog/tags/" (slugify tag)))


(define (tag-url-path tag)
  "Return a URL path for the TAG in the form blog/tags/TAG-SLUG.

   TAG (string)
     A tag as used by Haunt posts. For example: 'Scheme API'."
  (url-path-join "blog" "tags" (slugify tag)))



;;;
;;; Syntax highlighting.
;;;

(define %default-special-prefixes
  '("define" "syntax"))

(define lex-scheme/guix
  ;; Specialized lexer for the Scheme we use in Guix.
  ;; TODO: Add #~, #$, etc.
  (make-scheme-lexer (cons* "with-imported-modules"
                            "gexp" "ungexp"
                            "ungexp-native" "ungexp-splicing"
                            "ungexp-native-splicing"
                            "mlet" "mlet*"
                            "match"
                            %default-special-symbols)
                     %default-special-prefixes))

(define (syntax-highlight sxml)
  "Recurse over SXML and syntax-highlight code snippets."
  (match sxml
    (('code ('@ ('class "language-scheme")) code-snippet)
     `(code ,(highlights->sxml
              (highlight lex-scheme/guix code-snippet))))
    ((tag ('@ attributes ...) body ...)
     `(,tag (@ ,@attributes) ,@(map syntax-highlight body)))
    ((tag body ...)
     `(,tag ,@(map syntax-highlight body)))
    ((? string? str)
     str)))
