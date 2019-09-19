;;; GNU Guix web site
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
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

(define-module (sexp-xgettext)
  #:use-module (ice-9 match)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1) ;lists
  #:use-module (srfi srfi-9) ;records
  #:export (set-complex-keywords!
            set-simple-keywords!
            sgettext
            sngettext
            spgettext
            snpgettext
            %linguas))

(define %complex-keywords
  ;; Use set-complex-keywords! to change this to a list of keywords
  ;; for sexp-xgettext functions other than sgettext.
  (make-parameter '()))

(define (set-complex-keywords! kw)
  (%complex-keywords kw))

(define %simple-keywords
  ;; Use set-simple-keywords! to change this to a list of keywords
  ;; for sgettext.
  (make-parameter '()))

(define (set-simple-keywords! kw)
  (%simple-keywords kw))

(define (gettext-keyword? id)
  (or (member id (%complex-keywords))
      (member id (%simple-keywords))))

;;COPIED FROM scripts/sexp-xgettext.scm:
(define* (tag counter prefix #:key (flavor 'start))
  "Formats the number COUNTER as a tag according to FLAVOR, which is
either 'start, 'end or 'empty for a start, end or empty tag,
respectively."
  (string-append "<"
                 (if (eq? flavor 'end) "/" "")
                 prefix
                 (number->string counter)
                 (if (eq? flavor 'empty) "/" "")
                 ">"))
;;END COPIED FROM scripts/sexp-xgettext.scm

;;ADAPTED FROM scripts/sexp-xgettext.scm
(define-record-type <construct-fold-state>
  (make-construct-fold-state msgid-string maybe-part counter)
  construct-fold-state?
  ;; msgid constructed so far; #f if none, "" if only empty string
  (msgid-string construct-fold-state-msgid-string)
  ;; only append this if string follows:
  (maybe-part construct-fold-state-maybe-part)
  ;; counter for next tag:
  (counter construct-fold-state-counter))
;;END ADAPTED FROM scripts/sexp-xgettext.scm

(define (sexp->msgid exp)
  "Return the msgid as constructed by construct-msgid-and-po-entries
in scripts/sexp-xgettext.scm from the expression EXP."
  (let loop ((exp exp)
             (prefix ""))
    (match exp
      (() "")
      ((or ('quote inner-exp)
           ('quasiquote inner-exp)
           ('unquote inner-exp)
           ('unquote-splicing inner-exp))
       (loop inner-exp prefix))
      ((first-component . components)
       (cond
        ((gettext-keyword? first-component)
         (error "Double-marked for translation." exp))
        (else
         (or
          (construct-fold-state-msgid-string
           (fold
            (lambda (component prev-state)
              (match prev-state
                (($ <construct-fold-state> msgid-string maybe-part counter)
                 (let inner-loop ((exp component))
                   (match exp
                     ((or (? symbol?) (? keyword?))
                      (if (not msgid-string)
                          ;; ignore symbols at the beginning
                          prev-state
                          ;; else make a tag for the symbol
                          (make-construct-fold-state
                           msgid-string
                           (string-append maybe-part
                                          (tag counter prefix #:flavor 'empty))
                           (1+ counter))))
                     ((? string?)
                      (make-construct-fold-state
                       (string-append (or msgid-string "")
                                      maybe-part exp)
                       "" counter))
                     ((? list?)
                      (match exp
                        (() ;ignore empty list
                         prev-state)
                        ((or (singleton)
                             ('quote singleton)
                             ('quasiquote singleton)
                             ('unquote singleton)
                             ('unquote-splicing singleton))
                         (inner-loop singleton))
                        ((components ...)
                         (cond
                          ((and (not (null? components))
                                (member (car components) (%simple-keywords)))
                           ;; if marked for translation, insert inside tag
                           (make-construct-fold-state
                            (string-append (or msgid-string "")
                                           maybe-part
                                           (tag counter prefix #:flavor 'start)
                                           (loop (cadr components)
                                                 (string-append
                                                  prefix
                                                  (number->string counter)
                                                  "."))
                                           (tag counter prefix #:flavor 'end))
                            ""
                            (1+ counter)))
                          ;; else ignore if first
                          ((not msgid-string)
                           prev-state)
                          ;; else make empty tag
                          (else (make-construct-fold-state
                                 msgid-string
                                 (string-append
                                  maybe-part
                                  (tag counter prefix #:flavor 'empty))
                                 (1+ counter))))))))))))
            (make-construct-fold-state #f "" 1)
            exp))
          (error "Marking for translation yields empty msgid." exp)))))
      ((? string?) exp)
      (else (error "Single symbol marked for translation." exp)))))

(define-record-type <deconstruct-fold-state>
  (make-deconstruct-fold-state tagged maybe-tagged counter)
  deconstruct-fold-state?
  ;; XML-tagged expressions as an association list name->expression:
  (tagged deconstruct-fold-state-tagged)
  ;; associate this not-yet-tagged expression with pre if string
  ;; follows, with post if not:
  (maybe-tagged deconstruct-fold-state-maybe-tagged)
  ;; counter for next tag:
  (counter deconstruct-fold-state-counter))

(define (deconstruct exp msgstr)
  "Return an s-expression like EXP, but filled with the content from
MSGSTR."
  (define (find-empty-element msgstr name)
    "Returns the regex match structure for the empty tag for XML
element of type NAME inside MSGSTR.  If the element does not exist or
is more than the empty tag, #f is returned."
    (string-match (string-append "<" (regexp-quote name) "/>") msgstr))
  (define (find-element-with-content msgstr name)
    "Returns the regex match structure for the non-empty XML element
of type NAME inside MSGSTR.  Submatch 1 is its content.  If the
element does not exist or is just the empty tag, #f is returned."
    (string-match (string-append "<" (regexp-quote name) ">"
                                 "(.*)"
                                 "</" (regexp-quote name) ">")
                  msgstr))
  (define (get-first-element-name prefix msgstr)
    "Returns the name of the first XML element in MSGSTR whose name
begins with PREFIX, or #f if there is none."
    (let ((m (string-match
              (string-append "<(" (regexp-quote prefix) "[^>/.]+)/?>") msgstr)))
      (and m (match:substring m 1))))
  (define (prefix+counter prefix counter)
    "Returns PREFIX with the number COUNTER appended."
    (string-append prefix (number->string counter)))
  (let loop ((exp exp)
             (msgstr msgstr)
             (prefix ""))
    (define (unwrap-marked-expression exp)
      "Returns two values for an expression EXP containing a (possibly
quoted/unquoted) marking for translation with a simple keyword at its
root.  The first return value is a list with the inner expression, the
second is a procedure to wrap the processed inner expression in the
same quotes or unquotes again."
      (match exp
        (('quote inner-exp)
         (receive (unwrapped quotation)
             (unwrap-marked-expression inner-exp)
           (values unwrapped
                   (lambda (res)
                     (list 'quote (quotation res))))))
        (('quasiquote inner-exp)
         (receive (unwrapped quotation)
             (unwrap-marked-expression inner-exp)
           (values unwrapped
                   (lambda (res)
                     (list 'quasiquote (quotation res))))))
        (('unquote inner-exp)
         (receive (unwrapped quotation)
             (unwrap-marked-expression inner-exp)
           (values unwrapped
                   (lambda (res)
                     (list 'unquote (quotation res))))))
        (('unquote-splicing inner-exp)
         (receive (unwrapped quotation)
             (unwrap-marked-expression inner-exp)
           (values unwrapped
                   (lambda (res)
                     (list 'unquote-splicing (quotation res))))))
        ((marking . rest) ;list with marking as car
         ;; assume arg to translate is first argument to marking:
         (values (list-ref rest 0) identity))))
    (define (assemble-parenthesized-expression prefix tagged)
      "Returns a parenthesized expression deconstructed from MSGSTR
with the meaning of XML elements taken from the name->expression
association list TAGGED.  The special tags [prefix]pre and
[prefix]post are associated with a list of expressions before or after
all others in the parenthesized expression with the prefix,
respectively, in reverse order."
      (append ;prepend pre elements to what is in msgstr
       (reverse (or (assoc-ref tagged (string-append prefix "pre")) '()))
       (let assemble ((rest msgstr))
         (let ((name (get-first-element-name prefix rest)))
           (cond
            ((and name (find-empty-element rest name)) =>
             ;; first XML element in rest is empty element
             (lambda (m)
               (cons*
                (match:prefix m) ;prepend string before name
                (assoc-ref tagged name) ;and expression for name
                (assemble (match:suffix m)))))
            ((and name (find-element-with-content rest name)) =>
             ;; first XML element in rest has content
             (lambda (m)
               (receive (unwrapped quotation)
                   (unwrap-marked-expression (assoc-ref tagged name))
                 (cons*
                  (match:prefix m) ;prepend string before name
                  ;; and the deconstructed element with the content as msgstr:
                  (quotation
                   (loop
                    unwrapped
                    (match:substring m 1)
                    (string-append name ".")))
                  (assemble (match:suffix m))))))
            (else
             ;; there is no first element
             (cons
              rest ;return remaining string
              (reverse ;and post expressions
               (or (assoc-ref tagged (string-append prefix "post")) '())))))))))
    (match exp
      (() '())
      (('quote singleton)
       (cons 'quote (list (loop singleton msgstr prefix))))
      (('quasiquote singleton)
       (cons 'quasiquote (list (loop singleton msgstr prefix))))
      (('unquote singleton)
       (cons 'unquote (list (loop singleton msgstr prefix))))
      (('unquote-splicing singleton)
       (cons 'unquote-splicing (list (loop singleton msgstr prefix))))
      ((singleton)
       (list (loop singleton msgstr prefix)))
      ((first-component . components)
       (cond
        ((gettext-keyword? first-component)
         ;; another marking for translation
         ;; -> should be an error anyway; just retain exp
         exp)
        (else
         ;; This handles a single level of a parenthesized expression.
         ;; assemble-parenthesized-expression will call loop to
         ;; recurse to deeper levels.
         (let ((tagged-state
                (fold
                 (lambda (component prev-state)
                   (match prev-state
                     (($ <deconstruct-fold-state> tagged maybe-tagged counter)
                      (let inner-loop ((exp component) ;sexp to handle
                                       (quoting identity)) ;for wrapping state
                        (define (tagged-with-maybes)
                          "Returns the value of tagged after adding
all maybe-tagged expressions.  This should be used as the base value
for tagged when a string or marked expression is seen."
                          (match counter
                            (#f
                             (alist-cons (string-append prefix "pre")
                                         maybe-tagged
                                         tagged))
                            ((? number?)
                             (let accumulate ((prev-counter counter)
                                              (maybes (reverse maybe-tagged)))
                               (match maybes
                                 (() tagged)
                                 ((head . tail)
                                  (alist-cons
                                   (prefix+counter prefix prev-counter)
                                   head
                                   (accumulate (1+ prev-counter) tail))))))))
                        (define (add-maybe exp)
                          "Returns a deconstruct-fold-state with EXP
added to maybe-tagged.  This should be used for expressions that are
neither strings nor marked for translation with a simple keyword."
                          (make-deconstruct-fold-state
                           tagged
                           (cons (quoting exp) maybe-tagged)
                           counter))
                        (define (counter-with-maybes)
                          "Returns the old counter value incremented
by one for each expression in maybe-tagged.  This should be used
together with tagged-with-maybes."
                          (match counter
                            ((? number?)
                             (+ counter (length maybe-tagged)))
                            (#f
                             1)))
                        (define (add-tagged exp)
                          "Returns a deconstruct-fold-state with an
added association in tagged from the current counter to EXP.  If
MAYBE-TAGGED is not empty, associations for its expressions are added
to pre or their respective counter.  This should be used for
expressions marked for translation with a simple keyword."
                          (let ((c (counter-with-maybes)))
                            (make-deconstruct-fold-state
                             (alist-cons
                              (prefix+counter prefix c)
                              (quoting exp)
                              (tagged-with-maybes))
                             '()
                             (1+ c))))
                        (match exp
                          (('quote inner-exp)
                           (inner-loop inner-exp
                                       (lambda (res)
                                         (list 'quote res))))
                          (('quasiquote inner-exp)
                           (inner-loop inner-exp
                                       (lambda (res)
                                         (list 'quasiquote res))))
                          (('unquote inner-exp)
                           (inner-loop inner-exp
                                       (lambda (res)
                                         (list 'unquote res))))
                          (('unquote-splicing inner-exp)
                           (inner-loop inner-exp
                                       (lambda (res)
                                         (list 'unquote-splicing res))))
                          (((? gettext-keyword?) . rest)
                           (add-tagged exp))
                          ((or (? symbol?) (? keyword?) (? list?))
                           (add-maybe exp))
                          ((? string?)
                           ;; elements in maybe-tagged appear between strings
                           (let ((c (counter-with-maybes)))
                             (make-deconstruct-fold-state
                              (tagged-with-maybes)
                              '()
                              c))))))))
                 (make-deconstruct-fold-state '() '() #f)
                 exp)))
           (match tagged-state
             (($ <deconstruct-fold-state> tagged maybe-tagged counter)
              (assemble-parenthesized-expression
               prefix
               (match maybe-tagged
                 (() tagged)
                 (else ;associate maybe-tagged with pre or post
                  (alist-cons
                   (cond ;if there already is a pre, use post
                    ((assoc-ref tagged (string-append prefix "pre"))
                     (string-append prefix "post"))
                    (else (string-append prefix "pre")))
                   maybe-tagged
                   tagged))))))))))
      ((? string?) msgstr)
      (else (error "Single symbol marked for translation." exp)))))

;; NOTE: The sgettext macros have no hygiene because they use
;; datum->syntax and do not preserve the semantics of anything looking
;; like an sgettext macro.  This is an exceptional use case; do not
;; try this at home.

(define (sgettext x)
  "After choosing an identifier for marking s-expressions for
translation, make it usable by defining a macro with it calling
sgettext.  If for example the chosen identifier is G_,
use (define-syntax G_ sgettext)."
  (syntax-case x ()
    ((id exp)
     (let* ((msgid (sexp->msgid (syntax->datum #'exp)))
            (new-exp (deconstruct (syntax->datum #'exp)
                                  (gettext msgid))))
       (datum->syntax #'id new-exp)))))

;; gettext’s share/gettext/gettext.h tells us we can prepend a msgctxt
;; and #\eot before a msgid in a gettext call.

(define (spgettext x)
  "After choosing an identifier for behavior similar to pgettext:1c,2,
make it usable like (define-syntax C_ spgettext)."
  (syntax-case x ()
    ((id msgctxt exp)
     (let* ((gettext-context-glue #\eot) ;as defined in gettext.h
            (lookup (string-append (syntax->datum #'msgctxt)
                                   (string gettext-context-glue)
                                   (sexp->msgid (syntax->datum #'exp))))
            (msgstr (car (reverse (string-split (gettext lookup)
                                                gettext-context-glue))))
            (new-exp (deconstruct (syntax->datum #'exp)
                                  msgstr)))
       (datum->syntax #'id new-exp)))))

(define %plural-numbers
  ;; Hard-coded list of input numbers such that for each language’s
  ;; plural formula, for each possible output grammatical number,
  ;; there is an n among %plural-numbers that yields this output (for
  ;; any language documented when running “info "(gettext) Plural
  ;; forms"”), except 1 is omitted from this list because it is a
  ;; special case for sngettext.  That is, calling ngettext with each
  ;; number from %plural-numbers and with 1 in any locale is
  ;; guaranteed to return each plural form at least once.  It would be
  ;; more resilient towards new languages if instead of hard-coding we
  ;; computed this from the Plural-Forms in the MO file header entry,
  ;; but that is not worth the incurred code complexity.
  '(0 2 3 11 100))

(define (sngettext x)
  "After choosing an identifier for behavior similar to ngettext:1,2,
make it usable like (define-syntax N_ sngettext).  sngettext takes
into account that not all languages have only singular and plural
forms."
  (syntax-case x ()
    ((id exp1 exp2 n)
     (let* ((msgid1 (sexp->msgid (syntax->datum #'exp1)))
            (msgid2 (sexp->msgid (syntax->datum #'exp2)))
            (msgstr1 (ngettext msgid1 msgid2 1))
            (result (acons ;return an association list msgstr->deconstructed
                     ;; msgstr for n=1:
                     msgstr1
                     `(,'unquote ,(deconstruct (syntax->datum #'exp1)
                                               msgstr1))
                     ;; other msgstr for n of each plural form:
                     (map
                      (lambda (n)
                        (let ((msgstr (ngettext msgid1 msgid2 n)))
                          (cons msgstr `(,'unquote
                                         ,(deconstruct (syntax->datum #'exp2)
                                                       msgstr)))))
                      %plural-numbers))))
       (datum->syntax
        #'id
        `(,assoc-ref (,'quasiquote ,result)
                     (,ngettext ,msgid1 ,msgid2 ,(syntax->datum #'n))))))))

(define (snpgettext x)
  "After choosing an identifier for behavior similar to npgettext:1c,2,3,
make it usable like (define-syntax NC_ snpgettext)."
  (syntax-case x ()
    ((id msgctxt exp1 exp2 n)
     (let* ((gettext-context-glue #\eot) ;as defined in gettext.h
            (msgid1 (string-append (syntax->datum #'msgctxt)
                                   (string gettext-context-glue)
                                   (sexp->msgid (syntax->datum #'exp1))))
            ;; gettext.h implementation shows: msgctxt is only part of msgid1.
            (msgid2 (sexp->msgid (syntax->datum #'exp2)))
            (msgstr1 (car
                      (reverse
                       (string-split
                        (ngettext msgid1 msgid2 1)
                        gettext-context-glue))))
            (result (acons ;return an association list msgstr->deconstructed
                     ;; msgstr for n=1:
                     msgstr1
                     `(,'unquote ,(deconstruct (syntax->datum #'exp1)
                                               msgstr1))
                     ;; other msgstr for n of each plural form:
                     (map
                      (lambda (n)
                        (let ((msgstr (car
                                       (reverse
                                        (string-split
                                         (ngettext msgid1 msgid2 n)
                                         gettext-context-glue)))))
                          (cons msgstr `(,'unquote
                                         ,(deconstruct (syntax->datum #'exp2)
                                                       msgstr)))))
                      %plural-numbers))))
       (datum->syntax
        #'id
        `(,assoc-ref (,'quasiquote ,result)
                     (,car
                      (,reverse
                       (,string-split
                        (,ngettext ,msgid1 ,msgid2 ,(syntax->datum #'n))
                        ,gettext-context-glue)))))))))

(define %linguas
  (with-input-from-file "po/LINGUAS"
    (lambda _
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '()
            ;; else read linguas before comment
            (let ((before-comment (car (string-split line #\#))))
              (append
               (map match:substring (list-matches "[^ \t]+" before-comment))
               (loop (read-line)))))))))
