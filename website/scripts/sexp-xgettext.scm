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

(use-modules (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 peg)
             (ice-9 receive)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1) ;lists
             (srfi srfi-9) ;records
             (srfi srfi-19) ;date
             (srfi srfi-26)) ;cut

;;; This script imitates xgettext, but combines nested s-expressions
;;; in the input Scheme files to a single msgstr in the PO file.  It
;;; works by first reading the keywords specified on the command-line,
;;; then dealing with the remaining options using (ice-9 getopt-long).
;;; Then, it parses each Scheme file in the POTFILES file specified
;;; with --files-from and constructs po entries from it.  For parsing,
;;; a PEG is used instead of Scheme’s read, because we can extract
;;; comments with it.  The po entries are written to the PO file
;;; specified with the --output option.  Scheme code can then use the
;;; (sexp-xgettext) module to deconstruct the msgids looked up in the
;;; PO file via gettext.

(define-record-type <keyword-spec>
  (make-keyword-spec id sg pl c total xcomment)
  keyword-spec?
  (id keyword-spec-id) ;identifier
  (sg keyword-spec-sg) ;arg with singular
  (pl keyword-spec-pl) ;arg with plural
  (c keyword-spec-c) ;arg with msgctxt or 'mixed if sg is mixed msgctxt|singular
  (total keyword-spec-total) ;total number of args
  (xcomment keyword-spec-xcomment))

(define (complex-keyword-spec? keyword-spec)
  (match keyword-spec
    (($ <keyword-spec> _ _ #f #f _ #f) #f)
    (else #t)))

(define %keyword-specs
  ;; List of valid xgettext keyword options.
  ;; Read keywords from command-line options.
  (let loop ((opts (cdr (command-line)));command-line options from
                                        ;which to extract --keyword
                                        ;options
             (remaining-opts '()) ;unhandled opts
             (specs '()))
    (define (string->integer str)
      (if (string-match "[0-9]+" str)
          (string->number str)
          (error "Not a decimal integer.")))
    (define* (argnums->spec id #:optional (argnums '()))
      (let loop ((sg #f)
                 (pl #f)
                 (c #f)
                 (total #f)
                 (xcomment #f)
                 (argnums argnums))
        (match argnums
          (() (make-keyword-spec id
                                 (if sg sg 1)
                                 pl
                                 c
                                 total
                                 xcomment))
          ((arg . argnums)
           (cond
            ((string-suffix? "c" arg)
             (cond (c (error "c suffix clashes"))
                   (else
                    (let* ((number-str (string-drop-right arg 1))
                           (number (string->integer number-str)))
                      (loop sg pl number total xcomment argnums)))))
            ((string-suffix? "g" arg)
             (cond
              (sg (error "Only first argnum can have g suffix."))
              (c (error "g suffix clashes."))
              (else
               (let* ((number-str (string-drop-right arg 1))
                      (number (string->integer number-str)))
                 (loop number #f 'mixed total xcomment argnums)))))
            ((string-suffix? "t" arg)
             (cond (total (error "t suffix clashes"))
                   (else
                    (let* ((number-str (string-drop-right arg 1))
                           (number (string->integer number-str)))
                      (loop sg pl c number xcomment argnums)))))
            ((string-suffix? "\"" arg)
             (cond (xcomment (error "xcomment clashes"))
                   (else
                    (let* ((comment (substring arg
                                               1
                                               (- (string-length arg) 1))))
                      (loop sg pl c total comment argnums)))))
            (else
             (let* ((number (string->integer arg)))
               (if sg
                   (if pl
                       (error "Too many argnums.")
                       (loop sg number c total xcomment argnums))
                   (loop number #f c total xcomment argnums)))))))))

    (define (string->spec str) ;see `info xgettext`
      (match (string-split str #\:)
        ((id) (argnums->spec id))
        ((id argnums)
         (argnums->spec id (string-split argnums #\,)))))
    (match opts
      (() (begin
            ;; remove recognized --keyword command-line options:
            (set-program-arguments (cons (car (command-line))
                                         (reverse remaining-opts)))
            specs))
      ((current-opt . rest)
       (cond
        ((string=? "--" current-opt) specs)
        ((string-prefix? "--keyword=" current-opt)
         (let ((keyword (string-drop current-opt (string-length "--keyword="))))
           (loop rest remaining-opts (cons (string->spec keyword) specs))))
        ((or (string=? "--keyword" current-opt)
             (string=? "-k" current-opt))
         (let ((next-opt (car rest)))
           (loop (cdr rest)
                 remaining-opts
                 (cons (string->spec next-opt) specs))))
        (else (loop rest (cons current-opt remaining-opts) specs)))))))

;;; Other options are not repeated, so we can use getopt-long:

(define %options ;; Corresponds to what is documented at `info xgettext`.
  (let ((option-spec
         `((files (single-char #\f) (value #t))
           (directory (single-char #\D) (value #t))
           (default-domain (single-char #\d) (value #t))
           (output (single-char #\o) (value #t))
           (output-dir (single-char #\p) (value #t))
           (from-code (value #t))
           (join-existing (single-char #\j) (value #f))
           (exclude-file (single-char #\x) (value #t))
           (add-comments (single-char #\c) (value #t))

           ;; Because getopt-long does not support repeated options,
           ;; we took care of --keyword options further up.
           ;; (keyword (single-char #\k) (value #t))

           (flag (value #t))
           (force-po (value #f))
           (indent (single-char #\i) (value #f))
           (no-location (value #f))
           (add-location (single-char #\n) (value #t))
           (width (single-char #\w) (value #t))
           (no-wrap (value #f))
           (sort-output (single-char #\s) (value #f))
           (sort-by-file (single-char #\F) (value #f))
           (omit-header (value #f))
           (copyright-holder (value #t))
           (foreign-user (value #f))
           (package-name (value #t))
           (package-version (value #t))
           (msgid-bugs-address (value #t))
           (msgstr-prefix (single-char #\m) (value #t))
           (msgstr-suffix (single-char #\m) (value #t))
           (help (value #f))
           (pack (value #f)))))
    (getopt-long (command-line) option-spec)))


(define parse-scheme-file
  ;; This procedure parses FILE and returns a parse tree.
  (let ()
    ;;TODO: Optionally ignore case.
    (define-peg-pattern NL all "\n")
    (define-peg-pattern comment all (and ";"
                                         (* (and peg-any
                                                 (not-followed-by NL)))
                                         (and peg-any (followed-by NL))))
    (define-peg-pattern empty none (or " " "\t"))
    (define-peg-pattern whitespace body (or empty NL))
    (define-peg-pattern quotation body (or "'" "`" "," ",@"))
                                        ;TODO: Allow user to specify
                                        ;other quote reader macros to
                                        ;be ignored and also ignore
                                        ;quote spelled out without
                                        ;reader macro.
    (define-peg-pattern open body (and (? quotation)
                                       (or "(" "[" "{")))
    (define-peg-pattern close body (or ")" "]" "}"))
    (define-peg-pattern string body (and (followed-by "\"")
                                         (* (or "\\\""
                                                (and (or NL peg-any)
                                                     (not-followed-by "\""))))
                                         (and (or NL peg-any)
                                              (followed-by "\""))
                                         "\""))
    (define-peg-pattern token all (or string
                                      (and
                                       (not-followed-by open)
                                       (not-followed-by close)
                                       (not-followed-by comment)
                                       (* (and peg-any
                                               (not-followed-by open)
                                               (not-followed-by close)
                                               (not-followed-by comment)
                                               (not-followed-by string)
                                               (not-followed-by whitespace)))
                                       (or
                                        (and peg-any (followed-by open))
                                        (and peg-any (followed-by close))
                                        (and peg-any (followed-by comment))
                                        (and peg-any (followed-by string))
                                        (and peg-any (followed-by whitespace))
                                        (not-followed-by peg-any)))))
    (define-peg-pattern list all (or (and (? quotation) "(" program ")")
                                     (and (? quotation) "[" program "]")
                                     (and (? quotation) "{" program "}")))
    (define-peg-pattern t-or-s body (or token list))
    (define-peg-pattern program all (* (or whitespace
                                           comment
                                           t-or-s)))
    (lambda (file)
      (call-with-input-file file
        (lambda (port)
          ;; It would be nice to match port directly without
          ;; converting to a string first, but apparently guile cannot
          ;; do that yet.
          (let ((string (get-string-all port)))
            (peg:tree (match-pattern program string))))))))


(define-record-type <po-entry>
  (make-po-entry ecomments ref flags ctxt id idpl)
  po-entry?
;;; irrelevant: (tcomments po-entry-tcomments) ;translator-comments
  (ecomments po-entry-ecomments) ;extracted-comments
  (ref po-entry-ref) ;reference
  (flags po-entry-flags)
;;; irrelevant: (prevctxt po-entry-prevctxt) ;previous-ctxt
;;; irrelevant: (prev po-entry-prev) ;previous-translation
  (ctxt po-entry-ctxt) ;msgctxt
  (id po-entry-id) ;msgid
  (idpl po-entry-idpl) ;msgid-plural
;;; irrelevant: (str po-entry-str) ;msgstr string or association list
;;;                                ;integer to string
  )

(define (po-equal? po1 po2)
  "Returns whether PO1 and PO2 have equal ctxt, id and idpl."
  (and (equal? (po-entry-ctxt po1) (po-entry-ctxt po2))
       (equal? (po-entry-id po1) (po-entry-id po2))
       (equal? (po-entry-idpl po1) (po-entry-idpl po2))))

(define (combine-duplicate-po-entries list)
  "Returns LIST with duplicate po entries replaced by a single PO
entry with both refs."
  (let loop ((remaining list))
    (match remaining
      (() '())
      ((head . tail)
       (receive (before from)
           (break (cut po-equal? head <>) tail)
         (cond
          ((null? from) (cons head (loop tail)))
          (else
           (loop
            (cons
             (match head
               (($ <po-entry> ecomments1 ref1 flags ctxt id idpl)
                (match (car from)
                  (($ <po-entry> ecomments2 ref2 _ _ _ _)
                   (let ((ecomments (if (or ecomments1 ecomments2)
                                        (append (or ecomments1 '())
                                                (or ecomments2 '()))
                                        #f))
                         (ref (if (or ref1 ref2)
                                  (string-join
                                   (cons
                                    (or ref1 "")
                                    (cons
                                     (or ref2 "")
                                     '())))
                                  #f)))
                     (make-po-entry ecomments ref flags ctxt id idpl))))))
             (append before (cdr from)))))))))))

(define (write-po-entry po-entry)
  (define (prepare-text text)
    "If TEXT is false, returns #f.  Otherwise corrects the formatting
of TEXT by escaping backslashes and newlines and enclosing TEXT in
quotes. Note that Scheme’s write is insufficient because it would
escape far more.  TODO: Strings should be wrappable to a maximum line
width."
    (and text
         (string-append "\""
                        (with-output-to-string
                          (lambda ()
                            (call-with-input-string text
                              (lambda (port)
                                (let loop ((c (get-char port)))
                                  (unless (eof-object? c)
                                    (case c
                                      ((#\\) (display "\\"))
                                      ((#\newline) (display "\\n"))
                                      (else (write-char c)))
                                    (loop (get-char port))))))))
                        "\"")))
  (define (write-component c prefix)
    (when c
      (begin (display prefix)
             (display " ")
             (display c)
             (newline))))
  (match po-entry
    (($ <po-entry> ecomments ref flags ctxt id idpl)
     (let ((prepared-ctxt (prepare-text ctxt))
           (prepared-id (prepare-text id))
           (prepared-idpl (prepare-text idpl)))
       (when ecomments
         (for-each
          (lambda (line)
            (write-component line "#."))
          (reverse ecomments)))
       (write-component ref "#:")
       (write-component (and flags (string-join flags ", ")) "#,")
       (write-component prepared-ctxt "msgctxt")
       (write-component prepared-id "msgid")
       (write-component prepared-idpl "msgid_plural")
       (if idpl
           (begin
             (display "msgstr[0] \"\"")
             (newline)
             (display "msgstr[1] \"\""))
           (display "msgstr \"\""))
       (newline)))))

(define %comments-line
  (make-parameter #f))

(define %ecomments-string
  (make-parameter #f))

(define (update-ecomments-string! str)
  "Sets the value of the parameter object %ecomments-string if str is
an ecomments string.  An ecomments string is extracted from a comment
because it starts with TRANSLATORS or a key specified with
--add-comments." ;TODO: Support for other keys is missing.
  (cond
   ((not str) (%ecomments-string #f))
   ((= (1+ (or (%comments-line) -42)) (or (%line-number) 0))
    (let ((m (string-match ";+[ \t]*(.*)" str)))
      (when m
        (%comments-line (%line-number))
        (%ecomments-string
         (if (%ecomments-string)
             (cons (match:substring m 1) (%ecomments-string))
             (list (match:substring m 1)))))))
   (else
    (let ((m (string-match ";+[ \t]*(TRANSLATORS:.*)" str)))
      (if m
          (begin
            (%comments-line (%line-number))
            (%ecomments-string
             (if (%ecomments-string)
                 (cons (match:substring m 1) (%ecomments-string))
                 (list (match:substring m 1)))))
          (%ecomments-string '#f))))))

(define %file-name
  (make-parameter #f))

(define (update-file-name! name)
  "Sets the value of the parameter object %file-name to NAME."
  (%file-name name))

(define %old-line-number
  (make-parameter #f))

(define (update-old-line-number! number)
  "Sets the value of the parameter object %old-line-number to NUMBER."
  (%old-line-number number))

(define %line-number
  (make-parameter #f))

(define (update-line-number! number)
  "Sets the value of the parameter object %line-number to NUMBER."
  (%line-number number))

(define (incr-line-number!)
  "Increments the value of the parameter object %line-number by 1."
  (%line-number (1+ (%line-number))))

(define (incr-line-number-for-each-nl! list)
  "Increments %line-number once for each NL recursively in LIST.  Does
nothing if LIST is no list but e.g. an empty 'program."
  (when (list? list)
    (for-each
     (lambda (part)
       (match part
         ('NL (incr-line-number!))
         ((? list?) (incr-line-number-for-each-nl! part))
         (else #f)))
     list)))

(define (current-ref)
  "Returns the location field for a PO entry."
  (let ((add (option-ref %options 'add-location 'full)))
    (cond
     ((option-ref %options 'no-location #f) #f)
     ((eq? add 'full)
      (string-append (%file-name) ":" (number->string (%line-number))))
     ((eq? add 'file)
      (%file-name))
     ((eq? add 'never)
      #f))))

(define (make-simple-po-entry msgid)
  (let ((po (make-po-entry
             (%ecomments-string)
             (current-ref)
             #f ;TODO: Use scheme-format for format strings?
             #f ;no ctxt
             msgid
             #f)))
    (update-ecomments-string! #f)
    po))


(define (matching-keyword id)
  "Returns the keyword-spec whose identifier is the same as ID, or #f
if ID is no string or no such keyword-spec exists."
  (and (symbol? id)
       (let ((found (member (symbol->string id)
                            %keyword-specs
                            (lambda (id spec)
                              (string=? id (keyword-spec-id spec))))))
         (and found (car found)))))

(define (nth-exp program n)
  "Returns the Nth 'token or 'list inside the PROGRAM parse tree or #f
if no tokens or lists exist."
  (let loop ((i 0)
             (rest program))
    (define (on-hit exp)
      (if (= i n) exp
          ;; else:
          (loop (1+ i) (cdr rest))))
    (match rest
      (() #f)
      ((('token . _) . _) (on-hit (car rest)))
      ((('list open-paren exp close-paren) . _) (on-hit (car rest)))
      ((_ . _) (loop i (cdr rest)))
      (else #f))))

(define (more-than-one-exp? program)
  "Returns true if PROGRAM consiste of more than one expression."
  (if (matching-keyword (token->string-symbol-or-keyw (nth-exp program 0)))
      (nth-exp program 2) ;if there is third element, keyword does not count
      (nth-exp program 1)))

(define (token->string-symbol-or-keyw tok)
  "For a parse tree TOK, if it is a 'token parse tree, returns its
value as a string, symbol or #:-keyword, otherwise returns #f."
  (match tok
    (('token (parts ...) . remaining)
     ;; This is a string with line breaks in it.
     (with-input-from-string
         (string-append
          (apply string-append
                 (map-in-order
                  (lambda (part)
                    (match part
                      (('NL _)
                       (begin (incr-line-number!)
                              "\n"))
                      (else part)))
                  parts))
          (car remaining))
       (lambda ()
         (read))))
    (('token exp)
     (with-input-from-string exp
       (lambda ()
         (read))))
    (else #f)))

(define (complex-marked-list->po-entries parse-tree)
  "Checks if PARSE-TREE is marked by a keyword.  If yes, for a complex
keyword spec, returns a list of po-entries for it.  For a simple
keyword spec, returns the argument number of its singular form.
Otherwise returns #f."
  (let* ((first (nth-exp parse-tree 0))
         (spec (matching-keyword (token->string-symbol-or-keyw first))))
    (if spec
        (if ;if the identifier of a complex keyword occurs first
         (complex-keyword-spec? spec)
         ;; then make po entries for it
         (match spec
           (($ <keyword-spec> id sg pl c total xcomment)
            (if (eq? c 'mixed) ; if msgctxt and singular msgid are in one string
                (let* ((exp (nth-exp parse-tree sg))
                       (val (token->string-symbol-or-keyw exp))
                       (idx (if (string? val) (string-rindex val #\|))))
                  (list
                   (let ((po (make-po-entry
                              (%ecomments-string)
                              (current-ref)
                              #f ;TODO: Use scheme-format for format strings?
                              (string-take val idx)
                              (string-drop val (1+ idx))
                              #f))) ;plural forms are unsupported here
                     (update-ecomments-string! #f)
                     po)))
                ;; else construct msgids
                (receive (pl-id pl-entries)
                    (match pl
                      (#f (values #f '()))
                      (else (construct-msgid-and-po-entries
                             (nth-exp parse-tree pl))))
                  (receive (sg-id sg-entries)
                      (construct-msgid-and-po-entries
                       (nth-exp parse-tree sg))
                    (cons
                     (let ((po (make-po-entry
                                (%ecomments-string)
                                (current-ref)
                                #f ;TODO: Use scheme-format for format strings?
                                (and c (token->string-symbol-or-keyw
                                        (nth-exp parse-tree c)))
                                sg-id
                                pl-id)))
                       (update-ecomments-string! #f)
                       po)
                     (append sg-entries pl-entries)))))))
         ;; else if it is a simple keyword, return the argnum:
         (keyword-spec-sg spec))
        ;; if no keyword occurs, then false
        #f)))

(define (construct-po-entries parse-tree)
  "Converts a PARSE-TREE resulting from a call to parse-scheme-file to
a list of po-entry records.  Unlike construct-msgid-and-po-entries,
strings are not collected to a msgid.  The list of po-entry records is
the return value."
  (let ((entries (complex-marked-list->po-entries parse-tree)))
    (cond
     ((list? entries) entries)
     ((number? entries) ;parse-tree yields a single, simple po entry
      (update-old-line-number! (%line-number))
      (receive (id entries)
          (construct-msgid-and-po-entries
           (nth-exp parse-tree entries))
        (update-line-number! (%old-line-number))
        (let ((po (make-simple-po-entry id)))
          (incr-line-number-for-each-nl! parse-tree)
          (cons po entries))))
     (else ;search for marked translations in parse-tree
      (match parse-tree
        (() '())
        (('comment str) (begin
                          (update-ecomments-string! str)
                          '()))
        (('NL _) (begin (incr-line-number!) '()))
        (('token . _) (begin (incr-line-number-for-each-nl! parse-tree) '()))
        (('list open-paren program close-paren)
         (construct-po-entries program))
        (('program . components)
         (append-map construct-po-entries components))
        ;; Note: PEG compresses empty programs to non-lists:
        ('program
         '()))))))

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

(define-record-type <construct-fold-state>
  (make-construct-fold-state msgid-string maybe-part counter po-entries)
  construct-fold-state?
  ;; msgid constructed so far; #f if none, "" if only empty string:
  (msgid-string construct-fold-state-msgid-string)
  ;; only append this if string follows:
  (maybe-part construct-fold-state-maybe-part)
  ;; counter for next tag:
  (counter construct-fold-state-counter)
  ;; complete po entries from marked sub-expressions:
  (po-entries construct-fold-state-po-entries))

(define* (construct-msgid-and-po-entries parse-tree
                                         #:optional
                                         (prefix ""))
  "Like construct-po-entries, but with two return values.  The first
is an accumulated msgid constructed from all components in PARSE-TREE
for use in make-po-entry.  Non-strings are replaced by tags containing
PREFIX.  The second return value is a list of po entries for
sub-expressions marked with a complex keyword spec."
  (match parse-tree
    (() (values "" '()))
    ;; Note: PEG compresses empty programs to non-lists:
    ('program (values "" '()))
    (('comment str) (begin
                      (update-ecomments-string! str)
                      (values "" '())))
    (('NL _) (begin (incr-line-number!)
                    (error "Program consists only of line break."
                           `(,(%file-name) ,(%line-number)))))
    (('token . _)
     (let ((maybe-string (token->string-symbol-or-keyw parse-tree)))
       (if (string? maybe-string)
           (values maybe-string '())
           (error "Single symbol marked for translation."
                  `(,maybe-string ,(%file-name) ,(%line-number))))))
    (('list open-paren program close-paren)
     ;; parse program instead
     (construct-msgid-and-po-entries program prefix))
    (('program (? matching-keyword))
     (error "Double-marked for translation."
            `(,parse-tree ,(%file-name) ,(%line-number))))
    (('program . components)
     ;; Concatenate strings in parse-tree to a new msgid and add an
     ;; <x> tag for each list in between.
     (match
         (fold
          (lambda (component prev-state)
            (match prev-state
              (($ <construct-fold-state> msgid-string maybe-part
                  counter po-entries)
               (match component
                 (('comment str) (begin (update-ecomments-string! str)
                                        prev-state))
                 (('NL _) (begin (incr-line-number!)
                                 prev-state))
                 (('token . _)
                  (let ((maybe-string (token->string-symbol-or-keyw component)))
                    (cond
                     ((string? maybe-string)
                      ;; if string, append maybe-string to previous msgid
                      (make-construct-fold-state
                       (string-append (or msgid-string "")
                                      maybe-part maybe-string)
                       ""
                       counter
                       po-entries))
                     ((and (more-than-one-exp? components) ;not the only symbol
                           (or (not msgid-string) ;no string so far
                               (string-suffix? ">" msgid-string))) ;tag before
                      prev-state) ;then ignore
                     (else ;append tag representing the token
                      (make-construct-fold-state
                       msgid-string
                       (string-append
                        maybe-part
                        (tag counter prefix #:flavor 'empty))
                       (1+ counter)
                       po-entries)))))
                 (('list open-paren program close-paren)
                  (let ((first (nth-exp program 0)))
                    (incr-line-number-for-each-nl! list)
                    (match (complex-marked-list->po-entries program)
                      ((? list? result)
                       (make-construct-fold-state
                        msgid-string
                        (string-append
                         maybe-part
                         (tag counter prefix #:flavor 'empty))
                        (1+ counter)
                        (append result po-entries)))
                      (result
                       (cond
                        ((number? result)
                         (receive (id entries)
                             (construct-msgid-and-po-entries
                              (nth-exp program result)
                              (string-append prefix
                                             (number->string counter)
                                             "."))
                           (make-construct-fold-state
                            (string-append (or msgid-string "")
                                           maybe-part
                                           (tag counter prefix
                                                #:flavor 'start)
                                           id
                                           (tag counter prefix
                                                #:flavor 'end))
                            ""
                            (1+ counter)
                            (append entries po-entries))))
                        ((not (more-than-one-exp? components))
                         ;; Singletons do not need to be marked.
                         (receive (id entries)
                             (construct-msgid-and-po-entries
                              program
                              prefix)
                           (make-construct-fold-state
                            id
                            ""
                            counter
                            (append entries po-entries))))
                        (else ;unmarked list
                         (if (not msgid-string)
                             ;; then ignore
                             prev-state
                             ;; else:
                             (make-construct-fold-state
                              msgid-string
                              (string-append
                               maybe-part
                               (tag counter prefix #:flavor 'empty))
                              (1+ counter)
                              po-entries))))))))))))
          (make-construct-fold-state #f "" 1 '())
          components)
       (($ <construct-fold-state> msgid-string maybe-part counter po-entries)
        (values (or msgid-string
                    (error "Marking for translation yields empty msgid."
                           %file-name %line-number))
                po-entries))))))

(define scheme-file->po-entries
  (compose construct-po-entries
           parse-scheme-file))

(define %files-from-port
  (let ((files-from (option-ref %options 'files #f)))
    (if files-from
        (open-input-file files-from)
        (current-input-port))))

(define %source-files
  (let loop ((line (get-line %files-from-port))
             (source-files '()))
    (if (eof-object? line)
        (begin
          (close-port %files-from-port)
          source-files)
        ;; else read file names before comment
        (let ((before-comment (car (string-split line #\#))))
          (loop (get-line %files-from-port)
                (append
                 (map match:substring (list-matches "[^ \t]+" before-comment))
                 source-files))))))

(define %output-po-entries
  (fold (lambda (scheme-file po-entries)
          (begin
            (update-file-name! scheme-file)
            (update-line-number! 1)
            (update-old-line-number! #f)
            (%comments-line #f)
            (append (scheme-file->po-entries scheme-file)
                    po-entries)))
        '()
        %source-files))

(define %output-port
  (let ((output (option-ref %options 'output #f))
        (domain (option-ref %options 'default-domain #f)))
    (cond
     (output (open-output-file output))
     (domain (open-output-file (string-append domain ".po")))
     (else (open-output-file "messages.po")))))

(with-output-to-port %output-port
  (lambda ()
    (let ((copyright (option-ref %options 'copyright-holder
                                 "THE PACKAGE'S COPYRIGHT HOLDER"))
          (package (option-ref %options 'package-name "PACKAGE"))
          (version (option-ref %options 'package-version #f))
          (bugs-email (option-ref %options 'msgid-bugs-address "")))
      (display "# SOME DESCRIPTIVE TITLE.\n")
      (display (string-append "# Copyright (C) YEAR " copyright "\n"))
      (display (string-append "# This file is distributed under the same \
license as the " package " package.\n"))
      (display "# FIRST AUTHOR <EMAIL@ADDRESS>, YEAR.\n")
      (display "#\n")
      (write-po-entry (make-po-entry #f #f '("fuzzy") #f "" #f))
      (display (string-append "\"Project-Id-Version: "
                              package
                              (if version
                                  (string-append " " version)
                                  "")
                              "\\n\"\n"))
      (display (string-append "\"Report-Msgid-Bugs-To: "
                              bugs-email
                              "\\n\"\n"))
      (display (string-append "\"POT-Creation-Date: "
                              (date->string (current-date) "~1 ~H:~M~z")
                              "\\n\"\n"))
      (display "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"\n")
      (display "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"\n")
      (display "\"Language-Team: LANGUAGE <LL@li.org>\\n\"\n")
      (display "\"Language: \\n\"\n")
      (display "\"MIME-Version: 1.0\\n\"\n")
      (display "\"Content-Type: text/plain; charset=UTF-8\\n\"\n")
      (display "\"Content-Transfer-Encoding: 8bit\\n\"\n")
      (for-each (lambda (po-entry)
                  (begin
                    (newline)
                    (write-po-entry po-entry)))
                (combine-duplicate-po-entries %output-po-entries)))))
