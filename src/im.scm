;;; im.scm -- indentation matters python like parsing

#;(define (error . msg)
  (for-each display msg)
  (newline))

(define *indent* '())

(define (current-column) (car *indent*))
(define (push-indent col) (set! *indent* (cons col *indent*)))
(define (pop-indents-to! col)
  (cond ((> (current-column) col)
         (let ((c (current-column)))
           (set! *indent* (cdr *indent*))
           (cons c (pop-indents-to! col))))
        ((= (current-column) col)
         '())
        (else (error "bad indentation style"))))
     
(define (lex-from-file filename)
  (call-with-input-file filename
    (lambda (p)
      (start-of-file p (lambda () (lex p))))))

(define *ops* '(#\+ #\- #\* #\/ #\= #\< #\> #\:))

(define (lex port)
  (skip-blanks port
               (lambda ()
                 (let ((ch (peek-char port)))
		   ;(display ";; lex ") (write ch) (newline)
                   (cond ((eof-object? ch) (map (lambda (e) ':dedent) (pop-indents-to! 0)))
                         ((char=? ch #\") (read-string-literal port)) ; " makes a2ps happy
                         ((char-numeric? ch) (read-number-literal port))
                         ((or (char-alphabetic? ch)) (read-identifier port))
                         ((char=? ch #\newline) (read-char port) (cons ':nl (lex port)))
                         ((char=? ch #\() (read-char port) (cons ':open-paren (lex port)))
                         ((char=? ch #\)) (read-char port) (cons ':close-paren (lex port)))
                         ((char=? ch #\[) (read-char port) (cons ':open-bra (lex port)))
                         ((char=? ch #\]) (read-char port) (cons ':close-bra (lex port)))
                         ((char=? ch #\{) (read-char port) (cons ':open-curly (lex port)))
                         ((char=? ch #\}) (read-char port) (cons ':close-curly (lex port)))
                         ((char=? ch #\:) (read-char port) (cons ':colon (lex port)))
                         ((char=? ch #\;) (read-char port) (cons ':semicolon (lex port)))
                         ((char=? ch #\.) (read-char port) (cons ':dot (lex port)))
                         ((char=? ch #\,) (read-char port) (cons ':comma (lex port)))
			 ((char=? ch #\') (read-character-literal port))
                         ((member ch *ops*) (read-operator port))
                         (else (error "unknown lexical token starting with ~a" ch)))))))

(define (skip-blanks port k)
  (let ((ch (peek-char port)))
    ;(display ";; sb ") (write ch) (newline)
    (cond ((eof-object? ch) (k))
          ((char=? ch #\space) (read-char port) (skip-blanks port k))
          ((char=? ch #\newline) (start-of-line port k))
	  ((char=? ch #\#) (skip-comment port k))
          (else (k)))))

(define (skip-comment port k)
  (let ((ch (peek-char port)))
    ;(display ";; co ") (write ch) (newline)
    (cond ((eof-object? ch) (skip-blanks port k))
	  ((char=? ch #\newline) (start-of-line port k))
	  (else (read-char port) (skip-comment port k)))))
               
(define (start-of-line port k)
  (read-char port)
  (let lp ((ch (peek-char port))
           (col 0))
    ;(display ";; sol ") (write col) (display ":") (write ch) (newline)
    (cond ((eof-object? ch)
           (let ((indents (pop-indents-to! 0)))
             (cons':nl (append (mappend (lambda (e) (list ':dedent ':nl)) indents)
                               (k)))))
          ((char=? ch #\space) (read-char port) (lp (peek-char port) (+ col 1)))
          ((char=? ch #\newline) (read-char port) (lp (peek-char port) 0))
	  ((char=? ch #\#) (skip-comment port k))
          (else 
           (cond ((> col (current-column))
                  (set! *indent* (cons col *indent*))
                  (cons ':indent (k)))
                 ((= col (current-column))
                  (cons ':nl (k)))
                 (else 
                  (let ((indents (pop-indents-to! col)))
                    (cons ':nl (append (mappend (lambda (e) (list ':dedent ':nl)) indents)
                                       (k))))))))))

(define (mappend proc lst) (apply append (map proc lst)))

(define (start-of-file port k)
  (let lp ((ch (peek-char port))
           (col 0))
    (cond ((eof-object? ch) (k))
          ((char=? ch #\space) (read-char port) (lp (peek-char port) (+ col 1)))
          (else
           (set! *indent* (list col))
           (k)))))

(define (read-string-literal port)
  (read-char port)
  (let lp ((ch (read-char port))
           (contents '()))
    (cond ((eof-object? ch) (error "end of file inside string literal"))
          ((char=? ch #\") ; " makes a2ps happy
           (cons (list->string (reverse contents)) (lex port)))
	  ((char=? ch #\\)
	   (let ((next (read-char port)))
	     (cond ((eof-object? next) (error "end of file in character escape"))
		   ((char=? next #\n) (lp (read-char port) (cons #\newline contents)))
		   ((char=? next #\\) (lp (read-char port) (cons #\\ contents)))
		   (else (error "unknown escape sequence char ~a in string" next)))))
          (else (lp (read-char port) (cons ch contents))))))

(define (read-number-literal port)
  (let lp ((ch (peek-char port))
           (chars '()))
    (cond ((eof-object? ch) (cons (string->number (list->string (reverse chars))) (lex port)))
          ((char-numeric? ch) (read-char port) (lp (peek-char port) (cons ch chars)))
          (else
           (cons (string->number (list->string (reverse chars))) (lex port))))))

(define (read-identifier port)
  (let lp ((ch (peek-char port))
           (chars '()))
    (cond ((eof-object? ch) (cons (string->symbol (list->string (reverse chars))) (lex port)))
          ((or (char-numeric? ch) (char-alphabetic? ch) (member ch '(#\_ #\? #\! #\=)))
           (read-char port) (lp (peek-char port) (cons ch chars)))
          (else
           (cons (string->symbol (list->string (reverse chars))) (lex port))))))

(define (read-operator port)
  (let lp ((ch (peek-char port))
           (chars '()))
    (cond ((eof-object? ch) (cons (string->symbol (list->string (reverse chars))) (lex port)))
          ((member ch *ops*) (read-char port) (lp (peek-char port) (cons ch chars)))
          (else (cons (string->symbol (list->string (reverse chars))) (lex port))))))

(define (read-character-literal port)
  (read-char port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch)
	   (error "end of file inside character literal"))
	  ((char=? ch #\\)
	   (read-char port)
	   (let ((ch2 (peek-char port)))
	     (if (eof-object? ch2)
		 (error "end of file inside character literal escape")
		 (begin (read-char port)
			(let ((closing (read-char port)))
			  (if (eof-object? closing)
			      (error "end of file encountered, exoected closing character literal quote")
			      (if (char=? closing #\')
				  (cons ch2 (lex port))
				  (error "expected closing character literal quote"))))))))
	  (else (read-char port)
		(let ((closing (read-char port)))
		  (if (eof-object? closing)
		      (error "end of file encountered, exoected closing character literal quote")
		      (if (char=? closing #\')
			  (cons ch (lex port))
			  (error "expected closing character literal quote"))))))))

;;; parsing

(define (parse rule tokens)
  (rule tokens list))

(define (parse-file file-name)
  (let ((tokens (lex-from-file file-name)))
    ((top) tokens list)))
     
(define (seq . ps)
  (lambda (tokens k)
    (let lp ((ps ps)
             (acc '())
             (tokens tokens))
      (if (null? ps)
          (k (reverse acc) tokens)
          ((car ps) tokens (lambda (r rest)
                             (lp (cdr ps) (cons r acc) rest)))))))

(define (alt . ps)
  (lambda (tokens k)
    (let lp ((ps ps))
      (if (null? ps)
          #f
          (or ((car ps) tokens k)
              (lp (cdr ps)))))))

(define (opt p)
  (lambda (tokens k)
    (or (p tokens k)
        (k #f tokens))))

(define (rep p)
  (lambda (tokens k)
    (or (seq p (rep p))
	(k '() tokens))))

(define-syntax =>
  (syntax-rules ()
    ((=> p exp)
     (lambda (tokens k)
       (p tokens (lambda ($$ t) (k (apply exp $$) t)))))))

(define (:=> p e)
  (lambda (tokens k)
    (p tokens (lambda ($$ t) (k (apply e $$) t)))))

(define-syntax defrule
  (syntax-rules ()
    ((defrule ?name ?exp)
     (define (?name)
       (lambda (tokens k)
         (?exp tokens k))))))

(define (name? name)
  (and (symbol? name)
       (let ((str (symbol->string name)))
         (not (char=? #\: (string-ref str 0))))))

(define (:id)
  (lambda (tokens k)
    (if (and (pair? tokens) (name? (car tokens)))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:num)
  (lambda (tokens k)
    (if (and (pair? tokens) (number? (car tokens)))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:str)
  (lambda (tokens k)
    (if (and (pair? tokens) (string? (car tokens)))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:char)
  (lambda (tokens k)
    (if (and (pair? tokens) (char? (car tokens)))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:colon)
  (lambda (tokens k)
    (if (and (pair? tokens) (eq? (car tokens) ':colon))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:kw kw)
  (lambda (tokens k)
;    (display (list ':kw tokens)) (newline)
    (if (and (pair? tokens) (eq? (car tokens) kw))
        (k (car tokens) (cdr tokens))
        #f)))

(define (:term)
  (lambda (tokens k)
    (cond ((null? tokens) (k tokens tokens))
          ((and (pair? tokens) (eq? (car tokens) ':nl))
           (k (car tokens) (cdr tokens)))
          (else #f))))

(define (macro)
  (lambda (tokens k)
    (cond ((null? tokens) #f)
          ((and (pair? tokens) (macro? (car tokens)))
           ((macro-parser (car tokens)) (cdr tokens) k))
          (else #f))))

(define (trace str)
  (lambda (tokens k)
;    (display (list str tokens)) (newline)
    (k 'undef tokens)))

(defrule top
  (alt (=> (seq (:term)) (lambda (term) #f))
       (=> (seq (simple-line) (:term))
	   (lambda (line :term) (cons 'begin line)))))

(defrule block
  (alt (=> (seq (simple-line) (:term) (block)) (lambda (line :t block) (append line block)))
       (=> (seq (simple-line) (:term)) (lambda (a b) a))))

(defrule simple-line
  (alt (=> (seq (line) (:kw ':semicolon) (simple-line)) (lambda (line :semi lines) (cons line lines)))
       (=> (seq (line)) list)))

(defrule line (alt (stmt) (decl) (expression)))

(defrule expression (rel-exp))

(defrule rel-exp
  (=> (seq (term) (opt (seq (rel-op) (term))))
      (lambda (left right)
	(if right
	    `(,(car right) ,left ,(cadr right))
	    left))))

(defrule rel-op
  (alt (:kw '<) (:kw '<=) (:kw '=) (:kw '>) (:kw '>=)))

(defrule term
  (=> (seq (factor) (opt (seq (plus/minus) (term))))
      (lambda (left right)
	(if right
	    `(,(car right) ,left ,(cadr right))
	    left))))

(defrule plus/minus
  (alt (:kw '+) (:kw '-)))

(defrule factor
  (=> (seq (basic) (opt (seq (mul/div) (factor))))
      (lambda (left right)
	;(display (list 'factor left right)) (newline)
      	(if right
      	    `(,(car right) ,left ,(cadr right))
      	    left))))

(defrule mul/div
  (alt (:kw '/) (:kw '*) (:kw '%)))

(defrule basic
  (alt (=> (seq (leaf) (:kw ':open-paren) (args) (:kw ':close-paren)) (lambda (l :o args :c) (cons l args)))
       (=> (seq (leaf) (:kw ':open-paren) (:kw ':close-paren)) (lambda (l :o :c) (list l)))
       (=> (seq (leaf) (:kw ':open-bra) (args) (:kw ':close-bra)) (lambda (l :o args :c) (cons 'elt (cons l args))))
       (=> (seq (leaf) (:kw ':dot) (:id)) (lambda (leaf :dot id) (list id leaf)))
       (leaf)))

(defrule args
  (alt (=> (seq (expression) (:kw ':comma) (args)) (lambda (e _ es) (cons e es)))
       (=> (seq (expression)) (lambda (e) (list e)))))

(defrule leaf
  (alt (:str)
       (:id)
       (:num)
       (:char)
       (=> (:kw 'true) (lambda (kw) #t))
       (=> (:kw 'false) (lambda (kw) #f))
       (macro)
       (=> (seq (:kw 'fn) (:kw ':open-paren) (formals) (:kw ':close-paren) (body))
	   (lambda (:fn :open args :close body) (list 'fn args (cons 'begin body))))
       (=> (seq (:kw ':open-paren) (expression) (:kw ':close-paren)) (lambda (o e c) e))))

(defrule decl
  (alt (=> (seq (:kw 'def) (:id) (:kw ':open-paren) (formals) (:kw ':close-paren) (body))
           (lambda (:def id :op formals :cp body) `(def ,id (fn ,formals (begin ,@body)))))
       (=> (seq (:kw 'def) (:id) (:kw ':open-paren) (:kw ':close-paren) (body))
           (lambda (def id :op :cl body) `(def ,id (fn () (begin ,@body)))))))

(defrule formals
  (alt (=> (seq (:id) (:kw ':comma) (formals))
           (lambda (id :comma formals) (cons id formals)))
       (=> (seq (:id)) list)
       (=> (seq (:kw '*) (:id)) (lambda (:* id) id))))

(defrule stmt
  (alt (=> (seq (:kw 'if) (:kw ':open-paren) (expression) (:kw ':close-paren) (body) (opt-nl) (:kw 'else) (body))
           (lambda (:if :open exp :close then :nl :else else) `(if ,exp (begin ,@then) (begin ,@else))))
       (=> (seq (:kw 'if) (:kw ':open-paren) (expression) (:kw ':close-paren) (body))
           (lambda (:if :open exp :close body) `(if ,exp (begin ,@body))))
       (=> (seq (:kw 'let) (:id) (:kw '=) (expression) (body))
           (lambda (:let id :eq exp body) `(let ((,id ,exp)) (begin ,@body))))
       (=> (seq (:id) (:kw '=) (expression))
           (lambda (id :eq exp) `(set! id ,exp)))))

(defrule body
  (alt (=> (seq (:kw ':indent) (block) (:kw ':dedent))
           (lambda (:i e :d) e))
       (simple-line)))

(defrule opt-nl
  (opt (:kw ':nl)))

;;; Evaluation

(define *macros* '())
(define (set-macro! name parser)
  (display (list 'set-macro name parser)) (newline)
  (let ((entry (assq name *macros*)))
    (if entry
        (set-cdr! entry parser)
        (set! *macros* (cons (cons name parser) *macros*)))))

(define (macro? name) (assq name *macros*))
(define (macro-parser name)
  (let ((parser (assq name *macros*)))
    (if parser
        (cdr parser)
        (error "id ~a is not a macro definition"))))

(define *global* '())

(define (lookup-var id env)
  (let ((entry (assq id env)))
    (if entry
        (cdr entry)
        (let ((global-entry (assq id *global*)))
          (if global-entry
              (cdr global-entry)
              (error "Unbound variable ~a" id))))))

(define (bind-global! id value)
  (let ((entry (assq id *global*)))
    (if entry
        (set-cdr! entry value)
        (set! *global* (cons (cons id value) *global*)))))

(define (ev-toplevel e env)
  (cond ((or (string? e) (char? e) (number? e) (boolean? e)) (ev-literal e env))
        ((symbol? e) (ev-variable e env))
        ((and (pair? e) (eq? (car e) 'if)) (ev-if e env))
        ((and (pair? e) (eq? (car e) 'def)) (ev-def e env))
        ((and (pair? e) (eq? (car e) 'begin)) (ev-begin e env))
        ((and (pair? e) (eq? (car e) 'fn)) (ev-lambda e env))
        ((and (pair? e) (eq? (car e) 'let)) (ev-let e env))
        ((and (pair? e) (eq? (car e) 'set!)) (ev-set! e env))
        ((pair? e) (ev-application e env))
        (else (error "unknown expression ~a to evaluate" e))))

(define (ev-literal literal env) literal)
(define (ev-variable variable env) (lookup-var variable env))

(define (ev-set! e env)
  (let* ((id (cadr e))
         (value (ev-toplevel (caddr e) env)))
    (bind! id value env)
    #f))
         
(define (ev-let l env)
  (let* ((bindings (cadr l))
         (id (map car bindings))
         (inits (map (lambda (b) (ev-toplevel (cadr b) env)) bindings)))
    (ev-toplevel (caddr l) (bind id inits env))))

(define (ev-if test env)
  (if (ev-toplevel (cadr test) env)
      (ev-toplevel (caddr test) env)
      (ev-toplevel (cadddr test) env)))

(define (ev-def def env)
  (display ";; def") (display def) (newline)
  (bind-global! (cadr def) (ev-toplevel (caddr def) env))
  #f)

(define (bind ids vals env)
  (cond ((null? ids) env)
	((and (pair? ids) (pair? vals))
	 (cons (cons (car ids) (car vals))
	       (bind (cdr ids) (cdr vals) env)))
	((symbol? ids) (cons (cons ids vals) env))
	(else (error "bind error"))))

(define (bind! id value env)
  (let ((entry (assq id env)))
    (if entry
        (set-cdr! entry value)
        (set! *global* (cons (cons id value) *global*)))))

(define (ev-lambda e env) (lambda args (ev-toplevel (caddr e) (bind (cadr e) args env))))
(define (ev-application app env)
  (let* ((app* (map (lambda (e) (ev-toplevel e env)) app))
         (op (car app*)))
    (if (procedure? op)
        (apply op (cdr app*))
        (error "object is not a procedure in ~a" app*))))

(define (ev-begin es env)
  (let lp ((es (cdr es)))
    (cond ((null? es) #f)
          ((null? (cdr es)) (ev-toplevel (car es) env))
          (else (ev-toplevel (car es) env) (lp (cdr es))))))

(define (def id value) (bind-global! id value))

;; numerics
(def 'number? number?)
(def '+ +)
(def '- -)
(def '* *)
(def '< <)
(def '<= <)
(def '= equal?)
(def '> >)
(def '>= >=)
(def '% modulo)

;; booleans
(def 'not not)
(def 'bool? boolean?)

;; chars
(def 'char? char?)

;; pairs
(def 'pair? pair?)
(def 'pair cons)
(def 'head car)
(def 'tail cdr)
(def 'null? null?)

;; vectors
(def 'vector? vector?)

(def 'elt (lambda (c i)
	    (cond ((string? c) (string-ref c i))
		  ((pair? c) (list-ref c i))
		  ((vector? c) (vector-ref c i))
		  (else (error "bad type for REF" c)))))

;; i/o
(def 'print display)
(def 'newline newline)
(def 'open_input_file open-input-file)
(def 'open_output_file open-output-file)
(def 'read_char read-char)
(def 'peek_char peek-char)
(def 'close_input_port close-input-port)
(def 'close_output_port close-output-port)
(def 'current_input_port current-input-port)
(def 'current_output_port current-output-port)

;; macros
(def 'seq seq)
(def 'alt alt)
(def 'kw  :kw)
(def 'trans :=>)
(def 'expression expression)
(def 'body body)
(def 'colon :colon)

(def 'set_macro (lambda (name parser)
                   (if (string? name)
                       (set-macro! (string->symbol name) parser)
                       (set-macro! name parser))))

(def 'make_html (lambda (b) `(begin (print "<html>") ,@b (print "</html>") (newline))))
(def 'make_title (lambda (e) `(begin (print "<title>") (print ,e) (print "</title>"))))
(def 'rewrite_unless (lambda (test :colon body) #;(display (list 'unless test :colon body)) #;(newline) #;(display `(if ,test 0 ,body)) #;(newline) `(if ,test 0 (begin ,@body))))

(def 'use (lambda (id) (eval-file (if (symbol? id) (symbol->string id) id))))

;;; entry

(define (eval-file filename)
  (let lp ((tokens (lex-from-file filename)))
    ;(display ";; evaluating from ") (display tokens) (newline)
    (if (null? tokens)
        (display ";; evaluation done")
        (let ((parse (parse (top) tokens)))
          (if parse
              (begin
                ;(display ";; parsed as ") (display parse) (newline)
                (ev-toplevel (car parse) *global*)
                (lp (cadr parse)))
              (begin (display "Error evaluating file:") (display tokens) (newline)))))))
