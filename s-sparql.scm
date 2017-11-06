(module s-sparql *
(import chicken scheme extras data-structures srfi-1) 

(use sparql-query
     srfi-13 srfi-69 http-client intarweb uri-common medea cjson matchable irregex)


(require-extension typeclass input-classes abnf abnf-charlist abnf-consumers
                   lexgen)

(reexport sparql-query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(define (car-when p)
  (and (pair? p) (car p)))

(define (car-or p default)
  (if (pair? p) (car p) default))

(define (cdr-when p)
  (and (pair? p) (cdr p)))

(define (cdr-or p default)
  (if (pair? p) (cdr p) default))

(define (cons-when exp p)
  (if exp (cons exp p) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend definitions from module sparql-query
(*query-unpacker* typed-sparql-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatypes
(define (new-sparql-variable #!optional (prefix "v"))
  (string->symbol (conc "?" (->string (gensym prefix)))))

(define (new-blank-node #!optional (prefix "b"))
  (string->symbol (conc "_:" (->string (gensym prefix)))))

(define (sparql-variable str)
  (string->symbol (conc "?" (->string str))))

(define (sparql-variable? obj)
  (and (symbol? obj)
       (or (equal? "?" (substring (->string obj) 0 1))
           (equal? "$" (substring (->string obj) 0 1)))))

(define (triple? expr)
  (and (pair? expr)
       (<= (length expr) 3)
       (or (iri? (car expr))
	   (sparql-variable? (car expr))
	   (blank-node? (car expr)))))

(define (select? expr)
  (and (pair? expr)
       (member (car expr)
	       `(SELECT |SELECT DISTINCT| |SELECT REDUCED|))))

(define (subselect? expr)
  (and (pair? expr)
       (select? (car expr))))

(define (where-subselect? block)
  (and (equal? (car block) 'WHERE)
       (subselect? (cdr block))))

(define (quads-block? expr)
  (member (car expr)
	  '(WHERE
	    DELETE |DELETE WHERE| |DELETE DATA|
	    INSERT |INSERT WHERE| |INSERT DATA|
	    MINUS OPTIONAL UNION GRAPH)))

(define (blank-node? obj)
  (or (and (pair? obj)
           (symbol? (car obj))
           (equal? (car obj) '@Blank))
      (and (symbol? obj)
	   (not (sparql-variable? obj))
	   (let ((s (symbol->string obj)))
	     (and (> (string-length s) 2)
		  (equal? "_:" (substring s 0 2)))))))

(define (blank-node-path? obj)
  (and (list? obj)
       (blank-node? (car obj))))

(define (inverse-path? obj)
  (and (pair? obj)
       (equal? (car obj) '^)))

(define (sequence-path? obj)
  (and (pair? obj)
       (equal? (car obj) '/)))

(define (alternative-path? obj)
  (and (pair? obj)
       (equal? (car obj) '|\||)))

(define (zero-or-more-path? obj)
  (and (pair? obj)
       (equal? (car obj) '*)))

(define (one-or-more-path? obj)
  (and (pair? obj)
       (equal? (car obj) '+)))

(define (zero-or-one-path? obj)
  (and (pair? obj)
       (equal? (car obj) '?)))

(define (modified-path? obj)
  (and (pair? obj)
       (member (car obj) '(? * +))))

(define (negated-set? obj)
  (and (pair? obj)
       (equal? (car obj) '!)))

(define (group-path? obj)
  (and (pair? obj)
       (equal? (car obj) '|@()|)))

(define (property-path? obj)
  (or (inverse-path? obj)
      (sequence-path? obj)
      (alternative-path? obj)
      (zero-or-more-path? obj)
      (one-or-more-path? obj)
      (zero-or-one-path? obj)
      (negated-set? obj)
      (group-path? obj)))

(define (iri? obj)
  (and (symbol? obj)
       (let ((s (symbol->string obj)))
         (or (and (string-prefix? "<" s)
                  (string-suffix? ">" s))
             (= (length (string-split s ":")) 2)))))

(define (a? obj)
  (equal? 'a obj))

(define (langtag? exp)
  (and (symbol? exp)
       (equal? (substring (symbol->string exp) 0 1) "@")))

(define (typed-or-langtag-literal? exp)
  (and (pair? exp) (not (list? exp))
       (string? (car exp)) (symbol? (cdr exp))))

(define (inverse-element? elt)
  (and (pair? elt)
       (equal? (car elt) '^)))

(define (element-path? elt)
  (and (pair? elt)
       (equal? (car elt) '/)))

(define (un-sparql-variable var)
  (string->symbol (substring (symbol->string var) 1)))

(define (sparql-variable->string var)
  (substring (symbol->string var) 1))

(define (s-iri? elt)
  (let ((s (->string elt)))
    (and (string-prefix? "<" s)
         (string-suffix? ">" s))))

(include "s-sparql-transform.scm")
(include "s-sparql-writer.scm")
(include "s-sparql-parser.scm")

)
