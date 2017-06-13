(module s-sparql *

(import chicken scheme extras data-structures srfi-1) 

(use srfi-13 http-client intarweb uri-common medea matchable irregex)

(require-extension srfi-13)

(define (read-uri uri)
  (and (string? uri)
       (if (string-contains uri "://") ;; a bit hacky
           (string->symbol (conc "<" uri ">"))
           (string->symbol uri))))

(define *default-graph*
  (make-parameter
   '<http://mu.semte.ch/application>))

;; what about Docker?
(define *sparql-endpoint*
  (make-parameter
   "http://127.0.0.1:8890/sparql"))

(define *print-queries?* (make-parameter #t))

(define *namespaces* (make-parameter '()))

(define-syntax hit-property-cache
  (syntax-rules ()
    ((hit-property-cache sym prop body)
     (or (get sym prop)
         (put! sym prop body)))))

(define-syntax hit-hashed-cache
  (syntax-rules ()
    ((hit-hashed-cache cache key body)
     (or (hash-table-ref/default cache key #f)
         (begin
           (hash-table-set! cache key body)
           (hash-table-ref cache key))))))
    
;; to do : use this parameter in reify!
(define *expand-namespaces?* (make-parameter #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (sconc #!rest syms)
  (string->symbol 
   (apply conc (map ->string syms))))

(define (last-substr? str substr)
  (substring=? str substr
	       (- (string-length str)
		  (string-length substr))))

(define (conc-last str substr)
  (if (last-substr? str substr)
      str
      (conc str substr)))

(define (cdr-when p)
  (and (pair? p) (cdr p)))

(define (car-when p)
  (and (pair? p) (car p)))

(define (cons-when x p)
  (if x (cons x p) p))

(define (alist-ref-when x l)
  (or (alist-ref x l) '()))

(define (alist-merge-element x l)
    (let ((current (alist-ref-when (car x) l)))
      (if (null? current)
          (cons x l)
          (alist-update
           (car x)
           (cons (cdr x)
                 (if (pair? current)
                     current
                     (list current)))
           l))))

(define (fold-alist alst)
  (fold alist-merge-element '() alst))

(define-syntax if-pair?
  (syntax-rules ()
    ((if-pair? pair body)
     (if (null? pair)
         '()
         body))))

(define (str->num x)
  (and x (string->number x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data types

(define (new-sparql-variable)
  (string->symbol (conc "?" (->string (gensym)))))

(define (sparql-variable str)
  (string->symbol (conc "?" (->string str))))

(define (sparql-variable? obj)
  (and (symbol? obj)
       (or (equal? "?" (substring (->string obj) 0 1))
           (equal? "$" (substring (->string obj) 0 1)))))

(define (un-sparql-variable var)
  (string->symbol (substring (symbol->string var) 1)))

(define (sparql-variable->string var)
  (substring (symbol->string var) 1))

(define  (expand-uri x)
  (if (pair? x)
      (expand-namespace x)
      x))

(define (write-uri uri)
  (let ((str (symbol->string uri)))
    (substring str 1 (- (string-length str) 1))))

(define (rdf->json x)
  (cond ((symbol? x) (write-uri x))
	(else x)))

(define (register-namespace name namespace)
  (*namespaces* (cons (list name namespace) (*namespaces*))))

(define (lookup-namespace name #!optional (namespaces (*namespaces*)))
  (car (alist-ref name namespaces)))

(define (s-iri? elt)
  (let ((s (->string elt)))
    (and (string-prefix? "<" s)
         (string-suffix? ">" s))))

(define (expand-namespace* ns-pair namespaces)
  (let ((pair (string-split (->string ns-pair) ":")))
    (format #f "~A~A"
            (lookup-namespace (string->symbol (car pair)) namespaces)
            (cadr pair))))

(define (expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
  (if (or (sparql-variable? ns-pair)  (s-iri? ns-pair))
      ns-pair
      (string->symbol
       (format #f "<~A>" (expand-namespace* ns-pair namespaces)))))               

(define (write-expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
  (if (s-iri? ns-pair)
      (write-uri ns-pair)
      (expand-namespace* ns-pair namespaces)))

(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (begin
       (register-namespace (quote name) namespace) ;  (->string
       (define (name elt)
         (read-uri (conc namespace elt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDF

(define (range n)
  (list-tabulate n values))

(define (reify x)
  (cond ((string? x) (conc "\"" x "\""))
        ((keyword? x) (keyword->string x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((pair? x) (or (reify-special x)
                       (string-join (map reify x) " ")))))
                       ;; (if (pair? (car x))
                       ;;    (format #f "{ ~A }" (string-join (map reify x) " "))
                       ;;    (reify-triple x))))))

;; what about SELECT ?a ?b => commas or spaces?
(define (reify-special x)
  (case (car x)
    ((@TOP) (string-join (map reify (cdr x)) "\n"))
    ((@Prologue @Query) (conc (string-join (map reify (cdr x)) "\n") "\n"))
    ((@Dataset) (string-join (map reify (cdr x)) "\n"))
    ((|@()|) (format #f "( ~A )" (string-join (map reify-triple (cdr x)) " ")))
    ((|@[]|) (format #f "[ ~A ]" (string-join (map reify-triple (cdr x)) " ")))
    ((UNION) (string-join (map reify-triple (cdr x)) " UNION "))
    ((GRAPH) (format #f "GRAPH ~A ~A  "
                     (reify (cadr x)) (reify-triple (cddr x))))
    ((WHERE MINUS OPTIONAL) (format #f "~A ~A " (car x) (reify-triple (cdr x))))
    (else #f)))

;; add optional indent-level for readability
(define (reify-triple triple)
  (or (reify-special triple)
      (if (pair? (car triple)) ;; list of triples
          (format #f "{~%  ~A ~%}" (string-join (map reify-triple triple) " "))
          (conc
           (string-join
            (match triple
              ((subject properties) 
               (list (reify subject)
                     (reify-properties properties)))
              ((subject predicate . objects)
               (list (reify subject)
                     (reify-properties predicate)
                     (reify-objects objects)))))
           ". "))))

(define (reify-properties x)
  (if (pair? x)
      (string-join (map (lambda (property)
                          (format #f "~A ~A"
                                  (reify (car property))
                                  (reify-objects (cdr property))))
                        x)
                   ";  ")
      (reify x)))

(define (reify-objects x)
  (or (reify-special x)
      (string-join (map (lambda (y) (reify y)) x)
                   ", ")))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDF convenience functions

(define (s-triple triple)
  (if (string? triple)
      triple
      (reify-triple triple)))

(define (s-triples trips)
  (string-join (map s-triple trips) ".\n"))

(define (triple a b c)
  (s-triple (list a b c)))

(define (triples trips)
  (string-join trips "\n"))

(define (s-bracketed statement)
  (format #f "{ ~A }" statement))         

(define (s-graph graph statements)
  (if graph
      (format #f "GRAPH ~A { ~A } ."
              (reify graph)
              statements)
      statements))

(define (s-union statements)
  (string-join (map s-bracketed statements) " UNION "))

(define (s-optional statement)
  (format #f "~%OPTIONAL { ~A }" statement))

(define (s-filter statement filter)
  (format #f "~A FILTER (~A)" statement filter))

(define (s-bind as var)
  (format #f "BIND ~A as ~A" as var))

(define (s-insert triples #!key (with-graph (*default-graph*)))
  (conc (if with-graph (format #f "WITH ~A " with-graph) "")
        (format #f "~%INSERT {~%  ~A ~%}" triples)))

(define (sparql-varlist vars)
  (if (pair? vars)
      (string-join (map ->string vars) ", ")
      (->string vars)))

(define (s-select vars statements
                  #!key with-graph (from-graph (*default-graph*)) 
                  (from-named-graphs '()) order-by)
  (let ((query (if (pair? statements) (string-join statements "\n") statements))
        (order-statement (if order-by
			     (format #f "~%ORDER BY ~A" order-by)
			     "")))
    (conc (if with-graph (format #f "WITH ~A " with-graph) "")
          (format #f "SELECT ~A~%" (sparql-varlist vars))
          (if from-graph (format #f "FROM ~A~%" from-graph) "")
          (string-join
           (map (lambda (graph)
                  (format #f "FROM NAMED ~A~%" graph))
                from-named-graphs))
          (format #f "WHERE {~% ~A ~%} ~A"
                  query order-statement))))

(define (s-delete statements
                  #!key insert with-graph (from-graph (*default-graph*)) 
                  (from-named-graphs '()) where)
  (let ((statements (if (pair? statements) (string-join statements "\n") statements)))
    (conc (if with-graph  (format #f "WITH ~A " with-graph) "")
          (format #f "DELETE { ~A }~%" statements)
          (if insert (format #f "INSERT { ~A }~%" insert) "")
          (if from-graph (format #f "FROM ~A~%" from-graph) "")
          (string-join
           (map (lambda (graph)
                  (format #f "FROM NAMED ~A~%" graph))
                from-named-graphs))
          (format #f "WHERE {~% ~A ~%}~%" where))))

(define (expand-namespace-prefixes namespaces)
  (apply conc
	 (map (lambda (ns)
		(format #f "PREFIX ~A: <~A>~%"
			(car ns) (cadr ns)))
	      namespaces)))

(define (add-prefixes query)
  (format #f "~A~%~A"
	  (expand-namespace-prefixes (*namespaces*))
	  query))

(define (sparql/update query)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
      (format #t "~%~%Query:~%~%~A" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers '((content-type application/sparql-update))))
		   (add-prefixes query)
		   read-string)))
      (close-connection! uri)
      response)))

(define (sparql/select-unique query #!optional raw?)
  (car-when (sparql/select query raw?)))

(define (sparql/select query #!optional raw?)
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
	  (format #t "~%Query:~%~A~%" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers '((Content-Type application/x-www-form-urlencoded)
						     (Accept application/json))))
		   `((query . ,(add-prefixes query)))
                   read-json)))
      (close-connection! uri)
      (if raw? result (unpack-bindings result)))))

(define sparql-binding
  (match-lambda
    [(var (`type . "uri") . rest)
     (cons var (read-uri (alist-ref 'value rest)))]
    [(var (`type . "literal") . rest)
     (let ((lang (alist-ref 'xml:lang rest))
	   (value (alist-ref 'value rest)))
       (cons var (if lang (conc value "@" lang) value)))]
    [(var (`type . "typed-literal") . rest)
     (let ((datatype (alist-ref 'datatype rest))
	   (value (alist-ref 'value rest)))
       (match datatype
	 ("http://www.w3.org/2001/XMLSchema#integer"
	  (cons var (string->number value)))
	 (_ (cons var value))))]))

(define (unpack-bindings results)
  (map (lambda (binding)
	 (map sparql-binding binding))
	  (vector->list
	   (alist-ref 'bindings
                      (alist-ref 'results results)))))

(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings (vars ...) bindings body ...)
     (let ((vars (alist-ref (quote vars) bindings)) ...)
       body ...))))

(define-syntax query-with-vars
  (syntax-rules ()
    ((query-with-vars (vars ...) query form)
     (map (lambda (bindings)
            (with-bindings (vars ...) bindings form))
	  (sparql/select query)))))

(define-syntax query-unique-with-vars
  (syntax-rules ()
    ((query-unique-with-vars (vars ...) query form)
     (car-when (query-with-vars (vars ...) query form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup

(define-namespace foaf "http://xmlns.com/foaf/0.1/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")
(define-namespace rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-namespace owl "http://www.w3.org/2002/07/owl#")
(define-namespace skos "http://www.w3.org/2004/02/skos/core#")

)
