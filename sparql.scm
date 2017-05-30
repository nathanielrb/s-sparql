(use srfi-13 http-client intarweb uri-common medea matchable)

(require-extension srfi-13)

(define (read-uri uri)
  (and (string? uri)
       (string->symbol (conc "<" uri ">"))))

(define *default-graph*
  (make-parameter
   (or (read-uri (get-environment-variable "MU_DEFAULT_GRAPH"))
       '<http://mu.semte.ch/core/>)))

;; what about Docker?
(define *sparql-endpoint*
  (make-parameter
   (or (get-environment-variable "SPARQL_ENDPOINT")
       "http://127.0.0.1:8890/sparql")))

(define *print-queries?* (make-parameter #t))

(define *namespaces* (make-parameter '()))

(define *namespace-definitions*
  (or (get-environment-variable "NAMESPACES")
      "skos: http://www.w3.org/2004/02/skos/core#"))

(define-syntax hit-property-cache
  (syntax-rules ()
    ((hit-property-cache sym prop body)
     (or (get sym prop)
         (put! sym prop body)))))
    
;; to do : use this parameter in reify!
(define *expand-namespaces?* (make-parameter #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (sconc #!rest syms)
  (string->symbol 
   (apply conc (map ->string syms))))

;;useless I think
(define (assoc-get field object)
  (cdr (assoc field object)))

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
;; rdf

(define (reify x)
    (cond ((string? x) (conc "\"" x "\""))
	  ((keyword? x) (keyword->string x))
	  ((number? x) x)
	  ;;((list? x) (apply conc x))
          ((symbol? x) (symbol->string x))
	  ((namespace-pair? x) (expand-namespace x))))

(define (new-sparql-variable)
  (string->symbol (conc "?" (->string (gensym)))))

(define (sparql-variable str)
  (string->symbol (conc "?" (->string str))))

(define (s-triple trip)
  (match-let (((a b c) trip))
    (format #f "~A ~A ~A."
            (reify a)
            (reify b)
            (reify c))))

(define (triple a b c)
  (s-triple (list a b c)))

(define (bracketed statement)
  (format #f "{~A}" statement))         

(define (graph-statement graph statements)
  (if graph
      (format #f "GRAPH ~A { ~A } ."
              (reify graph)
              statements)
      statements))

(define (union statements)
  (string-join (map bracketed statements) " UNION "))

(define (s-optional statement)
  (format #f "OPTIONAL { ~A }" statement))

(define (s-filter statement filter)
  (format #f "~A FILTER (~A)" statement filter))

(define (s-bind as var)
  (format #f "BIND ~A as ~A" as var))

(define (triples trips)
  (string-join trips "\n"))

(define (s-triples trips)
  (string-join (map s-triple trips) "\n"))

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

(define format-triple
  (match-lambda 
    ((s p o)
     (format #f "~A ~A ~A .~%" s p o))))

;;  '(*top*
;;    (*namespaces* (foaf "http://foaf.org#")
;;                  (owl "http://owl.com/"))
;;    (*triples*
;;     (foaf:cake owl:likes foaf:icing)))
;; (define (full-triples triples)
;;  (let ((namespaces (assoc-val '*namespaces* (cdr triples)))
;;	(triples (assoc-val '*triples* (cdr triples))))

(define write-triple
  (match-lambda 
    ((s p o)
     (format-triple (map reify (list s p o))))))

(define (write-triples triples)
  (apply conc (map write-triple triples)))

(define (register-namespace name namespace)
  (*namespaces* (cons (list name namespace) (*namespaces*))))

(define (lookup-namespace name)
  (car-when (alist-ref name (*namespaces*))))

(define (expand-namespace ns-pair)
  (read-uri (format #f "~A~A" (lookup-namespace (car ns-pair)) (cadr ns-pair))))

(define (write-expand-namespace ns-pair)
  (format #f "~A~A" (lookup-namespace (car ns-pair)) (cadr ns-pair)))

(define (namespace-pair? x)
  (pair? x))

;; or consider a general function (expand-namespace mu 'pred)
(define-syntax define-namespace
  (syntax-rules ()
    ((define-namespace name namespace)
     (begin
       (register-namespace (quote name) namespace) ;  (->string
       (define (name elt)
         (read-uri (conc namespace elt)))))))

(define (insert-triples triples  #!key (graph (*default-graph*)))
  (format #f "WITH ~A~%INSERT {~%  ~A ~%}"
	  graph
	  triples))

(define (delete-triples triples  #!key (graph (*default-graph*)) (where #f))
  (conc
   (format #f "WITH ~A~%DELETE {~%  ~A ~%}" graph triples)
   (if where (format #f "~%WHERE {~% ~A ~%}" where) "")))

(define (sparql-vars vars)
  (if (pair? vars)
      (string-join (map ->string vars) ", ")
      (->string vars)))

(define (select-triples vars statements #!key (graph (*default-graph*)) order-by)
  (let ((query (if (pair? statements) (string-join statements "\n") statements))
        (order-statement (if order-by
			     (format #f "~%ORDER BY ~A" order-by)
			     "")))
    (format #f "WITH ~A~%SELECT ~A~%WHERE {~% ~A ~%} ~A"
	    graph (sparql-vars vars) query order-statement)))

(define (select-from vars statements
		     #!key (graph (*default-graph*)) (named-graphs '()) order-by)
  (let ((query (if (pair? statements) (string-join statements "\n") statements))
        (order-statement (if order-by
			     (format #f "~%ORDER BY ~A" order-by)
			     "")))
    (format #f (conc "SELECT ~A~%"
		     "FROM ~A~%"
		     (string-join
		      (map (lambda (graph)
			     (format #f "FROM NAMED ~A~%" graph))
			   named-graphs))
		     "WHERE {~% ~A ~%} ~A")
	    (sparql-vars vars) graph query order-statement)))

(define (delete-from statements
		     #!key (graph (*default-graph*)) (named-graphs '()) where)
  (let ((statements (if (pair? statements) (string-join query "\n") statements)))
    (format #f (conc "DELETE { ~A }~%"
                     "FROM ~A~%"
                     (string-join
                      (map (lambda (graph)
                             (format #f "FROM NAMED ~A~%" graph))
                           named-graphs))
                     "WHERE {~% ~A ~%}~%")
            statements graph where)))

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
	   (assoc-get 'bindings
		     (assoc-get 'results results)))))

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

(map (lambda (ns) 
       (match-let (((prefix uri)
                    (irregex-split ": " ns)))
         (register-namespace (string->symbol prefix)                                
                             uri)))
     (string-split *namespace-definitions* ","))
       
(define-namespace mu "http://mu.semte.ch/vocabularies/core/")

;; what about Docker??
(debug-file
 (or (get-environment-variable "LOG_FILE")
     "./debug.log"))
