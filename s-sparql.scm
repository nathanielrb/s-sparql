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
  (make-parameter #f))

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

(define-syntax splice-when
  (syntax-rules ()
    ((splice-when body)
     (let ((body-val body))
       (splice-when body-val body-val)))
    ((splice-when test body)
     (if (or (not test) (null? test)) '() body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data types

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

(define (blank-node? obj)
  (and (symbol? obj)
       (or (equal? "_:" (substring (->string obj) 0 2))
           (equal? obj '|@[]|))))

(define (blank-node-path? obj)
  (and (list? obj)
       (blank-node? (car obj))))

(define (iri? obj)
  (and (symbol? obj)
       (not (sparql-variable? obj))
       (not (a? obj))))

(define (a? obj)
  (equal? 'a obj))

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

(define (write-sparql x #!optional (level 0))
  (cond ((string? x) (conc "\"" x "\""))
        ((keyword? x) (keyword->string x))
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (if x "true" "false"))
        ((pair? x) (or (write-sparql-literal x)
		       (write-sparql-special x)
                       (string-join (map (cut write-sparql <> (+ level 1))
					 x)
				    " ")))))

(define (langtag? x)
  (and (symbol? x)
       (equal? (substring (symbol->string x) 0 1) "@")))

(define (typed-or-langtag-literal? x)
  (and (pair? x) (not (list? x))
       (string? (car x)) (symbol? (cdr x))))

(define (write-sparql-literal x)
  (and (typed-or-langtag-literal? x) 
       (if (langtag? (cdr x))
           (format #f "\"~A\"~A" (car x) (cdr x))
           (format #f "\"~A\"^^~A" (car x) (cdr x)))))

(define functions '(COUNT SUM MIN MAX AVG SAMPLE STR LANG LANGMATCHES 
                          DATATYPE BOUND IRI URI BNODE RAND NIL ABS CEIL FLOOR ROUND IF
                          CONCAT STRLEN UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS
                          STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS 
                          TIMEZONE TZ NOW NIL UUID NIL STRUUID NIL MD5 SHA1 SHA256 SHA384
                          SHA512 COALESCE  STRLANG STRDT isIRI isURI isBLANK isLITERAL isNUMERIC
                          REGEX SUBSTR REPLACE EXISTS))

;;  GROUP_CONCAT 
;; 'GROUP_CONCAT' '(' 'DISTINCT'? Expression ( ';' 'SEPARATOR' '=' String )? ')'

(define (write-sparql-function x)
  (and (member (car x) functions)
       (format #f "~A(~A)"
               (car x)
               (string-join 
                (map write-sparql (cdr x))
                ", "))))               

(define binary-operators '(+ - * / = != <= >= < >))

(define (write-sparql-binary exp)
  (and (member (car exp) binary-operators)
       (format #f "~A ~A ~A"
               (cadr exp)
               (car exp)
               (caddr exp))))          

(define (write-sparql-special x #!optional (level 0))
  (let ((pre (apply conc (make-list level " "))))
    (or (write-sparql-function x)
        (write-sparql-binary x)
        (case (car x)
          ((@Unit) (string-join (map write-sparql (cdr x)) ";\n"))
          ((@Prologue @Query) (conc (string-join (map write-sparql (cdr x)) "\n") "\n"))
          ((@Update) (conc (string-join (map write-sparql (cdr x)) "\n") "\n"))
          ((@Dataset @Using) (string-join (map write-sparql (cdr x)) "\n"))

          ;; ?? how to differentiate between triple collections and functions etc.?
          ((|@()|) (format #f "(~A)"
                           (string-join 
                            (map (cut write-sparql <> (+ level 1)) (cdr x)) 
                            " ")))

          ((|@[]|) (format #f "[ ~A ]"
                           (string-join
                            (map write-sparql-properties (cdr x)) 
                            " ")))
          ((UNION)     
           (conc pre
                 (string-join  (map (cut write-sparql-triple <> (+ level 1)) (cdr x))
                               (format #f "~%~AUNION " pre))))
          ((GRAPH) (format #f "~AGRAPH ~A ~A  "
                           pre (write-sparql (cadr x))
                           (write-sparql-triple (cddr x) (+ level 1))))
          ((WHERE MINUS OPTIONAL DELETE INSERT
                  |DELETE WHERE| |DELETE DATA| |INSERT DATA|)
           (format #f "~A~A ~A" pre (car x) (write-sparql-triple (cdr x) (+ level 1))))
          ((BIND FILTER) (format #f "~A~A ~A"
                          pre (car x) (string-join (map write-sparql (cdr x)) " ")))
          ((AS) (format #f "(~A AS ~A)"
                        (write-sparql (cadr x))
                        (write-sparql (caddr x))))
          ((VALUES) (format #f "~AVALUES ~A { ~A }"
                            pre (write-sparql (cadr x)) (write-sparql (caddr x))))
          (else #f)))))
  
(define (write-sparql-triple triple #!optional (level 0))
  (if (null? triple)
      "{}"
      (or (write-sparql-special triple level)
          (let ((pre (apply conc (make-list level " "))))
            (if (pair? (car triple)) ;; list of triples
                
                ;; abstract the spacing
                ;; and think about singletons : { a b c. }
                (format #f "{~%~A~%~A}"
                        (string-join (map (cut write-sparql-triple <> (+ level 1)) triple) "\n")
                        pre)
                (conc
                 pre
                 (string-join
                  (match triple
                    ((subject properties) 
                     (list (write-sparql subject)
                           (write-sparql-properties properties)))
                    ((subject predicate objects)
                     (list (write-sparql subject)
                           (write-sparql-properties predicate)
                           (write-sparql-objects objects)))))
                 "."))))))

(define (write-sparql-properties x)
  (if (pair? x)
      (string-join (map (lambda (property)
                          (format #f "~A ~A"
                                  (write-sparql (car property))
                                  (write-sparql-objects (cadr property))))
                        x)
                   ";  ")
      (write-sparql x)))

(define (write-sparql-objects x)
  (if (pair? x)
      (or (write-sparql-literal x)
	  (write-sparql-special x)
          (string-join (map (lambda (y) (write-sparql y)) x) ", "))
      (write-sparql x)))

(define (expand-special triple)
  (cond ((blank-node-path? (car triple))
         (let ((subject (new-blank-node)))
           (print "blank subj")
            (append (expand-triple (cons subject (cdr triple)))
                    (expand-triple (cons subject (cdar triple))))))
        ((and (= (length triple) 3)
              (blank-node-path? (caddr triple)))
         (let ((object (new-blank-node)))
           (print "blank obj " triple)
           (match triple
             ((s p (_ . rest))
               (append (expand-triple (list s p object))
                       (expand-triple (cons object rest)))))))
        (else
         (case (car triple)
           ((WHERE DELETE INSERT) 
            (cons (car triple) (expand-triples (cdr triple))))
           ((|@[]|)
            (expand-triple (cons (new-blank-node) (cdr triple))))
           ((|@()| MINUS OPTIONAL)
            (list (cons (car triple) (expand-triples (cdr triple)))))
           ((UNION)
            (list (cons (car triple) (map expand-triples (cdr triple)))))
           ((GRAPH) (list (append (take triple 2)
                                  (expand-triples (cddr triple)))))
           ((FILTER BIND) (list triple))
           (else #f)))))

(define (expand-triples triples)
  (or (expand-special triples)
      (join (map expand-triple triples))))

(define (expand-expanded-triple s p o)
  (if (blank-node-path? o)
      (expand-special (list s p o))
      (list (list s p o))))

(define (expand-triple triple)
  (or (expand-special triple)
      (match triple
        ((subject predicates)
         (let ((subject (car triple)))
           (join
            (map (lambda (po-list)
                   (let ((predicate (car po-list))
                         (object (cadr po-list)))
                     (if (and (list? object) (not (blank-node-path? object)))
                         (join
                          (map (lambda (object)
                                 (expand-expanded-triple subject predicate object))
                               (cadr po-list)))
                         (expand-expanded-triple subject predicate object))))
                 predicates))))
        ((subject predicate objects)
         (if (list? objects)
             (join
              (map (lambda (object)
                     (expand-expanded-triple subject predicate object))
                   objects))
             (expand-expanded-triple subject predicate objects))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RDF Convenience Functions

(define (s-triple triple)
  (if (string? triple)
      triple
      (write-sparql-triple triple)))

(define (s-triples trips)
  (string-join (map s-triple trips) "\n"))

(define (triple a b c)
  (s-triple (list a b c)))

(define (triples trips)
  (string-join trips "\n"))

(define (s-bracketed statement)
  (format #f "{ ~A }" statement))         

(define (s-graph graph statements)
  (if graph
      (format #f "GRAPH ~A { ~A } ."
              (write-sparql graph)
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

;; (define (s-insert triples #!key (using-graph (*default-graph*)))
;;    `(@Update
;;      (INSERT ,@triples)
;;      ,@(splice-when (and using-graph `((@Using (USING ,using-graph)))))))

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

(define (sparql/update query #!key (additional-headers '()))
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
      (format #t "~%~%==Executing Query==~%~%~A" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers (append
                                                    additional-headers
                                                    '((content-type application/sparql-update)
                                                      (Accept application/json)))))
		   (add-prefixes query)
		   read-json)))
      (close-connection! uri)
      result)))

(define (sparql/select-unique query #!optional raw?)
  (car-when (sparql/select query raw?)))

(define (sparql/select query #!optional raw? #!key (additional-headers '()))
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
	  (format #t "~%==Executing Query==~%~A~%" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers (append
                                                    additional-headers
                                                    '((Content-Type application/x-www-form-urlencoded)
                                                      (Accept application/json)))))
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
