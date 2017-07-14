(module s-sparql *
(import chicken scheme extras data-structures srfi-1) 

(use srfi-13 srfi-69 http-client intarweb uri-common medea matchable irregex)

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
    
(define (clear-hashed-cache! cache key)
  (hash-table-delete! cache key))

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

(define (cons-when exp p)
  (if exp (cons exp p) p))

;; (define-syntax splice-when
;;   (syntax-rules ()
;;     ((splice-when body)
;;      (let ((body-val body))
;;        (splice-when body-val body-val)))
;;     ((splice-when test body)
;;      (if (or (not test) (null? test)) '() body))))

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

(define (inverse-path? obj)
  (and (pair? obj)
       (equal? (car obj) '^)))

(define (sequence-path? obj)
  (and (pair? obj)
       (equal? (car obj) '/)))

(define (alternative-path? obj)
  (and (pair? obj)
       (equal? (car obj) '||)))

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

(define (property-path? obj)
  (or (inverse-path? obj)
      (sequence-path? obj)
      (alternative-path? obj)
      (zero-or-more-path? obj)
      (one-or-more-path? obj)
      (zero-or-one-path? obj)
      (negated-set? obj)))

(define (iri? obj)
  (and (symbol? obj)
       (not (sparql-variable? obj))
       (not (a? obj))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces 

(define (register-namespace name namespace)
  (*namespaces* (cons (list name namespace) (*namespaces*))))

(define (lookup-namespace name #!optional (namespaces (*namespaces*)))
  (car (alist-ref name namespaces)))

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
       (register-namespace (quote name) namespace)
       (define (name elt)
         (read-uri (conc namespace elt)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Sparql

(define (write-uri uri)
  (let ((str (symbol->string uri)))
    (substring str 1 (- (string-length str) 1))))

(define (rdf->json exp)
  (cond ((symbol? exp) (write-uri exp))
	(else exp)))

(define (write-sparql exp #!optional (level 0))
  (cond ((string? exp) (conc "\"" exp "\""))
        ((keyword? exp) (keyword->string exp))
        ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((boolean? exp) (if exp "true" "false"))
        ((pair? exp) (or (write-sparql-literal exp)
                         (write-sparql-path exp)                         
                         (write-sparql-special exp)
                         (string-join (map (cut write-sparql <> (+ level 1))
                                           exp)
                                      " ")))))

(define (write-sparql-literal exp)
  (and (typed-or-langtag-literal? exp) 
       (if (langtag? (cdr exp))
           (format #f "\"~A\"~A" (car exp) (cdr exp))
           (format #f "\"~A\"^^~A" (car exp) (cdr exp)))))

(define (write-sparql-inverse-element exp)
  (and (inverse-element? exp)
       (format #f "~A~A" (car exp) (write-sparql (cadr exp)))))

(define (write-sparql-element-path exp)
  (and (element-path? exp)
       (format #f "~A/~A" 
               (write-sparql (cadr exp))
               (write-sparql (caddr exp)))))

(define (write-sparql-modified-path exp)
  (and (modified-path? exp)
       (format #f "~A~A" (write-sparql (cadr exp)) (car exp))))

(define (write-sparql-negated-set exp)
  (and (negated-set? exp)
       (format #f "!~A" (write-sparql (cadr exp)))))

(define (write-sparql-alternative-path exp)
  (and (alternative-path? exp)
       (string-join
        (map write-sparql (cdr exp))
        "|")))

(define (write-sparql-path exp)
  (or
   (write-sparql-element-path exp)
   (write-sparql-modified-path exp)
   (write-sparql-inverse-element exp)
   (write-sparql-negated-set exp)
   (write-sparql-alternative-path exp)))


(define functions '(COUNT SUM MIN MAX AVG SAMPLE STR LANG LANGMATCHES 
                          DATATYPE BOUND IRI URI BNODE RAND NIL ABS CEIL FLOOR ROUND IF
                          CONCAT STRLEN UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS
                          STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS 
                          TIMEZONE TZ NOW NIL UUID NIL STRUUID NIL MD5 SHA1 SHA256 SHA384
                          SHA512 COALESCE  STRLANG STRDT isIRI isURI isBLANK isLITERAL isNUMERIC
                          REGEX SUBSTR REPLACE EXISTS))

(define (write-sparql-function exp)
  (and (member (car exp) functions)
       (format #f "~A(~A)"
               (car exp)
               (string-join 
                (map write-sparql (cdr exp))
                ", "))))               

(define binary-operators '(+ - * / = != <= >= < >))

(define (write-sparql-binary exp)
  (and (member (car exp) binary-operators)
       (format #f "~A ~A ~A"
               (cadr exp)
               (car exp)
               (caddr exp))))          

(define (write-sparql-special exp #!optional (level 0))
  (let ((pre (apply conc (make-list level " "))))
    (or (write-sparql-function exp)
        (write-sparql-binary exp)
        (case (car exp)
          ((@Unit) (string-join (map write-sparql (cdr exp)) ";\n"))
          ((@Prologue @Query) (conc (string-join (map write-sparql (cdr exp)) "\n") "\n"))
          ((@Update) (conc (string-join (map write-sparql (cdr exp)) "\n") "\n"))
          ((@Dataset @Using) (string-join (map write-sparql (cdr exp)) "\n"))

          ;; ?? how to differentiate between triple collections and functions etc.?
          ((|@()|) (format #f "(~A)"
                           (string-join 
                            (map (cut write-sparql <> (+ level 1)) (cdr exp)) 
                            " ")))

          ((|@[]|) (format #f "[ ~A ]"
                           (string-join
                            (map write-triple-properties (cdr exp)) 
                            " ")))
          ((UNION)     
           (conc pre
                 (string-join  (map (cut write-triple <> (+ level 1)) (cdr exp))
                               (format #f "~%~AUNION " pre))))
          ((GRAPH) (format #f "~AGRAPH ~A ~A  "
                           pre (write-sparql (cadr exp))
                           (write-triple (cddr exp) (+ level 1))))
          ((WHERE MINUS OPTIONAL DELETE INSERT
                  |DELETE WHERE| |DELETE DATA| |INSERT DATA|)
           (format #f "~A~A ~A" pre (car exp) (write-triple (cdr exp) (+ level 1))))
          ((BIND FILTER) (format #f "~A~A ~A"
                          pre (car exp) (string-join (map write-sparql (cdr exp)) " ")))
          ((AS) (format #f "(~A AS ~A)"
                        (write-sparql (cadr exp))
                        (write-sparql (caddr exp))))
          ((VALUES) (format #f "~AVALUES ~A { ~A }"
                            pre (write-sparql (cadr exp)) (write-sparql (caddr exp))))
          (else #f)))))
  
(define (write-triple triple #!optional (level 0))
  (if (null? triple)
      "{}"
      (or (write-sparql-special triple level)
          (let ((pre (apply conc (make-list level " "))))
             ;; list of triples
            (if (pair? (car triple))
                ;; (if (= (length triple) 1)
                ;;     (format #f "{ ~A }"
                ;;         (write-triple (car triple) (+ level 1)))
                (format #f "{~%~A~%~A}"
                        (string-join (map (cut write-triple <> (+ level 1)) triple) "\n")
                        pre)
                (conc
                 pre
                 (string-join
                  (match triple
                    ((subject properties) 
                     (list (write-sparql subject)
                           (write-triple-properties properties)))
                    ((subject predicate objects)
                     (list (write-sparql subject)
                           (write-triple-properties predicate)
                           (write-triple-objects objects)))))
                 "."))))))

(define (write-triple-properties exp)
  (if (pair? exp)
      (string-join (map (lambda (property)
                          (format #f "~A ~A"
                                  (write-sparql (car property))
                                  (write-triple-objects (cadr property))))
                        exp)
                   ";  ")
      (write-sparql exp)))

(define (write-triple-objects exp)
  (if (pair? exp)
      (or (write-sparql-literal exp)
          (write-sparql-path exp)                         
	  (write-sparql-special exp)
          (string-join (map (lambda (y) (write-sparql y)) exp) ", "))
      (write-sparql exp)))

(define (write-triples triples)
  (string-join (map s-triple triples) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand RDF triples

(define (expand-special triple)
  (cond ((blank-node-path? (car triple))
         (let ((subject (new-blank-node)))
            (append (expand-triple (cons subject (cdr triple)))
                    (expand-triple (cons subject (cdar triple))))))
        ((and (= (length triple) 3)
              (blank-node-path? (caddr triple)))
         (let ((object (new-blank-node)))
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

(define (expand-sequence-path-triple triple)
  (and (= (length triple) 3)
       (sequence-path? (cadr triple))
       (match triple
         ((s (`/ . ps) o)
          (let loop ((s s)
                     (ps ps))
            (if (= (length ps) 1)
                (expand-triple (list s (car ps) o))
                (let ((object (new-blank-node)))
                  (append (expand-triple (list s (car ps) object))
                          (loop object (cdr ps))))))))))

(define (expand-triples triples)
  (or (expand-special triples)
      (join (map expand-triple triples))))

(define (expand-expanded-triple s p o)
  (cond  ((blank-node-path? o)
          (expand-special (list s p o)))
         ((sequence-path? p)
          (expand-sequence-path-triple (list s p o)))
         (else
          (list (list s p o)))))

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
;; Convenience Functions

(define (s-triple triple)
  (if (string? triple)
      triple
      (write-triple triple)))

(define s-triples write-triples)

;; x
(define (triple a b c)
  (s-triple (list a b c)))

;; x
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
      (string-join (map ->string vars) " ")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPARQL Endpoint

(define (sparql/update query #!key (additional-headers '()))
  (let ((endpoint (*sparql-endpoint*)))
    (when (*print-queries?*)
      (format (current-error-port) "~%~%==Executing Query==~%~%~A" (add-prefixes query)))
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
	  (format (current-error-port) "~%==Executing Query==~%~A~%" (add-prefixes query)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup

(define-namespace foaf "http://xmlns.com/foaf/0.1/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")
(define-namespace rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-namespace owl "http://www.w3.org/2002/07/owl#")
(define-namespace skos "http://www.w3.org/2004/02/skos/core#")

)
