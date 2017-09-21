(module sparql-query *
(import chicken scheme extras data-structures srfi-1) 

(use srfi-13 srfi-69 http-client intarweb uri-common medea cjson matchable irregex)

(define *sparql-endpoint*
  (make-parameter
   "http://127.0.0.1:8890/sparql"))

(define *sparql-update-endpoint*
  (make-parameter
   "http://127.0.0.1:8890/sparql"))

(define *default-graph*
  (make-parameter #f))

(define *print-queries?* (make-parameter #t))

(define *namespaces* (make-parameter '()))

(define *expand-namespaces?* (make-parameter #t))

(define-inline (car-when p)
  (and (pair? p) (car p)))

(define-inline (cdr-when p)
  (and (pair? p) (cdr p)))

(define-inline (cons-when exp p)
  (if exp (cons exp p) p))

(define (read-uri uri)
  (and (string? uri)
       (if  (or (substring=? "http://" uri)
                (substring=? "https://" uri))
            (string->symbol (conc "<" uri ">"))
           (string->symbol uri))))

(define (sparql-escape-string str)
    (conc "\"" str "\""))

(define (sparql-escape-uri uri)
  (conc "<" uri ">"))

(define (sparql-escape-boolean exp)
  (or (and (boolean? exp) (if exp "true" "false"))
      (and (string? exp) 
           (or (and (equal? exp "true") "true")
               (and (equal? exp "false") "false")))))

(define-inline (langtag? exp)
  (let ((s (if (symbol? exp) (symbol->string exp) exp)))
    (equal? (substring s 0 1) "@")))

(define (sparql-escape-literal exp #!optional (joint " "))
  (or (sparql-escape-string exp)
      (sparql-escape-boolean exp)
      (and (keyword? exp) (keyword->string exp))
      (and (number? exp) (number->string exp))
      (and (symbol? exp) (symbol->string exp))
      (and (list? exp) (string-join (map sparql-escape-literal exp) joint))
      (and (pair? exp)
           (if (langtag? (cdr exp))
               (format "~A~A" (sparql-escape-literal (car exp)) (cdr exp))
               (format "~A^^~A" (sparql-escape-literal (car exp)) (cdr exp))))))
                      
(define (register-namespace name namespace)
  (*namespaces* (cons (list name namespace) (*namespaces*))))

(define (lookup-namespace name #!optional (namespaces (*namespaces*)))
  (car (alist-ref name namespaces)))

(define-syntax define-namespace
  (syntax-rules ()
    ((_ name namespace)
     (register-namespace (quote name) namespace))))

(define (expand-namespace-prefixes namespaces)
  (apply conc
	 (map (lambda (ns)
		(format #f "PREFIX ~A: <~A>~%" (car ns) (cadr ns)))
	      namespaces)))

(define (add-prefixes query)
  (format #f "~A~%~A"
	  (expand-namespace-prefixes (*namespaces*))
	  query))

(define sparql-headers (make-parameter '()))

(define (sparql-update query #!rest args)
  (let ((endpoint (*sparql-endpoint*))
        (query (apply format #f query args)))
    (when (*print-queries?*)
      (format (current-error-port) "~%~%==Executing Query==~%~%~A" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers (append
                                                    (sparql-headers)
                                                    '((content-type application/sparql-update)
                                                      (Accept application/json)))))
		   (add-prefixes query)
		   read-json)))
      (close-connection! uri)
      result)))

(define (json-unpacker unbinder)
  (lambda (results)
    (map (lambda (binding)
           (map unbinder binding))
         (vector->list
          (alist-ref 
           'bindings (alist-ref 'results (string->json results)))))))

(define sparql-binding
  (match-lambda
    ((var . bindings)
     (let ((value (alist-ref 'value bindings))
           (type (alist-ref 'type bindings)))
       (match type
         ("typed-literal"
          (let ((datatype (alist-ref 'datatype bindings)))
            (case datatype
              (("http://www.w3.org/2001/XMLSchema#integer")
               (cons var (string->number value)))
              (else (cons var value)))))
         (else (cons var value)))))))

(define sparql-bindings
  (json-unpacker sparql-binding))

(define typed-sparql-binding
  (match-lambda
    ((var . bindings)
     (let ((value (alist-ref 'value bindings))
           (type (alist-ref 'type bindings)))
       (match type
         ("literal"
          (let ((lang (alist-ref 'xml:lang bindings)))
            (cons var
                  (if lang
                      (conc value "@" lang) 
                      value))))
         ("typed-literal"
          (let ((datatype (alist-ref 'datatype bindings)))
            (case datatype
              (("http://www.w3.org/2001/XMLSchema#integer")
               (cons var (string->number value)))
              (else (cons var value)))))
         ("uri"
          (cons var (read-uri (alist-ref 'value bindings))))
         (else (cons var value)))))))

(define typed-sparql-bindings
  (json-unpacker typed-sparql-binding))

(define *query-unpacker* (make-parameter sparql-bindings))

(define (sparql-select query #!rest args)
  (let ((endpoint (*sparql-endpoint*))
        (unpack (*query-unpacker*))
        (query (apply format #f query args)))
    (when (*print-queries?*)
	  (format (current-error-port) "~%==Executing Query==~%~A~%" (add-prefixes query)))
    (let-values (((result uri response)
		  (with-input-from-request 
		   (make-request method: 'POST
				 uri: (uri-reference endpoint)
				 headers: (headers (append
                                                    (sparql-headers)
                                                    '((Content-Type application/x-www-form-urlencoded)
                                                      (Accept application/json)))))
		   `((query . ,(add-prefixes query)))
                   read-string)))
      (close-connection! uri)
      (unpack result))))

(define (sparql-select-unique query #!rest args)
  (car-when (apply sparql-select query args)))

(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings (vars ...) bindings body ...)
     (let ((vars (alist-ref (quote vars) bindings)) ...)
       body ...))))

(define-syntax query-with-vars
  (syntax-rules ()
    ((_ (vars ...) query form ...)
     (map (lambda (bindings)
            (with-bindings (vars ...) bindings form ...))
	  (sparql-select query)))))

(define-syntax query-unique-with-vars
  (syntax-rules ()
    ((query-unique-with-vars (vars ...) query form)
     (car-when (query-with-vars (vars ...) query form)))))

(define-namespace foaf "http://xmlns.com/foaf/0.1/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")
(define-namespace rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-namespace owl "http://www.w3.org/2002/07/owl#")
(define-namespace skos "http://www.w3.org/2004/02/skos/core#")
)
