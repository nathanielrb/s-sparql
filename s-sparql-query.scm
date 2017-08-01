;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Querying SPARQL Endpoints
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
           (map sparql-binding binding))
         (vector->list
          (alist-ref 
           'bindings (alist-ref 'results (string->json results)))))))

(define sparql-binding
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

(define unpack-sparql-bindings
  (json-unpacker sparql-binding))

(define untyped-binding
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

(define unpack-bindings
  (json-unpacker untyped-binding))

(define *sparql-query-unpacker* (make-parameter unpack-bindings))

(define (sparql-select query #!rest args)
  (let ((endpoint (*sparql-endpoint*))
        (unpack (*sparql-query-unpacker*))
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
