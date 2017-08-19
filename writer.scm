(use s-sparql s-sparql-parser matchable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Sparql
(define (write-uri uri)
  (let ((str (symbol->string uri)))
    (substring str 1 (- (string-length str) 1))))

;; leftover?
(define (write-expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
  (if (s-iri? ns-pair)
      (write-uri ns-pair)
      (expand-namespace* ns-pair namespaces)))

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

(define (rdf->json exp)
  (cond ((symbol? exp) (write-uri exp))
	(else exp)))

(define (value exp)
  (cond ((symbol? exp)
         (let ((s (symbol->string exp)))
           (if (string-contains s "://")
               (substring s 1 (- (string-length s) 1))
               s)))
        ((typed-or-langtag-literal? exp) (car exp))
        (else exp)))

(define (write-sparql-typed-literal exp)
  (and (typed-or-langtag-literal? exp) 
       (if (langtag? (cdr exp))
           (format #f "\"~A\"~A" (car exp) (cdr exp))
           (format #f "\"~A\"^^~A" (car exp) (cdr exp)))))

;; replace write-sparql with write-sparql-element
(define (write-sparql-inverse-element exp)
  (and (inverse-element? exp)
       (format #f "~A~A" (car exp) (sparql (cadr exp)))))

(define (write-sparql-element-path exp)
  (and (element-path? exp)
       (format #f "~A/~A" 
               (sparql (cadr exp))
               (sparql (caddr exp)))))

(define (write-sparql-modified-path exp)
  (and (modified-path? exp)
       (format #f "~A~A" (sparql (cadr exp)) (car exp))))

(define (write-sparql-negated-set exp)
  (and (negated-set? exp)
       (format #f "!~A" (sparql (cadr exp)))))

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

(define (write-sparql-element exp)
  (cond ((string? exp) (conc "\"" exp "\""))
        ((keyword? exp) (keyword->string exp))
        ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((boolean? exp) (if exp "true" "false"))
        ((pair? exp) (or (write-sparql-typed-literal exp)
                         (write-sparql-path exp)))))

(define functions '(COUNT SUM MIN MAX AVG SAMPLE STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND NIL ABS CEIL FLOOR ROUND IF CONCAT STRLEN UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW NIL UUID NIL STRUUID NIL MD5 SHA1 SHA256 SHA384 SHA512 COALESCE  STRLANG STRDT isIRI isURI isBLANK isLITERAL isNUMERIC REGEX SUBSTR REPLACE EXISTS))

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

(define (write-triple-properties exp)
  (let ((W (lambda (property)
             (format #f "~A ~A"
                     (write-triple-properties (car property))
                     (write-triple-objects (cadr property))))))
  (or (write-sparql-path exp)                         
      (if (pair? exp)
          (string-join (map W exp) ";  ")
          (sparql exp)))))

(define (write-triple-objects exp)
  (if (pair? exp)
      (or (write-sparql-typed-literal exp)
          (string-join (map (lambda (y) (sparql y)) exp) ", "))
      (sparql exp)))

(define (write-triple triple #!optional (level 0))
  (let ((pre (apply conc (make-list level " "))))
    (conc
     pre
     (string-join
      (match triple
	((subject properties) 
	 (list (sparql subject)
	       (write-triple-properties properties)))
	((subject predicate objects)
	 (list (sparql subject)
	       (write-triple-properties predicate)
	       (write-triple-objects objects)))))
     ".")))

(define (sparql exp #!optional (level 0))
  (or (write-sparql-element exp)
      (write-sparql-function exp)
      (write-sparql-binary exp)
      ))

(define (swrite block #!optional (bindings '()) (rules (*rules*)))
  (rewrite* block bindings rules conc ""))

(define (inc bindings)
  (fold-binding 1 'level + 0 bindings))

(define (pre bindings)
  (apply conc (make-list (get-binding/default 'level bindings 0) " ")))

;; (proc block rw)
(define (sw/node proc)
  (lambda (block bindings)
    (let-values (((rw _) (swrite (cdr block) (inc bindings))))
      (values (format "~%~A~A" (pre bindings) (proc block rw)) bindings))))

(define (sw/val proc)
  (lambda (block bindings)
    (values (proc block) bindings)))

(define (sw/list proc)
  (lambda (block bindings)
    (print (car block) bindings)
    (let-values (((rw _) (swrite block (inc bindings))))
      (values (format "~%~A~A" (pre bindings) (proc block rw)) bindings))))

(define sw/continue
  (sw/list
   (lambda (block rw) rw)))

(define sw/obj
  (lambda (block bindings)
    (let-values (((rw _) (swrite (cdr block) bindings)))
      (values rw bindings))))
         
(define (sw/polish block bindings)
  (values
   (format "(~A ~A ~A)"
	   (swrite (list (second block)))
	   (car block)
	   (swrite (list (third block))))
   '()))

(define (sw/copy block bindings)
  (values (format "~%~A~A"
		  (pre bindings)
		  (string-join (map symbol->string (flatten block)) " ")) 
	  '()))

(define (sw/literal exp bindings)
  (values (->string exp) '()))

(define (sw/block proc)
  (lambda (block bindings)
    (values (format "~A~A" (pre bindings) (proc block))
	    '())))

(define srules
  `((,symbol? . ,sw/literal)
    (,number? . ,sw/literal)
    ((|@()|) . ,(sw/node
		 (lambda (block rw)
		   (format "(~A)" rw))))
    (,functions . ,(sw/val
		    (lambda (block)
		      (format "~A(~A)" (car block) (swrite (cdr block))))))
    (,triple? 
     . ,(lambda (triple bindings)
	  (let ((level (get-binding/default 'level bindings 0)))
	    (values (write-triple triple level) '()))))
    ((GRAPH) 
     . ,(lambda (block bindings)
	  (values (format "GRAPH ~A{ ~A}" (second block) (swrite (cddr block) (inc bindings)))
		  '())))
    ((UNION)
     . ,(lambda (block bindings)
	  (values (string-join (map
				(lambda (b)
				  (swrite (list b) (inc bindings)))
				(cdr block)) 
			       (format "~AUNION" (pre bindings)))
		  '())))
    (,quads-block? 
     . ,(lambda (block bindings)
	  (values (format "~%~A~A {~A~A}" 
			  (pre bindings)
			  (car block)
			  (pre bindings)
			  (swrite (cdr block) (inc bindings))
			  (pre bindings))
		  '())))
    ((@QueryUnit @UpdateUnit @Query @Update @SubSelect @Prologue) . ,sw/obj)
    ((@Dataset @Using) . ,sw/obj)
    ((PREFIX) . ,sw/copy)
    ((SELECT |SELECT DISTINCT| |SELECT REDUCED|) . ,sw/continue)
    ((AS) . ,sw/polish)
    (,list? 
     . ,(lambda (block bindings)
	  (values (format "~%~A{~%~A~A~%~A}~%" 
			 (pre bindings)
			 (pre bindings)
			 (swrite block (inc bindings))
			 (pre bindings))
		 '())))))
;;    ((@Blank) . ,(lambda (block bindings)
;; VALUES

(define (write-sparql exp)
  (swrite (list exp) '() srules))

(define t1 (parse-query "SELECT ?s WHERE { SELECT ?s WHERE { ?s ?p ?o } }"))
(define t2 (parse-query "SELECT ?s WHERE { { SELECT ?s WHERE { ?s ?p ?o } } }"))
(define t3 (parse-query "SELECT ?s WHERE { GRAPH <G> { ?s ?p ?o. ?a ?b ?d, ?e } }"))
(define t4 (parse-query "SELECT ?s WHERE { { ?s ?p ?o } UNION { ?s ?p ?u } }"))
(define t5 (parse-query "SELECT ((COUNT(?a)) AS ?count) WHERE { { ?s ?p ?o } UNION { ?s ?p ?u } }"))
(define t6 (parse-query "SELECT (COUNT(?a) AS ?count) WHERE { { ?s ?p ?o } UNION { ?s ?p ?u } }"))


;; (define (write-sparql-special exp #!optional (level 0))
;;   (let ((pre (apply conc (make-list level " "))))
;;     (or (write-sparql-function exp)
;;         (write-sparql-binary exp)
;;         (case (car exp)
;;           ((@Unit) (string-join (map write-sparql (cdr exp)) ";\n"))
;;           ((@Prologue @Query) (conc (string-join (map write-sparql (cdr exp)) "\n") "\n"))
;;           ((@Update) (conc (string-join (map write-sparql (cdr exp)) "\n") "\n"))
;;           ((@Dataset @Using) (string-join (map write-sparql (cdr exp)) "\n"))

;; 	  ;; arglist, collections (list?) in ()

;;           ((@Blank) (format #f "[ ~A ]"
;;                            (string-join
;;                             (map write-triple-properties (cdr exp)) 
;;                             " ")))
;; 	  ((@SubSelect) (format #f "~A~A ~A"
;; 				pre (write-sparql (second exp)) (write-sparql (third exp))))
;;           ((SELECT |SELECT DISTINCT| |SELECT REDUCED|)
;;            (format #f "~A~A ~A"
;;                    pre (car exp)
;;                    (string-join (map write-sparql (cdr exp)) " ")))
;;           ((UNION)     
;;            (conc pre
;;                  (string-join  (map (cut write-triple <> (+ level 1)) (cdr exp))
;;                                (format #f "~%~AUNION " pre))))
;;           ((GRAPH) (format #f "~AGRAPH ~A ~A  "
;;                            pre (write-sparql (cadr exp))
;;                            (write-triple (cddr exp) (+ level 1))))
;;           ((WHERE MINUS OPTIONAL DELETE INSERT
;;                   |DELETE WHERE| |DELETE DATA| |INSERT DATA|
;;                   CONSTRUCT)
;;            (format #f "~A~A ~A" pre (car exp) (write-triple (cdr exp) (+ level 1))))
;;           ((BIND FILTER) (format #f "~A~A ~A"
;;                           pre (car exp) (string-join (map write-sparql (cdr exp)) " ")))
;;           ((AS) (format #f "(~A AS ~A)"
;;                         (write-sparql (cadr exp))
;;                         (write-sparql (caddr exp))))
;;           ((VALUES) (format #f "~AVALUES ~A { ~A }"
;;                             pre (write-sparql (cadr exp)) (write-sparql (caddr exp))))
;;           (else #f)))))
  
;; (define (write-triples triples)
;;   (string-join (map s-triple triples) "\n"))

;; (define rules
;;   (define top-rules
;;   `((,symbol? . ,rw/copy)
;;     ((@QueryUnit @UpdateUnit) . ,rw/continue)
;;     ((@Prologue)
;;      . ,(lambda (block bindings)
;;           (values `((@Prologue
;;                      (PREFIX |rewriter:| <http://mu.semte.ch/graphs/>)
;;                      ,@(cdr block)))
;;                   bindings)))
;;     ((@Query)
;;      . ,(lambda (block bindings)
;; 	  (print "in query")
;;           (let ((rewrite-select-queries? (rewrite-select?)))
;;             (if rewrite-select-queries?
;;                 (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
;;                   (let ((constraints (get-binding/default 'constraints new-bindings '())))
;;                     (values `((,(car block)
;;                                ,@(alist-update 'WHERE
;;                                                (delete-duplicates
;;                                                 (append constraints (or (alist-ref 'WHERE rw) '())))
;;                                                rw)))
;;                             new-bindings)))
;;                 (with-rewrite ((rw (rewrite (cdr block) bindings select-query-rules)))
;;                               `((@Query ,rw)))))))
;;     ((@Update)
;;      . ,(lambda (block bindings)
;;           (let-values (((rw new-bindings) (rewrite (reverse (cdr block)) '())))
;;             (let ((where-block (or (alist-ref 'WHERE rw) '()))
;;                   (constraints (get-binding/default* '() 'constraints new-bindings '())))
;;               (let ((insert (or (alist-ref '|INSERT DATA| (cdr block))
;;                                 (alist-ref 'INSERT (cdr block)))))
;;                 (let ((constraints (if insert
;;                                        (let ((triples (rewrite insert '() (expand-triples-rules #t #t))))
;;                                          (instantiate (delete-duplicates constraints) triples))
;;                                        constraints)))
;;                   (values `((@Update . ,(alist-update
;;                                          'WHERE
;;                                          `((SELECT *) (WHERE ,@(delete-duplicates
;;                                                                 (append constraints where-block))))
;;                                          (reverse rw))))
;;                           new-bindings)))))))
;;     ((@Dataset) . ,rw/remove)
;;     ((@Using) . ,rw/remove)
;;     ((GRAPH) . ,rw/copy)
;;     ((*REWRITTEN*)
;;      . ,(lambda (block bindings)
;;           (values (cdr block) bindings)))
;;     (,select? . ,rw/copy)
;;     (,subselect? . ,rewrite-subselect)        
;;     ((@Subselect) . ,rewrite-subselect)
;;     (,quads-block? . ,rewrite-quads-block)
;;     ((FILTER BIND |ORDER| |ORDER BY| |LIMIT|) . ,rw/copy)
;;     `((,where-subselect?
;;        . ,(lambda (block bindings)
;;             (let-values (((rw b) (rewrite-subselect (cdr block) bindings)))
;;               (values `((WHERE ,@rw))
;;                       (merge-bindings b bindings))))))
;;     ((|GROUP BY| OFFSET LIMIT) . ,rw/copy)
;;     (,list? . ,rw/list))))
