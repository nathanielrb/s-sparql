;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing Sparql
(define *write-annotations?* (make-parameter #f))

(define (write-uri uri)
  (let ((str (symbol->string uri)))
    (substring str 1 (- (string-length str) 1))))

(define (write-expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
  (if (s-iri? ns-pair)
      (write-uri ns-pair)
      (expand-namespace* ns-pair namespaces)))

(define (expand-namespace* pair namespaces)
  (format "~A~A"
          (lookup-namespace (string->symbol (car pair)) namespaces)
          (cadr pair)))

(define (expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
  (if (or (sparql-variable? ns-pair)  (s-iri? ns-pair) (blank-node? ns-pair))
      ns-pair
      (let ((pair (string-split (->string ns-pair) ":")))
        (if (equal? (length pair) 2)
            (string->symbol
             (format "<~A>" (expand-namespace* pair namespaces)))
            ns-pair))))

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
           (format "\"~A\"~A" (car exp) (cdr exp))
           (format "\"~A\"^^~A" (car exp) (cdr exp)))))

(define (write-sparql-inverse-element exp)
  (and (inverse-element? exp)
       (format "~A~A" (car exp) (sparql->string (cadr exp)))))

(define (write-sparql-element-path exp)
  (and (element-path? exp)
       (format "~A/~A" 
               (sparql->string (cadr exp))
               (sparql->string (caddr exp)))))

(define (write-sparql-modified-path exp)
  (and (modified-path? exp)
       (format "~A~A" (sparql->string (cadr exp)) (car exp))))

(define (write-sparql-negated-set exp)
  (and (negated-set? exp)
       (format "!~A" (sparql->string (cadr exp)))))

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

(define (write-sparql-value exp)
  (cond ((string? exp) (conc "\"" exp "\""))
        ((keyword? exp) (keyword->string exp))
        ((number? exp) (number->string exp))
        ((symbol? exp) (symbol->string exp))
        ((boolean? exp) (if exp "true" "false"))
        ((pair? exp) (or (write-sparql-typed-literal exp)
                         (write-sparql-path exp)))))

(define aggregates
  '(COUNT SUM MIN MAX AVG SAMPLE GROUP_CONCAT))

(define functions
  '(STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND NIL ABS CEIL FLOOR ROUND IF CONCAT STRLEN UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW NIL UUID NIL STRUUID NIL MD5 SHA1 SHA256 SHA384 SHA512 COALESCE  STRLANG STRDT isIRI isURI isBLANK isLITERAL isNUMERIC REGEX SUBSTR REPLACE))

(define (write-sparql-function exp)
  (and (member (car exp) (append functions aggregates))
       (format "~A(~A)"
               (car exp)
               (string-join 
                (map write-sparql (cdr exp))
                ", "))))               

(define (write-sparql-aggregate exp)
  (and (member (car exp) aggregates)
       (format "~A(~A)"
               (car exp)
               (match (cdr exp)
                 ((`DISTINCT exp) (format "DISTINCT ~A" (write-sparql exp)))
                 (else
                  (write-sparql (cdr exp)))))))

(define arithmetic-operators '(+ - * /))

(define binary-operators '(= != <= >= < > && \|\|))

(define (write-sparql-binary exp)
  (and (member (car exp) binary-operators)
       (format "~A ~A ~A"
               (cadr exp)
               (car exp)
               (caddr exp))))          

(define (write-triple-properties exp)
  (let ((W (lambda (property)
             (format "~A ~A"
                     (write-triple-properties (car property))
                     (write-triple-objects (cadr property))))))
  (or (write-sparql-path exp)                         
      (if (pair? exp)
          (string-join (map W exp) ";  ")
          (sparql->string exp)))))

(define (write-triple-objects exp)
  (if (pair? exp)
      (or (write-sparql-typed-literal exp)
	  (write-sparql-blank-node exp)
          (string-join (map (lambda (y) (sparql->string y)) exp) ", "))
      (sparql->string exp)))

(define (write-triple triple)
  (conc
   (string-join
    (match triple
      (((`@Blank . properties))
       (list (write-sparql-blank-node (car triple))))
	
      ((subject properties) 
       (list (sparql->string subject)
	     (write-triple-properties properties)))
      ((subject predicate objects)
       (list (sparql->string subject)
	     (write-triple-properties predicate)
	     (write-triple-objects objects)))))
   "."))

(define (write-sparql-blank-node node)
  (match node
    ((`@Blank . properties)
     (format "[~A]"
	     (string-join
	      (map write-triple-properties properties)
	      " ")))
    (else #f)))

(define (sparql->string exp #!optional (level 0))
  (or (write-sparql-blank-node exp)
      (write-sparql-value exp)
      (write-sparql-aggregate exp)
      (write-sparql-function exp)
      (write-sparql-binary exp)
      ))

(define (swrite block #!optional (bindings '()) (rules (*rules*)))
  (let ((concatter (lambda (a b)
                     (cond ((equal? b "") a)
                           (else
                            (conc a
                                  (if (equal? a "") ""
                                      (get-binding/default 'separator bindings ""))
                                  (if (get-binding 'linebreak bindings)
                                      (conc "\n" (pre bindings))
                                      "")
                                  b))))))
    (rewrite* block bindings rules concatter  "")))

(define (sep s bindings)
  (update-binding 'separator s bindings))

(define (zero bindings)
  (update-binding 'level 0 bindings))

(define (inc bindings)
  (fold-binding 1 'level + 0 bindings))

(define (one bindings)
    (update-binding 'level 1 bindings))

(define (linebreak bindings)
  (update-binding 'linebreak #t bindings))

(define (nobreak bindings)
  (update-binding 'linebreak #f bindings))

(define (pre bindings)
  (apply conc (make-list (get-binding/default 'level bindings 0) " ")))

(define sw/obj
  (lambda (block bindings)
    (if (null? (cdr block)) (values "" bindings)
	(let-values (((rw _) (swrite (cdr block) (linebreak bindings))))
	  (values rw bindings)))))
         
(define (sw/polish block bindings)
  (values
   (format "(~A ~A ~A)"
	   (swrite (list (second block)) bindings)
	   (car block)
	   (swrite (list (third block)) bindings))
   bindings))

(define (sw/copy block bindings)
  (values (format "~A"
		  (string-join
                   (map symbol->string (flatten block)) " ")) 
	    bindings))

(define (sw/literal exp bindings)
  (values (->string exp) bindings))

(define srules
  `((,symbol? . ,sw/literal)
    (,number? . ,sw/literal)
    (,null?
     . ,(lambda (block bindings)
          (values "{}" bindings))) ; correct?
    (,string?
     . ,(lambda (str bindings)
          (values (sparql->string str) bindings)))
    ((@Annotation)
     . ,(lambda (exp b)
        (if (*write-annotations?*)
            (match exp
              ((`@Annotation label key)
               (values (format "@~A ~A" label key) b))
              ((`@Annotation label key var)
               (values (format "@~A ~A(~A)" label key var) b)))
            (values "" b))))
    ((@Prologue) 
     . ,(lambda (block bindings)
          (let-values (((rw b) (sw/obj block (sep "" bindings))))
            (if (equal? rw "") (values "" b)
                (values (conc rw "\n") b)))))
    ((@Query @Update @SubSelect) 
     . ,(lambda (block bindings)
           (sw/obj block (sep "" bindings))))
    ((@QueryUnit) 
     . ,(lambda (block bindings)
          (swrite (cdr block) bindings)))
    ((@UpdateUnit)
     . ,(lambda (block bindings)
          (sw/obj block (sep ";" bindings))))
    ((@Dataset @Using) . ,sw/obj)
    ((FROM USING) 
     . ,(lambda (block bindings)
          (values (format "~A ~A" (car block) (swrite (cdr block) (nobreak (sep " " bindings)))) 
                  bindings)))
    ((PREFIX) . ,sw/copy)
    ((SELECT |SELECT DISTINCT| |SELECT REDUCED| DESCRIBE ASK
      LOAD CLEAR DROP CREATE ADD MOVE COPY)
     . ,(lambda (block bindings)
	  (values (format "~A ~A" 
			  (car block)
			  (swrite (cdr block) (nobreak (zero (sep " " bindings)))))
		  bindings)))
    ((AS) . ,sw/polish)
    ((ASC DESC)
     . ,(lambda (block bindings)
          (values (format "~A(~A)" (car block) (swrite (cdr block) (zero (sep "," bindings))))
                  bindings)))
    (,functions
     . ,(lambda (block bindings)
          (values (format "~A(~A)" (car block) (swrite (cdr block) (zero (sep "," bindings))))
                  bindings)))
    (,aggregates
     . ,(lambda (block bindings)
          (values
           (format "~A(~A)" (car block)
                   (swrite (cdr block) (zero (sep " " bindings))))
           bindings)))
    ((DISTINCT NAMED WITH)
     . ,(lambda (block bindings)
          (values
           (format "~A ~A" (car block) (swrite (cdr block)))
           bindings)))
    ((CONSTRUCT WHERE 
      DELETE |DELETE WHERE| |DELETE DATA|
      INSERT |INSERT WHERE| |INSERT DATA|      
      EXISTS |NOT EXISTS| MINUS OPTIONAL)
     . ,(lambda (block bindings)
	  (values (format "~A ~A" 
			  (car block)
			  (swrite (list (cdr block)) (nobreak bindings)))
		  bindings)))
    ((GRAPH) 
     . ,(lambda (block bindings)
	  (values (format "GRAPH ~A ~A"
			  (second block)
			  (swrite (list (cddr block)) (nobreak bindings)))
		  bindings)))
    ((UNION)
     . ,(lambda (block bindings)
	  (values (string-join (map
				(lambda (b)
				  (swrite (list b) (nobreak bindings)))
				(cdr block)) 
			       (format "~%~AUNION " (pre bindings)))
		  bindings)))
    (,triple? 
     . ,(lambda (triple bindings)
	  (values (write-triple triple) bindings)))
    ((FILTER BIND HAVING) 
     . ,(lambda (block bindings)
          (values (format "~A ~A" (car block) (swrite (cdr block) (nobreak bindings)))
                  bindings)))
    ((+ -)
     . ,(lambda (block bindings)
          (match block
            ((op . rest)
             (if (= (length rest) 1)
                 (values (format "~A~A" op (car rest)) bindings)
                 (values (string-join
                          (map (compose swrite list) rest)
                          (format " ~A "(symbol->string op)))
                      bindings))))))
    ((* /)
     . ,(lambda (block bindings)
          (match block
            ((op . rest)
             (values (string-join
                      (map (compose swrite list) rest)
                      (format " ~A "(symbol->string op)))
                     bindings)))))
    (,binary-operators
     . ,(lambda (block bindings)
          (values (format "(~A ~A ~A)"
                          (swrite (list (second block)) (nobreak bindings))
                          (first block)
                          (swrite (list (third block)) (nobreak bindings)))
                  bindings)))
    ((IN |NOT IN|)
     . ,(lambda (block bindings)
	  (match block
	    ((op el `NIL)
             (let ((rw (swrite (list el) (nobreak bindings))))
               (values (format "(~A ~A NIL)" op rw)
                       bindings)))
	    ((op el ellist)
             (let ((rw-el (swrite (list el) (nobreak bindings)))
                   (rw-ellist (swrite ellist (sep ", " (nobreak bindings)))))
               (values
                (format "(~A ~A (~A))"  rw-el op rw-ellist) 
                bindings))))))
    ((VALUES)
     . ,(lambda (block bindings)
          (let ((F (lambda (els) (string-join (map ->string els)))))
            (match block
                   ((`VALUES vars . vals)
                    (values
                     (if (pair? vars)
                         (let ((pr (pre (inc bindings))))
                           (format "VALUES (~A) { ~A ~%~A}"
                                   (F vars)
                                   (string-join
                                    (map (lambda (vs)
                                           (format "~%~A(~A)" 
                                                   pr (F vs)))
                                         vals))
                                   (pre bindings)))
                         (format "VALUES ~A { ~A }"
                                 vars (F vals)))
                     bindings))))))
    ((LIMIT OFFSET |GROUP BY| |ORDER BY|)
     . ,(lambda (block bindings)
          (values (format "~A ~A" (car block) (swrite (cdr block) (nobreak (sep " " bindings))))
                  bindings)))
    (,list? 
     . ,(lambda (block bindings)
	  (values (format "{~A~%~A}" 
			 (swrite block (inc (linebreak bindings)))
			 (pre bindings))
		  bindings)))))

(define (write-sparql exp)
  (swrite (list exp) '() srules))

(define (write-triples triples)
  (string-join (map write-triple triples) "\n"))
