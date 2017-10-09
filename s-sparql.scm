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

;; (define-syntax splice-when
;;   (syntax-rules ()
;;     ((splice-when body ...)
;;      (let ((body-val (and body ...)))
;;        (splice-when body-val body-val)))
;;     ((splice-when test ... body)
;;      (if (not (and test)) '() body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend definitions from sparql-query
(*query-unpacker* typed-sparql-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data types
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
           (equal? "_:" (substring (->string obj) 0 2)))))

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



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Writing Sparql
;; (define (write-uri uri)
;;   (let ((str (symbol->string uri)))
;;     (substring str 1 (- (string-length str) 1))))

;; ;; leftover?
;; (define (write-expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
;;   (if (s-iri? ns-pair)
;;       (write-uri ns-pair)
;;       (expand-namespace* ns-pair namespaces)))

;; (define (expand-namespace* ns-pair namespaces)
;;   (let ((pair (string-split (->string ns-pair) ":")))
;;     (format #f "~A~A"
;;             (lookup-namespace (string->symbol (car pair)) namespaces)
;;             (cadr pair))))

;; (define (expand-namespace ns-pair #!optional (namespaces (*namespaces*)))
;;   (if (or (sparql-variable? ns-pair)  (s-iri? ns-pair))
;;       ns-pair
;;       (string->symbol
;;        (format #f "<~A>" (expand-namespace* ns-pair namespaces)))))               

;; (define (rdf->json exp)
;;   (cond ((symbol? exp) (write-uri exp))
;; 	(else exp)))

;; (define (value exp)
;;   (cond ((symbol? exp)
;;          (let ((s (symbol->string exp)))
;;            (if (string-contains s "://")
;;                (substring s 1 (- (string-length s) 1))
;;                s)))
;;         ((typed-or-langtag-literal? exp) (car exp))
;;         (else exp)))

;; (define (write-sparql-typed-literal exp)
;;   (and (typed-or-langtag-literal? exp) 
;;        (if (langtag? (cdr exp))
;;            (format #f "\"~A\"~A" (car exp) (cdr exp))
;;            (format #f "\"~A\"^^~A" (car exp) (cdr exp)))))

;; ;; replace write-sparql with write-sparql-element
;; (define (write-sparql-inverse-element exp)
;;   (and (inverse-element? exp)
;;        (format #f "~A~A" (car exp) (write-sparql (cadr exp)))))

;; (define (write-sparql-element-path exp)
;;   (and (element-path? exp)
;;        (format #f "~A/~A" 
;;                (write-sparql (cadr exp))
;;                (write-sparql (caddr exp)))))

;; (define (write-sparql-modified-path exp)
;;   (and (modified-path? exp)
;;        (format #f "~A~A" (write-sparql (cadr exp)) (car exp))))

;; (define (write-sparql-negated-set exp)
;;   (and (negated-set? exp)
;;        (format #f "!~A" (write-sparql (cadr exp)))))

;; (define (write-sparql-alternative-path exp)
;;   (and (alternative-path? exp)
;;        (string-join
;;         (map write-sparql (cdr exp))
;;         "|")))

;; (define (write-sparql-path exp)
;;   (or
;;    (write-sparql-element-path exp)
;;    (write-sparql-modified-path exp)
;;    (write-sparql-inverse-element exp)
;;    (write-sparql-negated-set exp)
;;    (write-sparql-alternative-path exp)))

;; (define (write-sparql-element exp)
;;   (cond ((string? exp) (conc "\"" exp "\""))
;;         ((keyword? exp) (keyword->string exp))
;;         ((number? exp) (number->string exp))
;;         ((symbol? exp) (symbol->string exp))
;;         ((boolean? exp) (if exp "true" "false"))
;;         ((pair? exp) (or (write-sparql-typed-literal exp)
;;                          (write-sparql-path exp)))))

;; (define (write-sparql exp #!optional (level 0))
;;   ;; (cond ((string? exp) (conc "\"" exp "\""))
;;   ;;       ((keyword? exp) (keyword->string exp))
;;   ;;       ((number? exp) (number->string exp))
;;   ;;       ((symbol? exp) (symbol->string exp))
;;   ;;       ((boolean? exp) (if exp "true" "false"))
;;   ;;       ((pair? exp)
;;          (or (write-sparql-element exp)
;;              (write-sparql-special exp)
;;              (string-join (map (cut write-sparql <> (+ level 1))
;;                                exp)
;;                           " ")))


;; (define functions '(COUNT SUM MIN MAX AVG SAMPLE STR LANG LANGMATCHES 
;;                           DATATYPE BOUND IRI URI BNODE RAND NIL ABS CEIL FLOOR ROUND IF
;;                           CONCAT STRLEN UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS
;;                           STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS 
;;                           TIMEZONE TZ NOW NIL UUID NIL STRUUID NIL MD5 SHA1 SHA256 SHA384
;;                           SHA512 COALESCE  STRLANG STRDT isIRI isURI isBLANK isLITERAL isNUMERIC
;;                           REGEX SUBSTR REPLACE EXISTS))

;; (define (write-sparql-function exp)
;;   (and (member (car exp) functions)
;;        (format #f "~A(~A)"
;;                (car exp)
;;                (string-join 
;;                 (map write-sparql (cdr exp))
;;                 ", "))))               

;; (define binary-operators '(+ - * / = != <= >= < >))

;; (define (write-sparql-binary exp)
;;   (and (member (car exp) binary-operators)
;;        (format #f "~A ~A ~A"
;;                (cadr exp)
;;                (car exp)
;;                (caddr exp))))          

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
  
;; (define (write-triple triple #!optional (level 0))
;;   (if (null? triple)
;;       "{}"
;;       (or (write-sparql-special triple level)
;;           (let ((pre (apply conc (make-list level " "))))
;;              ;; list of triples
;;             (if (pair? (car triple))
;;                 (format #f "{~%~A~%~A}"
;;                         (string-join 
;;                          (map (cut write-triple <> (+ level 1)) triple) "\n")
;;                         pre)
;;                 (conc
;;                  pre
;;                  (string-join
;;                   (match triple
;;                     ((subject properties) 
;;                      (list (write-sparql subject)
;;                            (write-triple-properties properties)))
;;                     ((subject predicate objects)
;;                      (list (write-sparql subject)
;;                            (write-triple-properties predicate)
;;                            (write-triple-objects objects)))))
;;                  "."))))))

;; (define (write-triple-properties exp)
;;   (let ((W (lambda (property)
;;              (format #f "~A ~A"
;;                      (write-triple-properties (car property))
;;                      (write-triple-objects (cadr property))))))
;;   (or (write-sparql-path exp)                         
;;       (if (pair? exp)
;;           (string-join (map W exp) ";  ")
;;           (write-sparql exp)))))

;; (define (write-triple-objects exp)
;;   (if (pair? exp)
;;       (or (write-sparql-typed-literal exp)
;; 	  (write-sparql-special exp)
;;           (string-join (map (lambda (y) (write-sparql y)) exp) ", "))
;;       (write-sparql exp)))

;; (define (write-triples triples)
;;   (string-join (map s-triple triples) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand RDF triples
;; (define (expand-special triple)
;;   (cond ((blank-node-path? (car triple))
;;          (let ((subject (new-blank-node)))
;;            (append (expand-triple (cons subject (cdr triple)))
;;                    (expand-triple (cons subject (cdar triple))))))
;;         ((and (= (length triple) 3)
;;               (blank-node-path? (caddr triple)))
;;          (let ((object (new-blank-node)))
;;            (match triple
;;              ((s p (_ . rest))
;;                (append (expand-triple (list s p object))
;;                        (expand-triple (cons object rest)))))))        
;;         (else
;;          (case (car triple)
;;            ((SELECT |SELECT DISTINCT| |SELECT REDUCED|)
;;             triple)
;;            ((WHERE DELETE INSERT) 
;;             (cons (car triple) (expand-triples (cdr triple))))
;;            ((@Blank)
;;             (expand-triple (cons (new-blank-node) (cdr triple))))
;;            ((|@()| MINUS OPTIONAL)
;;             (list (cons (car triple) (expand-triples (cdr triple)))))
;;            ((UNION)
;;             (list (cons (car triple) (map expand-triples (cdr triple)))))
;;            ((GRAPH) (list (append (take triple 2)
;;                                   (expand-triples (cddr triple)))))
;;            ((FILTER BIND) (list triple))
;;            (else #f)))))

;; (define (expand-subselect triple)
;;   (and (pair? triple) (pair? (car triple))
;;        (member (caar triple) '(SELECT |SELECT DISTINCT| |SELECT REDUCED|))
;;        (list (car triple)
;;              (expand-triples (cdr triple)))))

;; (define (expand-sequence-path-triple triple)
;;   (and (= (length triple) 3)
;;        (sequence-path? (cadr triple))
;;        (match triple
;;          ((s (`/ . ps) o)
;;           (let loop ((s s) (ps ps))
;;             (if (= (length ps) 1)
;;                 (expand-triple (list s (car ps) o))
;;                 (let ((object (new-blank-node)))
;;                   (append (expand-triple (list s (car ps) object))
;;                           (loop object (cdr ps))))))))))

;; (define (expand-triples triples)
;;   (or (expand-subselect triples)
;;       (expand-special triples)
;;       (join (map expand-triple triples))))

;; (define (expand-expanded-triple s p o)
;;   (cond  ((blank-node-path? o)
;;           (expand-special (list s p o)))
;;          ((sequence-path? p)
;;           (expand-sequence-path-triple (list s p o)))
;;          (else
;;           (list (list s p o)))))

;; (define (expand-triple triple)
;;   (or ;(expand-subselect triple)
;;    (let ((x (expand-subselect triple))) (and x (list x)))
;;       (expand-special triple)
;;       (match triple
;;         ((subject predicates)
;;          (let ((subject (car triple)))
;;            (join
;;             (map (lambda (po-list)
;;                    (let ((predicate (car po-list))
;;                          (object (cadr po-list)))
;;                      (if (and (list? object) (not (blank-node-path? object)))
;;                          (join
;;                           (map (lambda (object)
;;                                  (expand-expanded-triple subject predicate object))
;;                                (cadr po-list)))
;;                          (expand-expanded-triple subject predicate object))))
;;                  predicates))))
;;         ((subject predicate objects)
;;          (if (list? objects)
;;              (join
;;               (map (lambda (object)
;;                      (expand-expanded-triple subject predicate object))
;;                    objects))
;;              (expand-expanded-triple subject predicate objects))))))

(include "s-sparql-transform.scm")
(include "s-sparql-writer.scm")
(include "s-sparql-parser.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience Functions
;; To be deprecated soon
(define (s-triple triple)
  (if (string? triple)
      triple
      (write-triple triple)))

(define s-triples write-triples)

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


)
