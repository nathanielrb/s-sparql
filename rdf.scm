;(module s-turtle
 ;   (*default-namespaces* read-triples write-triples)
  ;(import scheme chicken matchable srfi-13)

(use matchable)
(require-extension srfi-13)

  (define *default-namespaces*
    '((skos "http://www.w3.org/2004/02/skos/core#")
      (dct "http://purl.org/dc/terms/")
      (schema "http://schema.org/")))

  

;;  '(*top*
;;    (*namespaces* (foaf "http://foaf.org#")
;;                  (owl "http://owl.com/"))
;;    (*triples*
;;     (foaf:cake owl:likes foaf:icing)))

  (define (assoc-val key alist)
    (let ((v (assoc key alist)))
      (and v (cdr v))))

  (define (expand-namespace namespaces ent)
    (let* ((s (symbol->string ent))
           (n (string-index s #\:)))
      (string->symbol
       (if n
           (let ((namespace (assoc-val (string->symbol (substring s 0 n)) namespaces)))
             (string-append namespace (substring s (+ n 1))))
           s))))

(define (reify namespaces)
  (lambda (x)
    (cond ((keyword? x) (keyword->string x))
	;;((list? x) (apply conc x))
          ((symbol? x) (conc "<" (expand-namespace namespaces x) ">")); (symbol->string x) ">"))
          (else (conc "\"" x "\"")))))

(define format-triple
  (match-lambda 
    ((s p o)
     (format #f "~A ~A ~A .~%" s  p o))))

(define (namespace-prefixes namespaces)
  (apply conc
	 (map (lambda (ns)
		(format #f "PREFIX ~A: <~A>~%"
			(car ns) (cdr ns)))
	      namespaces)))


  (define (write-triples triples)
    (let ((namespaces (assoc-val '*namespaces* (cdr triples)))
          (triples (assoc-val '*triples* (cdr triples))))
      (apply conc
             ;; (append (namespace-prefixes namespaces))
             (map (match-lambda 
                    ((s p o) (format-triple (map (reify namespaces) (list s p o)))))
                  triples))))

  (define (read-triples string)
    '())

(define test-triples
  '(*top* (*namespaces* (a . "http://go.com/"))
          (*triples* (a:cake #:a a:icing)
                     (a:cake a:likes "You"))))
;  )
