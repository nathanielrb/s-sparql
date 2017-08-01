;;(module s-sparql-transform *
;;(import chicken scheme extras data-structures srfi-1) 
;; (use matchable s-sparql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand triples
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

(define (expand-blank-node-path triple)
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
        (else #f)))

(define (expand-expanded-triple s p o)
  (cond  ((blank-node-path? o)
          (expand-blank-node-path (list s p o)))
         ((sequence-path? p)
          (expand-sequence-path-triple (list s p o)))
         (else
          (list (list s p o)))))

(define (expand-triple triple)
  (or (expand-blank-node-path triple)
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
;; Nested association lists
(define (nested-alist-ref* keys alist)
  (if (null? keys)
      alist
      (let ((nested (alist-ref (car keys) alist)))
	(and nested
	     (nested-alist-ref* (cdr keys) nested)))))

(define-syntax nested-alist-ref
  (syntax-rules ()
    ((_ key ... alist) 
     (nested-alist-ref* (list key ...) alist))))

(define (nested-alist-update* keys val alist)
  (let ((key (car keys)))
    (if (null? (cdr keys))
        (alist-update key val alist)
        (alist-update 
         key
         (nested-alist-update*
          (cdr keys) val (or (alist-ref key alist) '()))
         alist))))

(define-syntax nested-alist-update
  (syntax-rules ()
    ((_ key ... val alist) 
     (nested-alist-update* (list key ...) val alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context
;; Inverted trees
;; needs refactoring and extending...
(define-record context head next previous parent)

(define empty-context (make-context #f #f #f #f))

(define *context* (make-parameter empty-context))

(define (make-parent-context context)
  (if (not context) empty-context
      (make-context (and (context-head context) (car (context-head context)))
                    (context-previous context)
                    (context-next context)
                    (context-parent context))))

(define (parent-context context)
  (and (context? context)
       (let ((parent (context-parent context)))
         (and parent
              (make-context 
               (filter (lambda (x) (not (null? x)))
                       (list (context-head parent)
                             (context-previous context)
                             (context-head context)
                             (context-next context)))
               (context-previous parent)
               (context-next parent)
               (context-parent parent))))))

(define (previous-sibling context)
  (and (pair? (context-next context))
       (make-context (car (context-previous context))
                     (cons (context-head context)
                           (context-next context))
                     (cdr (context-previous context))
                     (context-parent context))))

(define (next-sibling context)
  (and (pair? (context-next context))
       (make-context (car (context-next context))
                     (cdr (context-next context))
                     (cons (context-head context)
                           (context-previous context))
                     (context-parent context))))

(define (context-children context #!optional filtr)
  (let ((parent (make-parent-context context)))
    (let loop ((count 0) (cc '()) (prevc '()) (nextc (cdr (context-head context))))
      (if (null? nextc) 
          (if (number? filtr) (car cc) (reverse cc)) ;; do with call/cc
          (let ((child (car nextc)))
            (if (or (not filtr) 
                    (and (procedure? filtr) (filtr child))
                    (and (number? filtr) (= count filtr)))
                (loop (+ count 1)
                      (cons (make-context child prevc (cdr nextc) parent) cc)
                      (cons child prevc)
                      (cdr nextc))
                (loop (+ count 1) cc prevc (cdr nextc))))))))

;; (define (parent-axis proc)
;;   (lambda (context)
;;     (call/cc
;;      (lambda (out)
;;        (let loop ((context context))
;;          (if (not context) (out #f)
;;              (let ((try (proc context)))
;;                (if try (out try)
;;                    (loop (parent-context context))))))))))

;;      (call/cc
;;       (lambda (out)

;; children/descendants and ancestors needs to be thought through better
(define (axis next #!optional recursive?)
  (lambda (proc)
    (lambda (context)
      (let loop ((context context))
        (cond ((pair? context) 
               (map loop context))
              ((context? context)
               (let ((try (proc context)))
                 (if try context
                     (loop (next context)))))
              (else #f))))))

(define (head? label)
  (lambda (context) 
    (let ((head (context-head context)))
      (and head (equal? (car head) label)))))

(define parent-axis (axis parent-context))

(define ancestors-axis (axis parent-context))

(define children-axis (axis context-children))

(define descendants-axis (axis context-children))

(define next-sibling-axis (axis next-sibling))

(define previous-sibling-axis (axis previous-sibling))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
(define *rules* (make-parameter '()))

(define default-rules (make-parameter (lambda () '())))

(define (nested-alist-replace* keys proc alist)
  (nested-alist-update* keys (proc (nested-alist-ref* keys alist)) alist))

(define (get-binding* vars key bindings)
  (nested-alist-ref* (append vars (list '@bindings key)) bindings))

(define (get-binding/default* vars key bindings default)
  (or (nested-alist-ref* (append vars (list '@bindings key)) bindings)) default)

(define-syntax get-binding
  (syntax-rules ()
    ((_ var ... key bindings)
     (get-binding* (list var ...) key bindings))))

(define-syntax get-binding/default
  (syntax-rules ()
    ((_ var ... key bindings default)
     (get-binding/default* (list var ...) key bindings default))))

(define (update-binding* vars key val bindings)
  (nested-alist-update* (append vars (list '@bindings key)) val bindings))

(define (update-bindings* vkvs bindings)
  (if (null? vkvs) bindings
      (match (car vkvs)
        ((vars key val)
         (update-binding* vars key val
                         (update-bindings* (cdr vkvs) bindings))))))

(define-syntax update-bindings
  (syntax-rules ()
    ((_ (var ... key val) ... bindings)
     (update-bindings* (list (list (list var ...) key val) ...) bindings))))

(define-syntax update-binding
  (syntax-rules ()
    ((_ var ... key val bindings)
     (update-binding* (list var ...) key val bindings))))

(define (project-bindings vars bindings)
  (let loop ((bindings bindings) (projected-bindings '()))
    (if (null? bindings) projected-bindings
        (let ((binding (car bindings)))
          (cond ((member (car binding) vars)
                 (loop (cdr bindings)
                       (append projected-bindings
                               (list (cons (car binding)
                                           (project-bindings vars (cdr binding)))))))
                ((equal? (car binding) '@bindings)
                 (loop (cdr bindings)
                       (append projected-bindings (list binding))))
                (else
                 (loop (cdr bindings) projected-bindings)))))))

(define (merge-bindings new-bindings bindings)
  (if (and (pair? new-bindings) (pair? bindings))
      (let loop ((new-bindings new-bindings) (merged-bindings bindings))
        (if (null? new-bindings) merged-bindings
            (let* ((new-binding (car new-bindings)))
              (loop (cdr new-bindings)
                    (alist-update (car new-binding)
                                  (merge-bindings (cdr new-binding)
                                                  (or (alist-ref (car new-binding) bindings) '()))
                                  merged-bindings)))))
      new-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformations
(define query-namespaces (make-parameter (*namespaces*)))

(define (remove-trailing-char sym #!optional (len 1))
  (let ((s (symbol->string sym)))
    (string->symbol
     (substring s 0 (- (string-length s) len)))))

(define (PrefixDecl? decl) (equal? (car decl) 'PREFIX))

(define (BaseDecl? decl) (equal? (car decl) 'BASE))

(define (graph? quad)
  (and (list? quad)
       (equal? (car quad) 'GRAPH)))

;; this isn't quite legal, but Virtuoso does it...
(define (all-prologues QueryUnit)
  (join
   (map (lambda (unit)
          (alist-ref '@Prologue unit))
        (alist-ref '@Unit QueryUnit))))

(define (query-prefixes QueryUnit)
  (map (lambda (decl)
         (list (remove-trailing-char (cadr decl)) (write-uri (caddr decl))))
       (filter PrefixDecl? (all-prologues QueryUnit))))

(define (rewrite-query Query #!optional (rules ((default-rules))))
  (parameterize ((query-namespaces (query-prefixes Query))
                 (*rules* rules))
    (rewrite Query '() rules)))

(define (rewrite* blocks bindings rules kappend knil)
  (let loop ((blocks blocks) (statements knil) (bindings bindings) (visited-blocks '()))
    (if (null? blocks)
	(values statements bindings)
	(let-values (((new-statements updated-bindings)
		      (apply-rules (car blocks) bindings rules
                                   (make-context
                                    (car blocks) visited-blocks
                                    (cdr blocks) 
                                    (make-parent-context (*context*))))))
	  (loop (cdr blocks)
		(kappend statements new-statements)
		updated-bindings
                (cons (car blocks) visited-blocks))))))

(define (rewrite blocks #!optional (bindings '()) (rules (*rules*)))
  (rewrite* blocks bindings rules append '()))

(define (apply-rules block bindings rules context)
  (let ((rule-match? (lambda (rule)
                       (or (and (symbol? rule) (equal? rule block))
                           (and (pair? rule) (member (car block) rule))
                           (and (procedure? rule) (rule block))))))
    (let loop ((remaining-rules rules))
      (if (null? remaining-rules) (abort (format #f "No matching rule for ~A" block))
          (match (car remaining-rules)
            ((rule . proc) 
             (if (rule-match? rule)
                 (parameterize ((*context* context)
                                (*rules* rules))
                   (proc block bindings))
                 (loop (cdr remaining-rules)))))))))

;; Macros for when the bindings don't matter, and should just be passed through.
;; Limited support for second-passes using the same rules and context,
;; but it'd be nice if this playing-nice with with-rewrite could be more general.
;; (rw/lambda (block)
;;  (with-rewrite ((rw (rewrite block)))
;;    body)) 
;; =>
;; (lambda (block bindings)
;;  (let-values (((rw new-bindings) (rewrite exp bindings)))
;;    (values body new-bindings))))
(define-syntax with-rewrite
  (syntax-rules ()
    ((_ ((var expr)) body)
     (let-values (((var updated-bindings) expr))
       (values body updated-bindings)))))

(define-syntax rw/lambda
  (syntax-rules (with-rewrite rewrite)
    ((_ (var) (with-rewrite ((rw (rewrite exp))) body))
     (lambda (var bindings)
       (let-values (((rw new-bindings) (rewrite exp bindings)))
         (values body new-bindings))))
    ((_ (var) body)
     (lambda (var bindings)
       (values body bindings)))))

(define (rw/continue block bindings)
  (with-rewrite ((new-statements (rewrite (cdr block) bindings)))
    `((,(car block) ,@new-statements))))

(define (rw/copy block bindings)
  (values (list block) bindings))

(define (rw/remove block bindings)
  (values (list) bindings))

;;)
