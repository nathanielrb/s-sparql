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

(define (expand-alternative-path-triple triple)
  (and (= (length triple) 3)
       (alternative-path? (cadr triple))
       (match triple
         ((s (`|\|| . ps) o)
          `((UNION
             ,@(map (lambda (p)
                      (expand-triple (list s p o)))
                    ps)))))))

(define (expand-negated-set-triple triple)
  (and (= (length triple) 3)
       (negated-set? (cadr triple))
       (match triple
         ((s (`! (`|@()| (`|\|| . ps))) o)
          (map (lambda (p)
                 (join (expand-triple `(,s (! ,p) ,o))))
               ps))
         ((s (`! (`^ p)) o)
          `((,o (! ,p) ,s)))
         (else (list triple)))))

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
  (let ((triple (list s p o)))
    (or (expand-blank-node-path triple)
        (expand-sequence-path-triple triple)
        (expand-alternative-path-triple triple)
        (expand-negated-set-triple triple)
        (list triple))))

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

(define (nested-alist-update alist val #!rest keys)
  (nested-alist-update* keys val alist))

(define-syntax nested-alist-update
  (syntax-rules ()
    ((_ key ... val alist) 
     (nested-alist-update* (list key ...) val alist))))

(define (nested-alist-delete* keys alist)
  (let ((key (car keys)))
    (if (null? (cdr keys))
        (alist-delete key alist)
        (let ((inner (alist-ref key alist)))
          (if (null? inner) #f
              (alist-update
               key
               (nested-alist-delete* (cdr keys) inner)
               alist))))))

(define-syntax nested-alist-delete
  (syntax-rules ()
    ((_ key ... alist)
     (nested-alist-delete* (list key ...) alist))))

(define (nested-alist-replace* keys proc alist)
  (nested-alist-update* keys (proc (nested-alist-ref* keys alist)) alist))

(define (nested-alist-replace alist proc #!rest keys)
  (nested-alist-replace* keys proc alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context
;; Inverted trees
;; needs refactoring and extending...
(define-record context head previous next parent)

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
                       (append (list (context-head parent))
                               (reverse (context-previous context))
                               (list (context-head context))
                               (context-next context)))
               (context-previous parent)
               (context-next parent)
               (context-parent parent))))))

(define (previous-sibling context)
  (and (pair? (context-next context))
       (make-context (car (context-previous context))
                     (cdr (context-previous context))
                     (cons (context-head context)
                           (context-next context))
                     (context-parent context))))

(define (next-sibling context)
  (and (pair? (context-next context))
       (make-context (car (context-next context))
                     (cons (context-head context)
                           (context-previous context))
                     (cdr (context-next context))
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

(define (get-binding* vars key bindings)
  (let ((inner (nested-alist-ref* (append vars (list '@bindings)) bindings)))
    (and inner 
         (let ((pair (assoc key inner)))
           (and pair (cdr pair))))))

(define (get-binding/default* vars key bindings default)
  (let ((inner (nested-alist-ref* (append vars (list '@bindings)) bindings)))
    (if inner
        (let ((pair (assoc key inner)))
          (if pair (cdr pair) default))
        default)))

(define-syntax get-binding
  (syntax-rules ()
    ((_ var ... key bindings)
     (get-binding* (list var ...) key bindings))))

(define-syntax get-binding/default
  (syntax-rules ()
    ((_ var ... key bindings default)
     (get-binding/default* (list var ...) key bindings default))))

(define (assoc-update key val alist)
  (cond ((null? alist) (list (cons key val)))
        ((equal? key (caar alist))
         (cons (cons key val) (cdr alist)))
        (else (cons (car alist) (assoc-update key val (cdr alist))))))

(define (update-binding* vars key val bindings)
  (nested-alist-update*
   (append vars (list '@bindings))
   (assoc-update key val
                 (or (nested-alist-ref* (append vars (list '@bindings)) bindings) '()))
   bindings))

(define (update-bindings* vkvs bindings)
  (if (null? vkvs) bindings
      (match (car vkvs)
        ((vars key val)
         (update-binding* vars key val
                         (update-bindings* (cdr vkvs) bindings))))))

(define-syntax update-bindings
  (syntax-rules ()
    ((_ ((var ... key val) ...) bindings)
     (update-bindings* (list (list (list var ...) key val) ...) bindings))))

(define-syntax update-binding
  (syntax-rules ()
    ((_ var ... key val bindings)
     (update-binding* (list var ...) key val bindings))))

(define (fold-binding* val vars key kons knil bindings)
  (update-binding* vars key
                   (kons val (get-binding/default* vars key bindings knil))
                   bindings))

(define-syntax fold-binding 
  (syntax-rules ()
    ((_ val vars ... key kons knil bindings)
     (fold-binding* val (list vars ...) key kons knil bindings))))

(define (cons-binding* val vars key bindings)
  (fold-binding* val vars key cons '() bindings))

(define-syntax cons-binding
  (syntax-rules ()
    ((_ val vars ... key bindings)
     (cons-binding* val (list vars ...) key bindings))))

(define (delete-binding* vars key bindings)
  (nested-alist-delete* (append vars (list '@bindings key)) bindings))

(define-syntax delete-binding
  (syntax-rules ()
    ((_ var ... key bindings) (delete-binding* (list var ...) key bindings))))

(define-syntax delete-bindings
  (syntax-rules ()
    ((_ ((var ... key) ...) bindings)
     (delete-bindings* (list (list (list var ...) key) ...) bindings))))

(define (delete-bindings* vks bindings)
  (if (null? vks) bindings
      (match (car vks)
        ((vars key)
         (delete-binding* vars key (delete-bindings* (cdr vks) bindings))))))

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
              (if (equal? (car new-binding) '@bindings)
                  (loop (cdr new-bindings)
                        (alist-update (car new-binding)
                                      (cdr new-binding)
                                      merged-bindings))
              (loop (cdr new-bindings)
                    (alist-update (car new-binding)
                                  (merge-bindings (cdr new-binding)
                                                  (or (alist-ref (car new-binding) bindings) '()))
                                  merged-bindings))))))
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

(define (all-prologues queries)
  (join
   (map (lambda (query)
          (or (alist-ref '@Prologue (cdr query)) '()))
        queries)))

(define (query-prefixes QueryUnit)
  (map (lambda (decl)
         (list (remove-trailing-char (cadr decl)) (write-uri (caddr decl))))
       (filter PrefixDecl? (all-prologues (cdr QueryUnit)))))

(define (rewrite-query QueryUnit rules)
  (parameterize ((query-namespaces (query-prefixes QueryUnit))
                 (*rules* rules))
    (let-values (((rw bindings)  (rewrite (list QueryUnit) '() rules)))
      (values (car rw) bindings))))

(define (rewrite* blocks bindings rules kappend knil)
  (let loop ((blocks blocks) (statements knil) (bindings bindings) (visited-blocks '()))
    (if (null? blocks)
	(values statements bindings)
	(let-values (((new-statements updated-bindings)
		      (apply-rules (car blocks) bindings rules
                                   ;; (make-context
                                   ;;  (car blocks) 
                                   ;;  visited-blocks
                                   ;;  (cdr blocks) 
                                   ;;  (make-parent-context (*context*))))))
                                   )))
	  (loop (cdr blocks)
		(kappend statements new-statements)
		updated-bindings
                (cons (car blocks) visited-blocks))))))

(define (rewrite blocks #!optional (bindings '()) (rules (*rules*)))
  (rewrite* blocks bindings rules append '()))

(define (apply-rules block bindings rules) ; context)
  (let ((rule-match? (lambda (rule)
                       (or (and (symbol? rule) (equal? rule block))
                           (and (pair? rule) (member (car block) rule))
                           (and (procedure? rule) (rule block))))))
    (let loop ((remaining-rules rules))
      (if (null? remaining-rules) (abort (format #f "No matching rule for ~A" block))
          (match (car remaining-rules)
            ((rule . proc) 
             (if (rule-match? rule)
                 (parameterize ((*rules* rules)) ; (*context* context)
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
