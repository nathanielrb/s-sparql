;; Usage:
;;
;; (define-rest-page  (($path "/admin/article/:type/:id"))
;;  (lambda ()
;;   `(p ($path 'type)))
;;     ...)

(use awful medea irregex srfi-13)

(define (rest-indexes parts)
  (let rec ((n 0)
	    (parts parts))
    (if (null? parts)
	'()
	(let ((part (car parts)))
	  (if (string-prefix? ":" part)
	      (cons
	       (cons (string->symbol (substring part 1))
		     n)
	       (rec (+ 1 n) (cdr parts)))
	      (rec (+ 1 n) (cdr parts)))))))
	
(define (parse-args args)
  (if (null? args)
      '()
      (cons (cons (caar args) (cdar args))
	    (parse-args (cdr args)))))

(define (parse-flat-args args)
  (if (null? args)
      '()
      (cons (cons (car args) (cadr args))
	    (parse-flat-args (cddr args)))))

;; Even better:
;; (define-rest-page ("/home" jim james)
;; and concat... ??

(define-syntax define-rest-call
  (syntax-rules ()
    ((_ ((vars ...) restexpr) body . args)
     (let* ((pathexpr (irregex-replace/all "[:][^/]+" restexpr "[^/]+"))
	    (rest-parts (irregex-split "/" restexpr))
	    (indexes (rest-indexes rest-parts)))
       (define-page (irregex pathexpr)
	 (lambda (path)
	   (let ((pathfun
		  (lambda (param #!optional default)
		    (let ((index (alist-ref param indexes)))
		      (if index
			  (list-ref (irregex-split "/" path) index)
			  default)))))
	     (let ((vars (pathfun (quote vars))) ...)
	       (awful-response-headers '((content-type "application/json")))
	       (json->string (body)))))
         no-template: #t
	 . args)))))

(define-syntax define-rest-page-fn
  (syntax-rules ()
    ((_ ((pathfun restexpr)) body . args)
     (let* ((pathexpr (irregex-replace/all "[:][^/]+" restexpr "[^/]+"))
	    (rest-parts (irregex-split "/" restexpr))
	    (indexes (rest-indexes rest-parts)))
       (define-page (irregex pathexpr)
	 (lambda (path)
	   (let ((pathfun
		  (lambda (param #!optional default)
		    (let ((index (alist-ref param indexes)))
		      (if index
			  (list-ref (irregex-split "/" path) index)
			  default)))))
             (awful-response-headers '((content-type "application/json")))
	     (json->string (body))))
         no-template: #t
	 . args)))))
