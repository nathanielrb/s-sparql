(define (assoc-get field object)
  (cdr (assoc field object)))

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

(define (cons-when x p)
  (if x (cons x p) p))

(define (alist-ref-when x l)
  (or (alist-ref x l) '()))

(define (alist-merge-element x l)
  (let ((current (alist-ref-when (car x) l)))
    (if (null? current)
	(cons x l)
	(alist-update
	 (car x)
	 (cons (cdr x)
	       (if (pair? current)
		   current
		   (list current)))
	 l))))

(define (fold-alist alst)
  (fold alist-merge-element '() alst))
