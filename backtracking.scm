(use sparql-query
     srfi-13 srfi-69 http-client intarweb uri-common medea cjson matchable irregex)

(require-extension utf8 utf8-srfi-14
                   typeclass input-classes abnf abnf-charlist abnf-consumers
                   lexgen)

(define char-list-<Input>
  (make-<Input> null? car cdr))

(define char-list-<Token>
  (Input->Token char-list-<Input>))

(define char-list-<CharLex>
  (Token->CharLex char-list-<Token>))

(define char-list-<CoreABNF>
  (CharLex->CoreABNF char-list-<CharLex>))

(import-instance (<Token> char-list-<Token> char-list/)
		 (<CharLex> char-list-<CharLex> char-list/)
                 (<CoreABNF> char-list-<CoreABNF> char-list/)
                 )


(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))



(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(define lit char-list/lit)

;; This matches a sequence of patterns. 


;; ;; This matches either one of two patterns. It's analogous to patterns
;; ;; separated by the '|' in regular expressions.

;; ;; Kleene closure. Analogous to '*' 

(define (bstar p)
  (letrec ((rec
	    (vac
	     (bar
	      (seq p rec)
	      pass))))
    rec))


(define (bstar p)
  (letrec ((rec
	    (vac
	     (bar
	      (opt (seq p rec))
	      pass))))
    rec))

(define (bseq p1 p2)
  (lambda (sk fk strm)
    (p1 (lambda (strm1) (p2 sk fk strm1)) fk strm)))

(define (lstar p1 p2)
  (letrec ((rec
            (vac
             (alternatives
              (seq p1 rec)
              p2))))
    rec))

(define (lstar p1 p2)
  (vac
   (alternatives
    (seq p1 (lstar p1 p2))
    p2)))




(use amb amb-extras)

(define (bstar p)
  (lambda (sk fk strm)
    (print "trying " ((seq p (bstar p)) sk fk strm))
    (let ((s
	   (amb ((seq p (bstar p)) sk fk strm)
		(p sk fk strm)
		(sk strm))))
      (required (not (equal? s '(error))))
      s)))

(define (er s) '(error))

(define (bstar p)
  (lambda (sk fk strm)
    (let ((s
	   (amb (p (lambda (strm1)
		     ((bstar p) sk fk strm1)) fk strm)
		(pass sk fk strm))))
      (required (not (equal? s '(error))))
      s)))

(define (bstar p)
  (lambda (sk fk strm)
    (or
     (call/cc
      (lambda (k)
	(let ((try (lambda (K strm)
		     (lambda (s)
;;		       (print "trying " s " => " (K s))
		       (let ((ss (K s)))
			 (if (equal? ss '(error)) 
			     (fk strm) ;;(k #f) 
			     ss))))))
	  (p (lambda (strm1)
	       (if (null? (cadr strm1)) (k #f)
		   ((bstar p) 
		    sk ;(try sk strm1)
		    (try sk strm) strm1)))
	     (try sk strm)
	     strm))))
     (sk strm))))

(define (bstar p)
  (lambda (sk fk strm)
    (call/cc
     (lambda (k)
       (p (lambda (strm1)
	    (let ((try 		
		   (lambda (s)
		     ;;(print "trying " s "=>" (sk s) "\nat level " strm "\nand " strm1 "\n")
		     ;;(print "trying " s  "\nat level " strm "\nand " strm1 "\n")
		     (print "trying " s)
		     (let ((ss (sk s)))
		      (when (equal? ss '(error)) 
			(print "*** error on " ss " with " strm1))
		       (if (equal? ss '(error)) 
			   (k strm)
			   s)))))
	    (if (null? (cadr strm1)) (sk strm1)
		((bstar p)
		 try
		 try
		 strm1))))
	  sk strm)))))


;; (define (star p)
;;   (lambda (sk fk strm)
;;         (p (lambda (strm1) 
;;    	    (if (eoi? (cadr strm1)) (sk strm1)
;; 	     ((star p) sk sk strm1))) sk strm)))

(define (bstar p)
  (lambda (sk fk strm)
    (p (lambda (strm1)
	 (or 
	  (if (null? (cadr strm1))
	      (begin (print "null")
		     (sk strm))
	      ((bstar p)
	       (lambda (s)
		 (let ((ss (sk s)))		      
		   (if (equal? ss '(error))
		       #f
		       ss)))
	       ;;sk
	       (lambda (s)
		 (print "error")
		 (let ((ss (sk strm)))		      
		   (if (equal? ss '(error))
		       #f
		       ss)))
	       strm1))
	  (begin (print "backtracking on " strm)
		 (sk strm))) )
       (lambda (s)
	 (print "failed here, s = " s ", strm = " strm) ;; ** 
	 (let ((ss (sk s)))
	   (print "(sk s) =>" ss)
	   (print "(sk strm) => " (sk strm))
	   (print "(fk s) => "(fk s))
	   (print "(fk strm) => "(fk strm))
	   (if (equal? ss '(error))
	       (sk strm)
	       ss)))
       strm)  ))

(define-syntax test
  (syntax-rules ()
    ((_  answer exp)
     (begin
     (print (quote exp))
     (print "Expected: " answer)
     (print "Actual:   " exp)
     (newline)))))

(test '((a a a a a) ())
      (lex (seq (bstar (lit "a")) (lit "a")) er "aaaaa"))

(test '((a a a a a b) ())
      (lex (seq (bstar (lit "a")) (lit "b")) er "aaaaab"))

(test '((a a a a a) (b))
      (lex (seq (bstar (lit "a")) (lit "a")) er "aaaaab"))

(test '(error)
      (lex (seq (bstar (lit "a")) (lit "b")) er "aaaaa"))

(test '((a a a a a) ())
      (lex (opt (seq (bstar (lit "a")) (lit "a"))) er "aaaaa"))

(test '((a a a a a) (b))
      (lex (opt (seq (bstar (lit "a")) (lit "a"))) er "aaaaab"))

(test '(() (a a a a a b))
      (lex (opt (seq (bstar (lit "a")) (lit "b"))) er "aaaaaa"))

(test '((a a a a a b) ())
      (lex (opt (seq (bstar (lit "a")) (lit "b"))) er "aaaaab"))
