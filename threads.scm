(use srfi-1 srfi-18 pool http-client)

;(define pool (make-pool (make-list 10)))

(define (pvec #!rest thunks)
  (let* ((len (length thunks))
         (result (make-vector len #f))
         (remaining len)
         (result-mutex (make-mutex))
         (done-mutex (make-mutex)))
    (letrec ((children
              (map (lambda (thunk k)
                     (make-thread
                      (lambda ()
                        (let ((x (thunk)))
                          (mutex-lock! result-mutex #f #f)
                          (vector-set! result k x)
                          (set! remaining (- remaining 1))
                          (mutex-unlock! result-mutex)
                          (when (= remaining 0)
                            (mutex-unlock! done-mutex))))))
                   thunks (list-tabulate len values))))
      (mutex-lock! done-mutex #f #f)
      (map thread-start! children)
      (mutex-lock! done-mutex #f #f)
      result)))

(define (take-max lst n)
  (if (or (null? lst) (= n 0))
      '()
      (cons (car lst) (take-max (cdr lst) (- n 1)))))

(define (drop-max lst n)
  (if (or (null? lst) (= n 0))
      lst
      (drop-max (cdr lst) (- n 1))))

;; Works even with big numbers

(define (pvec-batch batch-size thunks)
  (let* ((len (length thunks))
         (result (make-vector len #f))
         (remaining len)
         (result-mutex (make-mutex))         
         (done-mutex (make-mutex)))
    (let loop ((thunks thunks)
               (batch 0))
      (if (null? thunks)
          result
          (letrec ((batch-remaining (min batch-size (length thunks)))
                   (batch-done-mutex (make-mutex))
                   (children
                    (map (lambda (thunk k)
                           (make-thread
                            (lambda ()
                              (let ((x (thunk)))
                                (mutex-lock! result-mutex #f #f)
                                (vector-set! result k x)
                                (set! remaining (- remaining 1))
                                (set! batch-remaining (- batch-remaining 1))
                                (mutex-unlock! result-mutex)
                                (when (= batch-remaining 0)
                                  (mutex-unlock! batch-done-mutex)) ))))
                         (take-max thunks batch-size)
                         (list-tabulate len
                                        (lambda (x) (+ x (* batch batch-size)))))))
            (mutex-lock! batch-done-mutex #f #f)
            (map thread-start! children)
            (mutex-lock! batch-done-mutex #f #F)
            (loop (drop-max thunks batch-size) (+ batch 1)))))))

(define pool (make-pool '(1 2 3 4)))

;; gives abandoned-mutex-exception
;; (define v (pvec-pool 100 (make-list 500 t)))
;; needs some more thought
(define (pvec-pool pool-size thunks)
  (let* ((len (length thunks))
         (result (make-vector len #f))
         (remaining len)
         (count pool-size)
         (pool-mutex (make-mutex))
         (result-mutex (make-mutex))
         (done-mutex (make-mutex)))
    (letrec ((children
              (map (lambda (thunk k)
                     (make-thread
                      (lambda ()
                        (let ((waiting (if (= count 0)
                                           (begin (mutex-lock! pool-mutex #f #f)
                                                  #t)
                                           (begin (set! count (- count 1))
                                                  #f)))
                              (x (thunk)))
                          (mutex-lock! result-mutex #f #f)
                          (vector-set! result k x)
                          (set! remaining (- remaining 1))
                          (mutex-unlock! result-mutex)
                          (if waiting
                            (set! count (+ count 1))
                            (mutex-unlock! pool-mutex))
                          (when (= remaining 0)
                            (mutex-unlock! done-mutex))))))
                   thunks (list-tabulate len values))))
      (mutex-lock! done-mutex #f #f)
      (map thread-start! children)
      (mutex-lock! done-mutex #f #f)
      result)))

(define (pmap fn #!rest lists)
  (vector->list
   (apply pvec
    (apply map
           (lambda (e) (lambda () (fn e)))
           lists))))

(define (pmap-batch batch-size fn #!rest lists)
  (vector->list
   (pvec-batch batch-size
     (apply map
            (lambda (e) (lambda () (fn e)))
            lists))))

(define (pmap-pool pool-size fn #!rest lists)
  (vector->list
   (pvec-pool pool-size
     (apply map
            (lambda (e) (lambda () (fn e)))
            lists))))

(define (pmap-vec fn #!rest lists)
   (apply pvec
    (apply map
           (lambda (e) (lambda () (fn e)))
           lists)))

(define pool (make-pool '(1 2 3 4 5)))

(define t (lambda () 
            (query-with-vars (x y)
                             "SELECT * WHERE { <http://data.europa.eu/eurostat/id/taxonomy/ECOICOP/concept/041220> ?p ?o } LIMIT 1"
                             (list x y))))
