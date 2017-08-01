(use s-sparql s-sparql-parser lexgen)

(define (run-test expected given)
  (or (equal? expected given)
      (format #t "Failed: expected~%~A~% but given ~%~A~%" expected given)))

;; This fails for queries without "." after the final triple, e.g.,
;; SELECT * WHERE { ?s ?p ?o }
(define (test-round-trip in)
  (run-test (string-translate in " \n")
            (string-translate (write-sparql (read-sparql in)) " \n")))

(define (test-lex Part in out)
  (run-test out (car (lex Part err in))))


(test-lex SelectClause "SELECT *" '(SELECT *))

(test-round-trip "SELECT * WHERE { ?s ?p ?o. }")
