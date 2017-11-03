;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple test suite
;;
;; Requirements for tests:
;; - "." after the final triple in a Quads block
;; - all keywords in CAPS

(use s-sparql lexgen)

(define (test-round-trip in)
  (let ((out (write-sparql (parse-query in))))
    (or (equal? (string-translate in " \n")
                (string-translate out " \n"))
        (format #t "Failed: expected~%~A~% but given ~%~A~%" in out))))

(define (test-lex Part in out)
  (let ((out* (car (lex Part err in))))
    (or (equal? out out*)
        (format #t "Failed: expected~%~A~% but given ~%~A~%" out out*))))

(test-lex SelectClause "SELECT *" '(SELECT *))

(test-round-trip "SELECT * WHERE { ?s ?p ?o. }")

(test-round-trip "SELECT * WHERE { ?s ?p ?o. }")

(test-round-trip "DELETE WHERE { GRAPH ?g { ?s ?p ?o. } }")
