# s-sparql

Chicken Scheme module for reading, writing, and transforming SPARQL queries as s-expressions.

**Work in progress, subject to change**

## Installation

Clone this repo and run `sudo chicken-install` in the main directory. Requires Chicken Scheme 4.12+ (http://code.call-cc.org/). Then add to your scheme code:

```
(use s-sparql) 
(use s-sparql-parser)
```

## Main Concepts

**s-sparql** use s-expressions to represent SPARQL queries. This allows for easy parsing, transforming, and writing whole queries as well as simple triple groups.

A triple can be represented as a list of subject, predicate, and object: `'(?s ?p ?o)`, of subject, predicate, and objects: `'(?s ?p (?q ?r ?s))`, or of subject and predicates: ``(?s ((?p ?o) (?q ?u) (?r (?x ?y))))`. For example:

```
(define-namespace schema "http://schema.org/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")

(define triples
  `((app:entity1 ((a schema:Person)
                  (dc:title "Mr Director")))
    (app:entity2 <http://example.com/application/age> 45)
    (app:entity3 <http://example.com/application/likes> 
                 (<http://example.com/application/concepts/candy>
                  <http://example.com/application/concepts/apples>))))

(print (write-triples query))

;; => app:entity1 a schema.Person;
;;                dc:title "Mr Director".
;;    app:entity2  <http://example.com/application/age> 45.
;;    app:entity3 <http://example.com/application/likes> <http://example.com/application/concepts/candy>, <http://example.com/application/concepts/apples>
```

Using convenience functions (prefixed with `s-`) we can easily query a SPARQL endpoint: 

```
(*default-graph* `<http://example.com/application>)
(*sparql-endpoint* "http://127.0.0.1:8890/sparql")
(define-namespace dc "http://purl.org/dc/elements/1.1/") ; (defined by default)

(print (s-insert (write-triples triples)))

;; => WITH <http://example.com/application> 
;;    INSERT {
;;       app:entity1 a schema:Person;
;;                   dc:title "Mr Director".
;;       app:entity1 dc:title "Mr Director".
;;       app:entity2  <http://example.com/application/age> 45.
;;       app:entity3 <http://example.com/application/likes> <http://example.com/application/concepts/candy>, <http://example.com/application/concepts/apples>.  
;;     }

(sparql/update (s-insert (write-triples triples)))

;; => adds PREFIX declarations and queries the database...

```

**s-sparql** also includes a function for expanding triples, including blank nodes and property paths:

```
(expand-triples '((?s ((?p (?a ?b ?c)) (?q ?x)))))

;; => '((?s ?p ?a) (?s ?p ?b) (?s ?p ?c) (?s ?q ?x))

(expand-triples '(?a ?b (|@[]| ?x ?y)))

;; => '((?a ?b _:b3457) (_:b3457 ?x ?y))
```

**s-sparql-parser** allows parsing of triple blocks and full queries. Note that the parser always parses triples as nested lists, so `(read-triples "?s ?p ?o.")` returns `'(?s ((?p (?o))))` which is equivalent to `'(?s ?p ?o)`.

```
(read-triples "?s ex:friend ?a, ?b. ?a ex:friend ?b")

;; => '((?s ((ex:friend ?a ?b)))
;;      (?a ((ex:friend ?b))))

(read-query "
PREFIX schema: <http://schema.org/>
Prefix dc: <http://purl.org/dc/elements/1.1/>

SELECT *
FROM <http://example.com/data/>
WHERE {
  ?s ?p ?o, ?t;
     dc:title \"Mr Director\".
  ?s a schema:Person
}
")

;; => ((@Unit 
;;      (@Prologue
;;        (PREFIX |schema:| <http://schema.org/>)
;;        (PREFIX |dc:| <http://purl.org/dc/elements/>))
;;      (@Query
;;        (SELECT *)
;;        (@Dataset (FROM <http://example.com/data/>))
;;         (WHERE (?s ((?p (?o ?t))
;;                     (dc:title (?x))))
;;                (?s ((a (schema:Person))))))))
```

## s-sparql format

### Triples

### Path Expressions

### Arithmetic
## API

### Writing SPARQL 

### Parsing SPARQL

### Querying SPARQL Endpoints

### Transformations






