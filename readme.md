# s-sparql

**\*\*Work in progress, subject to change!\*\***

Chicken Scheme module for reading, writing, and transforming SPARQL queries as s-expressions.

## Installation

Clone this repo and run `sudo chicken-install` in the main directory. Requires Chicken Scheme 4.12+ (http://code.call-cc.org/). Then add to your scheme code:

```
(use s-sparql) 
(use s-sparql-parser)
```

## Main Concepts

**s-sparql** use s-expressions to represent SPARQL queries. This allows for easy parsing, transforming, and writing whole queries as well as simple triple groups.

A triple can be represented as a list of subject, predicate, and object: `'(?s ?p ?o)`, of subject, predicate, and objects: `'(?s ?p (?q ?r ?s))`, or of subject and predicates: `(?s ((?p ?o) (?q ?u) (?r (?x ?y))))`. For example:

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

**s-sparql-parser** allows parsing of triple blocks and full queries. 

**Note** that the parser always parses triples as nested lists, so `(read-triples "?s ?p ?o.")` returns `'(?s ((?p (?o))))` which is equivalent to `'(?s ?p ?o)`.

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

;; => '((@Unit 
;;       (@Prologue
;;         (PREFIX |schema:| <http://schema.org/>)
;;         (PREFIX |dc:| <http://purl.org/dc/elements/>))
;;       (@Query
;;         (SELECT *)
;;         (@Dataset (FROM <http://example.com/data/>))
;;          (WHERE (?s ((?p (?o ?t))
;;                      (dc:title (?x))))
;;                 (?s ((a (schema:Person))))))))
```

## s-sparql format

## API

### General

[procedure] (define-namespace prefix namespace)

Define namespaces for querying and expanding. The following namespaces are defined by default:

```
(define-namespace foaf "http://xmlns.com/foaf/0.1/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")
(define-namespace rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-namespace owl "http://www.w3.org/2002/07/owl#")
(define-namespace skos "http://www.w3.org/2004/02/skos/core#")
```

### Writing SPARQL 

[parameter] (\*expand-namespaces?\*)

Defaults to #t.

[procedure] (write-triples triples)

Returns the RDF expression of the given s-sparql expression, which should be a list of triples as defined above.

[procedure] (write-sparql query)

Returns the RDF expression of the given s-sparql expression, which should be a full or partial SPARQL query, but not a triples block.

[procedure] (new-sparql-variable)

[procedure] (new-blank-node #!optional prefix)

[procedure] (sparql-variable string)

[procedure] (expand-namespace nspair)

```
(expand-namespace 'dc:title) ;; => '<http://purl.org/dc/elements/1.1/title>
```

[procedure] (write-uri uri)

### Parsing SPARQL

[procedure] (read-triples <string>)
[procedure] (read-sparql <string>)

Return the s-sparql representation of the given string.

[procedure] (read-uri string)

### Convenience functions

Functions for writing queries without using the complete s-sparql format.

[parameter] (\*default-graph\*)

s-sparql representation of the default graph for queries, e.g., `(\*default-graph* '<http://example.org/application>)`. If this is set, a "WITH <GRAPH>" is added to `s-select`, `s-insert` and `s-delete` expressions.

### Querying SPARQL Endpoints

[parameter] (\*sparql-endpoint\*)

Defaults to "http://127.0.0.1:8890/sparql".

[parameter] (\*print-queries?\*) boolean

Defaults to #t.

[procedure] (sparql/select query #!optional raw? #!key additional-headers)

[procedure] (sparql/select-unique query #!optional raw? #!key additional-headers)

Returns a list of association lists representing the variable bindings, or in the case of unique, a single alist.

```
(sparql/select "SELECT * WHERE { ?s ?p ?o }")

;; => '(((s . "s1") (p . "p1"))
;;      ((s . "s2") (p . "p2")))

(sparql/select-unique "SELECT * WHERE { ?s ?p ?o }")

;; => '((s . "s1") (p . "p1"))

```

[procedure] (sparql/update query #!key additional-headers)

[procedure] (query-with-vars (vars ...) query form)

[procedure] (query-unique-with-vars (vars ...) query form)

```
(query-with-vars (s p)
  "SELECT * WHERE { ?s ?p ?o }"
  (list s p))

;; => '(("s1" "p1") ("s2" "p2"))

(query-unique-with-vars (s p)
  "SELECT * WHERE { ?s ?p ?o }"
  (list s p))

;; => '("s1" "p1")

```

### Transformations

[procedure] (expand-triples triples)

[procedure] (expand-triple triple)







