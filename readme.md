# s-sparql

Chicken Scheme module for reading, writing, and transforming SPARQL queries as s-expressions.

**Work in progress**

## Installation

Clone this repo and run `sudo chicken-install` in the main directory. Requires Chicken Scheme 4.9+ (http://code.call-cc.org/).

## Main Concepts

**s-sparql** use s-expressions to represent SPARQL queries. This allows for easy parsing, transforming, and writing whole queries as well as simple triple expressions.

To start with a simple example of writing triples:

```
(define-namespace schema "http://schema.org/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")

(*default-graph* `<http://example.com/application>)

(define triples
  `((app:entity1 ((a schema:Person)
                  (dc:title "Mr Director")))
    (app:entity2 <http://example.com/application/age> 45)
    (app:entity3 <http://example.com/application/likes> (<http://example.com/application/concepts/candy> <http://example.com/application/concepts/apples>))))

(print (s-triples query))

;; => app:entity1 a schema.Person;
;;                dc.title "Mr Director".
;;    app:entity2  <http://example.com/application/age> 45.
;;    app:entity3 <http://example.com/application/likes> <http://example.com/application/concepts/candy>, <http://example.com/application/concepts/apples>
```

and querying SPARQL endpoints: 

```
(print (s-insert (s-triples triples)))

;; => WITH <http://example.com/application> 
;;    INSERT {
;;       app:entity1 a schema:Person;
;;                   dc.title "Mr Director".
;;       app:entity1 dc:title "Mr Director".
;;       app:entity2  <http://example.com/application/age> 45.
;;       app:entity3 <http://example.com/application/likes> <http://example.com/application/concepts/candy>, <http://example.com/application/concepts/apples>.  
;;     }

(*sparql-endpoint* "http://127.0.0.1:8890/sparql")

(sparql/update (s-insert (s-triples triples)))

;; => adds PREFIX declarations and queries the database...

```

Example of a whole query:

```
(parse-query "
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

## API

### Writing SPARQL 

### Parsing SPARQL

### Querying SPARQL Endpoints

### Transformations






