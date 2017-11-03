# s-sparql

An experimental Chicken Scheme module for parsing and transforming SPARQL 1.1 queries as s-expressions.

## Installation

Requires Chicken Scheme 4.12 (http://code.call-cc.org/). Clone this repo and run `sudo chicken-install` in the main directory.  Then add to your Scheme code:

```
(use s-sparql) 
```

## Parsing and Writing

The `s-sparql` parser implements the complete SPARQL 1.1 EBNF grammar, with the optional addition of annotations (see below).

The procedure `parse-query` parses a string into s-expression, and the procedure `write-sparql` writes the s-expression back to a string.

Note that the parser always parses triples as nested lists, so `?s ?p ?o` is parsed as `'(?s ((?p (?o))))` which is equivalent to `'(?s ?p ?o)`.

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

;; => '(@QueryUnit
;;      (@Query
;;       (@Prologue
;;        (PREFIX |schema:| <http://schema.org/>)
;;        (PREFIX |dc:| <http://purl.org/dc/elements/1.1/>))
;;       (SELECT *) 
;;       (@Dataset (FROM <http://example.com/data/>))
;;       (WHERE
;;        (?s ((?p (?o ?t))
;;             (dc:title "Mr Director"))) 
;;        (?s ((a schema:Person))))))

(write-sparql
 '(@QueryUnit
   (@Query
    (@Prologue
     (PREFIX |schema:| <http://schema.org/>)
     (PREFIX |dc:| <http://purl.org/dc/elements/1.1/>))
    (SELECT *) 
    (@Dataset (FROM <http://example.com/data/>))
    (WHERE
     (?s ((?p (?o ?t))
          (dc:title "Mr Director"))) 
     (?s a schema:Person)))))

;; => "PREFIX schema: <http://schema.org/>
;;     PREFIX dc: <http://purl.org/dc/elements/1.1/>
;;     
;;     SELECT *
;;    
;;     FROM <http://example.com/data/>
;;     WHERE {
;;      ?s ?p ?o, ?t;  dc:title "Mr Director".
;;      ?s a schema:Person.
;;     }"
```
### Annotations 

The SPARQL 1.1 grammar is optionally enriched with annotations of the type `@access Label` and `@access Label(?var)`, which are parsed as Quad blocks (GraphPatternNotTriples). By default these are not written by `write-sparql`, unless the parameter `*write-annotations?*` is set to `#t`.

## Transforming SPARQL

The procedure `rewrite-query` and low-level `rewrite` allows for the rule-based transformation of s-sparql expressions, in the spirit of Oleg Kiselyov's `sxml`. While fully operational, this functionality would benefit from a better separation of concerns, for instance using monads, and is not fully documented here. 

Utility rules are provided: `rw/copy`, `rw/remove`, `rw/quads`, `rw/graph`, `rw/union`, and `rw/subselect`.

Briefly, every rule must return two values, the return value (which is combined by a combiner that defaults to `append`) and the updated bindings. These bindings are passed through the entire tree in depth-first order. A `*context*` object representing the inverted tree is also provided.

Here is a very simplistic example. For the sake of clarity:

- triples are assumed to be already expanded using `expand-triple`
- the bindings are updated every time, whether or not a new variable has been renamed; in a real application, this could be done in one go with `let-values` and `fold-binding`.

```
(define (rename v bindings)
  (if (sparql-variable? v)
      (or (get-binding v 'renaming bindings)
          (new-sparql-variable "var"))
      v))

(define rename-variables-rules
 `( ((@QueryUnit @UpdateUnit @Query @Update CONSTRUCT WHERE DELETE INSERT |DELETE WHERE| |INSERT DATA|) . ,rw/quads)
    ((@Prologue @Dataset @Using) . ,rw/copy)
    ((FILTER BIND MINUS OPTIONAL UNION VALUES @SubSelect |GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,select? . ,rw/copy)
    ((UNION) . ,rw/union)
    ((GRAPH)
     . ,(lambda (block bindings)
          (match block
            ((`GRAPH graph . rest)
              (let-values (((rw new-bindings) (rewrite rest bindings)))
                (let ((new-graph (rename graph bindings)))
	          (values `((GRAPH ,new-graph ,@rw))
                          (update-binding graph new-graph new-bindings))))))))
    (,triple? 
      . ,(lambda (triple bindings)
           (match triple
             ((s p o)
               (let ((s* (rename s bindings))
                     (p* (rename p bindings))
                     (o* (rename o bindings)))
                (values `((,s* ,p* ,o*))
                        (update-bindings ((s 'renaming s*)
                                          (p 'renaming p*)
                                          (o 'renaming o*))
                                         bindings)))))))
    (,list? . ,rw/list)
    (,symbol? . ,rw/copy)))

(define q
  '(@QueryUnit
    (@Query
     (SELECT *) 
     (WHERE
      (GRAPH ?g
       (?s a <fish>)
       (?s <eats> ?food))))))

(rewrite-query q rename-variables-rules)

;; => '(@QueryUnit (@Query (SELECT *) (WHERE (GRAPH ?var19527 (?var19525 a <fish>) (?var19525 <eats> ?var19526)))))
;;    '((@bindings (?g . ?var19527)))
;;    ; 2 values
```

For a more complex example, see "s-sparql-writer.scm", which defines a customized version of `rewrite`.

## Querying SPARQL Endpoints

The module `sparql-query` defines basic procedures for querying SPARQL endpoints and parsing the results. It is documented in the [mu-chicken-template](https://github.com/nathanielrb/mu-chicken-template) readme.

`s-sparql` provides `sparql-query` and defines the default `*query-unpacker*` as `typed-sparql-bindings`, which parses URIs as symbols, typed values as cons-pairs, and numbers as numbers:

```
(sparql-select query)

;; => '(((var1 . "str-val") 
;;       (var2 . 123)
;;       (var3 . <http://example.org/uri>)
;;       (var4 . ("2017-08-01" . <http://www.w3.org/2001/XMLSchema#dateTime>))) ...)
```

## API

### General

**[procedure]** (define-namespace prefix namespace)

Define namespaces for querying and expanding. The following namespaces are defined by default:

```
(define-namespace foaf "http://xmlns.com/foaf/0.1/")
(define-namespace dc "http://purl.org/dc/elements/1.1/")
(define-namespace rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(define-namespace owl "http://www.w3.org/2002/07/owl#")
(define-namespace skos "http://www.w3.org/2004/02/skos/core#")
```

### Writing SPARQL 

**[parameter]** (\*expand-namespaces?\*)

Defaults to #t.

**[procedure]** (write-triples triples)

Returns the RDF expression of the given s-sparql expression, which should be a list of triples as defined above.

**[procedure]** (write-sparql query)

Returns the RDF expression of the given s-sparql expression, which should be a full or partial SPARQL query, but not a triples block.

**[procedure]** (new-sparql-variable)

**[procedure]** (new-blank-node #!optional prefix)

**[procedure]** (sparql-variable string)

**[procedure]** (expand-namespace nspair)

```
(expand-namespace 'dc:title) ;; => '<http://purl.org/dc/elements/1.1/title>
```

**[procedure]** (write-uri uri)

### Parsing SPARQL

**[procedure]** (parse-query <string>)

Return the s-sparql representation of the given string.

**[procedure]** (read-uri string)

### Convenience functions

Functions for writing queries without using the complete s-sparql format.

**[parameter]** (\*default-graph\*)

s-sparql representation of the default graph for queries, e.g., `(\*default-graph* '<http://example.org/application>)`. If this is set, a "WITH <GRAPH>" is added to `s-select`, `s-insert` and `s-delete` expressions.

**[procedure]** (s-select vars statements #!key with-graph from-graph from-named-graphs)

vars can be a string (`"?s ?p"`), a symbol (`'?s`) or a list (`'(?s ?p ?o)`).

statements can be an RDF string or an s-sparql triples expression.

**[procedure]** (s-insert statements #!key with-graph)

**[procedure]** (s-delete statements #!key insert with-graph from-graph from-named-graphs where

### Querying SPARQL Endpoints

**[parameter]** (\*sparql-endpoint\*)

Defaults to "http://127.0.0.1:8890/sparql".

**[parameter]** (\*print-queries?\*) boolean

Defaults to #t.

**[parameter]** sparql-headers

Additional headers to be included in request to the SPARQL endpoint.

**[procedure]** (sparql-select query #!rest args)

**[procedure]** (sparql-select-unique query #!rest args)

query is a `format` string, with optional args. Returns a list of association lists representing the variable bindings, or in the case of unique, a single alist.

```
(sparql-select "SELECT * WHERE { ?s ?p ?o }")

;; => '(((s . "s1") (p . "p1"))
;;      ((s . "s2") (p . "p2")))

(sparql-select-unique "SELECT * WHERE { ?s a ~A }" '<http://example.com/Person>)

;; => '((s . <http://example.com/users/person01>))
```

**[procedure]** (sparql-update query #!rest args)

**[procedure]** (query-with-vars (vars ...) query form)

**[procedure]** (query-unique-with-vars (vars ...) query form)

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

**[procedure]** (expand-triple triple)

**[procedure]** (rewrite-query query rules)

**[procedure]** (rewrite block bindings rules)







