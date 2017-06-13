(require-extension typeclass input-classes abnf abnf-charlist  abnf-consumers)

(use matchable)

(define *alist?* (make-parameter #f))

;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

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

(define (value? x)
  (or (symbol? x) (char? x)
      (string? x) (number? x) (boolean? x)
      (vector? x) (null? x) (pair? x) (symbol? x)))

(define consumed-values (consumed-objects value?))

;; helper macro for mutually-recursive parser definitions

(define-syntax vac
  (syntax-rules ()
    ((_ fn) (lambda args (apply fn args)))))

(define consumed-values->list
  (consumed-objects-lift consumed-values))

;; shortcut for (abnf:bind (consumed-values->list ...) ... )

(define-syntax bind-consumed-values->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)  p))
    ))

(define-syntax ->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)  p))
    ))

(define-syntax bind-consumed-values->alist
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
    ))

;; connect to *alist?*
(define-syntax ->alist
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
    ))


(define fws
  (concatenation
   (optional-sequence 
    (concatenation
     (repetition char-list/wsp)
     (drop-consumed 
      (alternatives char-list/crlf char-list/lf char-list/cr))))
   (repetition char-list/wsp)))
;;   (repetition char-list/wsp)))


(define (between-fws p)
  (concatenation
   (drop-consumed (optional-sequence fws)) p 
   (drop-consumed (optional-sequence fws))))

(define (err s)
  (print "lexical error on stream: " s)
  `(error))

(define (lit/sp str)
  (between-fws (char-list/lit str)))

(define (lit/sym str)
  (bind-consumed->symbol
   (between-fws (char-list/lit str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Production for terminals

;; PN_LOCAL_ESC

;; HEX

;; PERCENT

;; PLX

(define PN_LOCAL  ;; **
  (:+
   (alternatives
    char-list/decimal
    char-list/alpha
    (set-from-string "-_%"))))

;; PN_PREFIX

(define PN_CHARS
  (vac
   (alternatives
    PN_CHARS_U
    (set-from-string "-")))) ;; ** !!

(define varname
  (:+
   (alternatives
    char-list/decimal
    char-list/alpha
    (set-from-string "-_"))))

(define PN_CHARS_U
  (vac
   (alternatives
    PN_CHARS_BASE
    (set-from-string "_"))))

(define PN_CHARS_BASE
  (vac
   (alternatives
    char-list/decimal
    char-list/alpha))) ;; **
;;   (set-from-string "_")

(define ANON
  (->alist
   '|@[]|
   ;;(bind-consumed->symbol
    (:: (drop-consumed (lit/sp "["))
        (drop-consumed (:? fws))
        (drop-consumed (lit/sp "]")))))

;; WS

(define NIL
  (:: (lit/sp "(") (lit/sp ")")))

(define ECHAR
  (:: (char-list/lit "\\")
      (set-from-string "tbnrf\\\"'")))

;; ??? for stringliterals
(define STRINGCHAR
  (alternatives
   char-list/decimal
   char-list/alpha)) ;; **

;; STRING_LITERAL_LONG2/LONG1/1/2

;; Exponent

;; DOUBLE_NEGATIVE

;; DECIMAL_NEGATIVE

;; INTEGER_NEGATIVE

;; DOUBLE_POSITIVE

;; DECIMAL_POSITIVE

;; INTEGER_POSITIVE

;; DOUBLE

;; DECIMAL

;; INTEGER

;; LANGTAG

;; VAR2

;; VAR1

(define BLANK_NODE_LABEL
  (vac
   (::
   (char-list/lit "_:")
   (alternatives PN_CHARS_U
		 char-list/decimal
		 char-list/alpha) ;; **
   (:?
    (::
     (:* (alternatives
	  PN_CHARS
	  (char-list/lit ".")))
     PN_CHARS)))))

(define PNAME_LN
  (vac
   (concatenation PNAME_NS PN_LOCAL)) )

(define PNAME_NS
  (concatenation
   (repetition1 char-list/alpha) 
   (char-list/lit ":")))

(define IRIREF ;; **
  (concatenation
   (char-list/lit "<http://")
   (repetition1 
    (alternatives ;; should be list-from-string
     char-list/alpha
     (char-list/lit ".")
     (char-list/lit "/")))
   (char-list/lit ">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar

(define BlankNode
   (alternatives BLANK_NODE_LABEL ANON))

(define PrefixedName
  (bind-consumed->symbol
   (alternatives PNAME_LN PNAME_NS)))

(define iri
  (alternatives 
   (bind-consumed->symbol
    (between-fws IRIREF))
   PrefixedName))

(define String
  (alternatives
   (::  (char-list/lit "'")
	(repetition
	 (alternatives
	  STRINGCHAR ECHAR))
	(char-list/lit "'"))))

(define BooleanLiteral
   (alternatives
    (lit/sp "true")
    (lit/sp "false")))

;; NUMERICLITERALNEGATIVE/Positive/Unsigned

(define NumericLiteral
   (::
    (:? (alternatives
	 (char-list/lit "-")
	 (char-list/lit "+")))
    (:* char-list/decimal)
    (:? (char-list/lit "+"))
    (:+ char-list/decimal)))

(define RDFLiteral
  (:: String)) ;; ( LANGTAG | ( '^^' iri ) )?

;; iriOrFunction

;; Aggregate

;; NotExistsFunction

;; ExistsFunc

;; StrReplaceExpression

;; SubstringExpression

;; RegexExpression

;; BuiltInCall

;; BrackettedExpression

;; PrimaryExpression

;; UnaryExpression

;; MutiplicativeExpression

;; AdditiveExpression
					;
;; RelationalExpression

;; ValueLogical

;; ConditionalAndExpression

;; ConditionalOrExpression

(define Expression (lit/sp "?b")) ;; **

(define GraphTerm
   (alternatives
    iri RDFLiteral NumericLiteral BooleanLiteral BlankNode NIL))

(define Var
  (bind-consumed->symbol
   (alternatives
    (concatenation (char-list/lit "?") varname)
    (concatenation (char-list/lit "$") varname))))

(define VarOrIri
  (alternatives Var iri))

(define VarOrTerm
  (alternatives Var GraphTerm))

(define GraphNodePath
  (vac
   (alternatives
    VarOrTerm TriplesNodePath)))

(define GraphNode
  (vac
   (alternatives
    VarOrTerm TriplesNode)))

(define CollectionPath
  (vac
    (:: (lit/sp "(")
	(:+ GraphNodePath)
	(lit/sp ")"))))

(define Collection
  (vac
    (:: (lit/sp "(")
	(:+ GraphNode)
	(lit/sp ")"))))

(define BlankNodePropertyListPath
  (vac
   (->alist
    '|@[]|
   (:: (drop-consumed (lit/sym "["))
       PropertyListPathNotEmpty
        (drop-consumed (lit/sym "]")))) ))

(define TriplesNodePath
  (alternatives CollectionPath BlankNodePropertyListPath))

(define BlankNodePropertyList
  (vac
   (->alist
    '|@[]|
    (:: (drop-consumed (lit/sym "["))
       PropertyListNotEmpty
        (drop-consumed (lit/sym "]")))) ))

(define TriplesNode
   (alternatives Collection BlankNodePropertyList)) ;; ** !!

;; Integer

;; PathOneInPropertySet

;; PathNegatedPropertySet

(define PathPrimary
  (vac
    (alternatives
     iri
     (lit/sp "a")
     ;; (:: (char-list/lit "!") PathNegatedPropertySet) ;; ** !!
     (:: (lit/sp "(") Path (lit/sp ")")))))

(define PathMod
  (set-from-string "?*+"))

(define PathEltOrInverse
  (vac
   (alternatives PathElt (:: (char-list/lit "^") PathElt))))

(define PathElt
   (:: PathPrimary (:? PathMod)))

(define PathSequence
  (:: PathEltOrInverse
      (:* (:: (lit/sp "/") PathEltOrInverse))))

(define PathAlternative
   (:: PathSequence
       (:*
	(:: (lit/sp "|") PathSequence))))

(define Path PathAlternative)

(define ObjectPath GraphNodePath)

(define	ObjectListPath
;;  (->list
   (:: ObjectPath
       (:*
        (:: (drop-consumed (lit/sp ","))
            ObjectPath))))

(define VerbSimple Var)

(define VerbPath Path)

(define PropertyListPathNotEmpty
  (vac
   (->list
   (::
    (->list
     (::
      (between-fws (alternatives VerbPath VerbSimple))
      ObjectListPath))
    (:*
     (:: (drop-consumed (lit/sp ";"))
         (:?
          (->list
           (::
            (between-fws (alternatives VerbPath VerbSimple))
            ObjectList)))))))))

(define PropertyListPath
  (:? PropertyListPathNotEmpty))

(define TriplesSameSubjectPath
  (alternatives
   (:: (between-fws VarOrTerm)
       (between-fws PropertyListPathNotEmpty))
   (:: (between-fws TriplesNodePath)
       (between-fws PropertyListPath))))

(define Object GraphNode)

(define ObjectList
  ;(->list
   (:: Object
       (:*
	(:: (drop-consumed (lit/sp ","))
	    Object))))

(define Verb (alternatives VarOrIri (lit/sym "a")))

(define PropertyListNotEmpty
  (:: Verb ObjectList
      (:* (:: (drop-consumed (lit/sp ";"))
              (:? (:: Verb ObjectList))))))

;; PropertyList

;; TriplesSameSubject

;; ConstructTriples

;; ConstructTemplate

;; ExpressionList

;; ArgList

;; FunctionCall

;; Constraint

;; Filter

(define GroupOrUnionGraphPattern
  (vac
   (->alist
    'UNION
    (::
     GroupGraphPattern
     (:* (::
          (drop-consumed (lit/sp "UNION"))
          GroupGraphPattern))))))

(define MinusGraphPattern
  (vac
   (:: (lit/sym "MINUS") GroupGraphPattern)))

;; DataBlockValue

;; InlineDataFull

;; InlineDataOneVar

;; DataBlock

;; InlineData

;; Bind

;; ServiceGraphPattern

(define GraphGraphPattern
  (vac
   (->list
    (:: (lit/sym "GRAPH")
        VarOrIri
        GroupGraphPattern))))

(define OptionalGraphPattern
  (vac
   (:: (lit/sym "OPTIONAL")
       VarOrIri
       GroupGraphPattern)))

(define GraphPatternNotTriples
  (alternatives GroupOrUnionGraphPattern
                OptionalGraphPattern
		MinusGraphPattern
                GraphGraphPattern))

(define TriplesBlock
  (vac
   (:: (bind-consumed-values->list
	TriplesSameSubjectPath)
       (:? (:: (drop-consumed (lit/sp "."))
	       (:? TriplesBlock))))))

(define GroupGraphPatternSub
   (::
    (:? TriplesBlock)
     (:*
      (:: GraphPatternNotTriples
          (:? (lit/sp "."))
          (:? TriplesBlock)))))

(define GroupGraphPattern 
  (vac
   ;;(->list
    ;;'|@{}|
    (:: (drop-consumed (lit/sp "{"))
        (alternatives
         ;; SubSelect
         GroupGraphPatternSub)
        (drop-consumed (lit/sp "}")))))

;; TriplesTemplate

;; QuadsNotTriples

;; Quads

;; QuadData

;; QuadPattern

;; GraphRefAll

;; GraphRef

;; GraphOrDefault

;; UsingClause

;; InsertClause

;; Delete Clause

;; Modify

;; DeleteWhere

;; DeleteData

;; InsertData

;; Copy

;; Move

;; Add

;; Create

;; Drop

;; Clear

;; Load

;; Update1

;; Update

;; ValuesClause

;; OffsetClause

;; LimitClause

;; LimitOffsetClauses

;; OrderCondition

;; OrderClause

;; HavingClause

;; GroupCondition

;; GroupClause

;; SolutionModifier

(define WhereClause
  (::
   (lit/sym "WHERE")
   GroupGraphPattern))

(define SourceSelector iri)

(define NamedGraphClause
  (concatenation
   (lit/sym "NAMED")
   SourceSelector))

(define DefaultGraphClause SourceSelector)

(define DatasetClause
  (->list
   (::
    (lit/sym "FROM")
    (alternatives DefaultGraphClause NamedGraphClause))))

;; AskQuery

;; DescribeQuery

;; ConstructQuery

(define SelectClause
  ;(->alist
   ;'@SelectClause
   (::
    (lit/sym "SELECT")
    (:?
     (alternatives (lit/sym "DISTINCT") (lit/sym "REDUCED")))
    (alternatives
     (->list
      (:+
       (between-fws 
        (alternatives
         Var
         (:: (lit/sp "(") Expression (lit/sym "AS") Var (lit/sp ")"))))))
     (lit/sym "*"))))

(define SubSelect
   (:: SelectClause WhereClause
       ;; SolutionModifier ValuesClause
       ))

(define SelectQuery
  (:: (->list SelectClause) ;; '@SelectClause 
      (->alist '@Dataset (:* DatasetClause)) ;;'@DatasetClause
      (->list WhereClause) ;;'@WhereClause 
      ;; SolutionModifier
      ))

(define PrefixDecl
  (->list
   (concatenation
    (lit/sym "PREFIX")
    (bind-consumed->symbol 
     (between-fws PNAME_NS))
    (bind-consumed->symbol
     (between-fws IRIREF)))))

;; BaseDecl

(define Prologue
  ;;(bind-consumed-values->alist
   ;;'*PROLOGUE*
   (repetition PrefixDecl))

;; UpdateUnit

(define Query
  (concatenation
   (->alist '@Prologue Prologue)
   (->alist '@Query
            (alternatives
             SelectQuery
             ;; ConstructQuery DescribeQuery AskQuery )
             ))
   ;; (->alist '@Values ValuesClause)
   ))
;; QueryUnit


(define (nested-alist-ref alist #!rest keys)
  (nested-alist-ref* alist keys))

(define (nested-alist-ref* alist keys)
  (if (null? keys)
      alist
      (nested-alist-ref
       (alist-ref (car keys) alist)
       (cdr keys))))





(use s-sparql)

;; should also read from ports
(define (read-sparql str)
  (lex Query err str))

(define (query-prologue QueryUnit)
  (alist-ref '@Prologue QueryUnit))

(define (PrefixDecl? decl) (equal? (car decl) 'PREFIX))

(define (BaseDecl? decl) (equal? (car decl) 'BASE))

(define (remove-trailing-char sym #!optional (len 1))
  (let ((s (symbol->string sym)))
    (string->symbol
     (substring s 0 (- (string-length s) len)))))

(define (query-prefixes QueryUnit)
  (map (lambda (decl)
         (list (remove-trailing-char (cadr decl))
               (write-uri (caddr decl))))
       (filter PrefixDecl? (query-prologue QueryUnit))))

(define (query-bases QueryUnit)
  (map (lambda (decl)
         (list (cadr decl)
               (write-uri (caddr decl))))
       (map cdr (filter BaseDecl? (query-prologue QueryUnit)))))

(define (query-dataset QueryUnit)
  (alist-ref '@Dataset QueryUnit))

(define (query-query QueryUnit)
  (alist-ref '@Query QueryUnit))

(define (query-select QueryUnit)
  (assoc 'SELECT (query-query QueryUnit)))

(define (query-dataset QueryUnit)
  (alist-ref '@Dataset (query-query QueryUnit)))

(define (query-where QueryUnit)
  (assoc 'WHERE (query-query QueryUnit)))





(define (expand-special triple)
  (case (car triple)
    ((WHERE) 
     (cons (car triple) (join (map expand-triples (cdr triple)))))
    ((|@()| |@[]| MINUS OPTIONAL UNION)
     (list (cons (car triple) (join (map expand-triples (cdr triple))))))
    ((GRAPH) (list (append (take triple 2)
                           (join (map expand-triples (cddr triple))))))
    (else #f)))

(define (expand-triples triples)
  (or (expand-special triples)
      (expand-triple triple)))

(define (expand-triple triple)
  (match triple
    ((subject predicates)
     (let ((subject (car triple)))
       (join
        (map (lambda (po-list)
               (let ((predicate (car po-list)))
                 (map (lambda (object)
                        (list subject predicate object))
                      (cdr po-list))))
             predicates))))
    ((subject predicate . objects)
     (map (lambda (object)
            (list subject predicate object))
          objects))))

(define *rules* '(((_ mu:uuid) GRAPH:ONE)))

(define (rule-equal? subject predicate rule)
  (match-let
      ((((rule-subject rule-predicate) graph) rule))
    (and
     (or (equal? rule-subject '_) 
         (equal? rule-subject subject))
     (or (equal? rule-predicate '_) 
         (equal? rule-predicate predicate))
     graph)))

(define (lookup-rule subject predicate rules)
  (and (not (null? rules))
       (or (rule-equal? subject predicate (car rules))
           (lookup-rule subject predicate (cdr rules)))))

;; rewrite could do its own expanding, if objects are not matched against
;; to avoid over-graphing.
;; also, it should  group expanded triples by same graph...
(define (rewrite group)
  (case (car group)
    ((WHERE)
     (cons (car group) (join (map rewrite (cdr group)))))
    ((|@()| |@[]| MINUS OPTIONAL UNION)
     (list (cons (car group) (join (map rewrite (cdr group))))))
    ((GRAPH) (list (append (take group 2)
                           (join (map rewrite (cddr group))))))
    (else (let ((triples (expand-triple group)))
            (map (lambda (triple)
                   (let ((graph (lookup-rule (car triple) (cadr triple) *rules*)))
                     (if graph
                         `(GRAPH ,graph ,triple)
                         triple)))
                 triples)))))


(require-extension sort-combinators)

(define (graph-of group)
  (and (equal? (car group) 'GRAPH)
       (cadr group)))

;; (define (group-graphs groups)
;;   (if

(define (add-graph triple)
  (let ((graph (lookup-rule (car triple) (cadr triple) *rules*)))
    (if graph
        `(GRAPH ,graph ,triple)
        triple)))

(define (join-graphs groups)
  (map (lambda (group)
         (if (graph-of (car group))
             `(GRAPH ,(graph-of (car group))
                     ,@(join (map cddr group)))
             group))
       ((group-by graph-of) groups)))

(define (rewrite group)
  (case (car group)
    ((WHERE) 
     (cons (car group) (join-graphs (map rewrite (cdr group)))))
    ((|@()| |@[]| MINUS OPTIONAL UNION)
     (list (cons (car group) (join-graphs (map rewrite (cdr group))))))
    ((GRAPH) (list (append (take group 2)
                           (join (map rewrite (cddr group))))))
    (else (join (map add-graph (expand-triple group))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(require-extension lexgen)

(define r (car (lex Query err "PREFIX pre: <http://www.home.com/> 
                  PREFIX pre: <http://www.gooogle.com/> 
SELECT ?a ?a
FROM <http://www.google.com/>
FROM NAMED <http://www.google.com/>  
  WHERE {
    [] ?p ?o .
    [?b ?v] ?o ?v .
    ?p ?x ?x, ?zoop  .
     ?p ?z ?l, ?m, ?o; ?zan ?zim, ?zing, ?zot;
  } 
")))

(define s (car (lex Query err "PREFIX pre: <http://www.home.com/> 
                  PREFIX rdf: <http://www.gooogle.com/> 
SELECT ?a ?a
FROM <http://www.google.com/>
FROM NAMED <http://www.google.com/>  
  WHERE {
     GRAPH ?g { ?s ?p ?o }
  } 
")))

(define t (car (lex Query err "PREFIX mu: <http://mu.semte.ch/application/> 
PREFIX dc: <http://schema.org/dc/> 
SELECT ?a ?b
FROM <http://www.google.com/>
FROM NAMED <http://www.google.com/app>  
  WHERE {
   { ?a mu:uuid ?b . ?c mu:uuid ?e . ?f ?g ?h }
   UNION   { ?s mu:uuid ?o, ?p, ?q }  
    UNION { ?a dc:title ?title }
    UNION { GRAPH ?g { ?l mu:function ?u } }
  } 

")))

(print r)
(newline)
(print s)
