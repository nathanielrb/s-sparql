;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

;;(module s-sparql-parser *
;;(import chicken scheme extras data-structures srfi-1) 

(use srfi-1 srfi-13 matchable irregex)

(require-extension typeclass input-classes abnf abnf-charlist abnf-consumers
                   sort-combinators lexgen)

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
  (->list
   (:: ObjectPath
       (:*
        (:: (drop-consumed (lit/sp ","))
            ObjectPath)))))

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
  (->list
   (:: Object
       (:*
	(:: (drop-consumed (lit/sp ","))
	    Object)))))

(define Verb (alternatives VarOrIri (lit/sym "a")))

(define PropertyListNotEmpty
  (->list
   (:: (->list 
        (:: (between-fws Verb)
            ObjectList))
       (:* (:: (drop-consumed (lit/sp ";"))
               (:? (->list
                    (:: (between-fws Verb)
                        ObjectList))))))))

(define PropertyList (:? PropertyListNotEmpty))

(define TriplesSameSubject
  (alternatives (:: (between-fws VarOrTerm)
                    (between-fws PropertyListNotEmpty))
                (:: (between-fws TriplesNode)
                    (between-fws PropertyList))))

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
     (->list GroupGraphPattern)
     (:* (::
          (drop-consumed (lit/sp "UNION"))
          (->list GroupGraphPattern)))))))

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
   (:: (->list
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
    (:: (drop-consumed (lit/sp "{"))
        (alternatives
         ;; SubSelect
         GroupGraphPatternSub)
        (drop-consumed (lit/sp "}")))))

(define TriplesTemplate
  (vac
   (::
    (->list TriplesSameSubject)
    (:? (:: (drop-consumed (lit/sp "."))
            (:? TriplesTemplate))))))
  
(define QuadsNotTriples
  (::
   (lit/sym "GRAPH")
   VarOrIri
   (:: (drop-consumed (lit/sp "{"))
       (:? TriplesTemplate)
       (drop-consumed (lit/sp "}")))))

(define Quads
  (::
   (:? TriplesTemplate)
   (:* (::
        QuadsNotTriples
        (:? (drop-consumed (lit/sp ".")))
        (:? TriplesTemplate)))))

(define QuadData
  (:: (drop-consumed (lit/sp "{"))
      Quads
      (drop-consumed (lit/sp "}"))))


;; QuadPattern

;; GraphRefAll

;; GraphRef

;; GraphOrDefault

;; UsingClause

;; InsertClause

;; Delete Clause

;;(define Modify
;;  (:: (:? (:: (lit/sp "WITH") iri))
;;      (alternatives (:: DeleteClause (:? InsertClause))
;;                    InsertClause)
;;      (:* UsingClause)
;;      (lit/sp "WHERE")
;;      GroupGraphPattern))

(define DeleteWhere
  (:: 
   (bind-consumed->symbol
    (::
     (lit/sp "DELETE")
     (drop-consumed fws)
     (lit/sp "WHERE")))
   QuadData))

(define DeleteData 
  (:: 
   (bind-consumed->symbol
    (::
     (lit/sp "DELETE")
     (drop-consumed fws)
     (lit/sp "DATA")))
   QuadData))

(define InsertData
  (:: 
   (bind-consumed->symbol
    (::
     (lit/sp "INSERT")
     (drop-consumed fws)
     (lit/sp "DATA")))
   QuadData))

;; Copy

;; Move

;; Add

;; Create

;; Drop

;; Clear

;; Load

(define Update1
  (alternatives ;;Load Clear Drop Add Move Copy Create 
   InsertData DeleteData DeleteWhere))
;; Modify))

(define Update
  (vac
   (:: Prologue
       (:? (:: Update1 (:? (:: (char-list/lit ";") Update)))))))

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

(define UpdateUnit Update)

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

(define QueryUnit
  (->alist '@QueryUnit Query))

(define TopLevel (alternatives QueryUnit UpdateUnit))

;; should also read from ports
(define (parse-query query)
  ;;(cons '@TOP
  (car (lex TopLevel err query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors

(define (nested-alist-ref alist #!rest keys)
  (nested-alist-ref* alist keys))

(define (nested-alist-ref* alist keys)
  (and keys
       (if (null? keys)
           alist
           (nested-alist-ref*
            (alist-ref (car keys) alist)
            (cdr keys)))))


(define (query-prologue QueryUnit)
  (nested-alist-ref QueryUnit '@QueryUnit '@Prologue))

(define (PrefixDecl? decl) (equal? (car decl) 'PREFIX))

(define (BaseDecl? decl) (equal? (car decl) 'BASE))

(define (remove-trailing-char sym #!optional (len 1))
  (let ((s (symbol->string sym)))
    (string->symbol
     (substring s 0 (- (string-length s) len)))))

(define (query-dataset QueryUnit)
  (nested-alist-ref QueryUnit '@QueryUnit '@Query '@Dataset))

(define (query-query QueryUnit)
  (nested-alist-ref QueryUnit '@QueryUnit '@Query))

(define (query-select QueryUnit)
  (assoc 'SELECT (query-query QueryUnit)))

(define (query-dataset QueryUnit)
  (alist-ref '@Dataset (query-query QueryUnit)))

(define (query-where QueryUnit)
  (assoc 'WHERE (query-query QueryUnit)))

(define (graph-of group)
  (and (equal? (car group) 'GRAPH)
       (cadr group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

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

(define t (parse-query ;; (car (lex QueryUnit err "
"PREFIX dc: <http://schema.org/dc/> 
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
SELECT ?a ?b
FROM <http://www.google.com/>
FROM NAMED <http://www.google.com/app>  
  WHERE {
   { ?a mu:uuid ?b . ?c mu:uuid ?e . ?f ?g ?h }
   UNION   { ?s mu:uuid ?o, ?p, ?q }  
    UNION { ?a dc:title ?title }
    UNION { GRAPH ?g { ?l mu:function ?u } }
  } 

"))

(define u (parse-query ;; (car (lex QueryUnit err "
"PREFIX dc: <http://schema.org/dc/> 
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
SELECT ?a ?b

DELETE WHERE {
   ?a mu:uuid ?b . ?c mu:uuid ?e . ?f ?g ?h
  } 

"))

;;)
