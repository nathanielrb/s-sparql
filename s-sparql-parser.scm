(require-extension typeclass input-classes abnf abnf-charlist  abnf-consumers)

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

(define (bracketed p)
  (->alist
   '|[]| p))

(define (braced p)
  (->alist
   '|{}| p))

(define (paren-ed p)
  (->alist
   '|()| p))

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
  (bind-consumed->symbol
   (concatenation
    (repetition1 char-list/alpha) (char-list/lit ":"))))

(define IRIREF
  (bind-consumed->symbol
   (between-fws
    (concatenation
     (char-list/lit "<http://")
     (repetition1 
      (alternatives ;; should be list-from-string
       char-list/alpha
       (char-list/lit ".")
       (char-list/lit "/")))
     (char-list/lit ">")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar

(define BlankNode
   (alternatives BLANK_NODE_LABEL ANON))

(define PrefixedName
  (bind-consumed->symbol
   (between-fws
    (alternatives PNAME_LN PNAME_NS))))

(define iri
  (alternatives IRIREF PrefixedName))

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
   (bind-consumed-values->alist
    '*BLANK*
   (:: (lit/sym "[")
       PropertyListPathNotEmpty
       (lit/sym "]")))) )

(define TriplesNodePath
  (alternatives CollectionPath BlankNodePropertyListPath))

(define BlankNodePropertyList
  (vac
   (bind-consumed-values->alist
    '*BLANK*
   (:: (lit/sym "[")
       PropertyListNotEmpty
       (lit/sym "]")))) )

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
   (::
    GroupGraphPattern
    (:* (::
	 (lit/sp "UNION") GroupGraphPattern)))))

(define MinusGraphPattern
  (vac
   (:: (lit/sp "MINUS") GroupGraphPattern)))

;; DataBlockValue

;; InlineDataFull

;; InlineDataOneVar

;; DataBlock

;; InlineData

;; Bind

;; ServiceGraphPattern

(define GraphGraphPattern
  (vac
   (:: (lit/sp "GRAPH")
       VarOrIri
       GroupGraphPattern)))

(define OptionalGraphPattern
  (vac
   (:: (lit/sym "OPTIONAL")
       VarOrIri
       GroupGraphPattern)))

(define GraphPatternNotTriples
  (alternatives GroupOrUnionGraphPattern OptionalGraphPattern
		MinusGraphPattern GraphGraphPattern))

(define TriplesBlock
  (vac
   (:: (bind-consumed-values->list
	TriplesSameSubjectPath)
       (:? (:: (drop-consumed (lit/sp "."))
	       (:? TriplesBlock))))))

(define GroupGraphPatternSub
   (::
    (:? TriplesBlock) ))
    ;; (:*
    ;; (:: GraphPatternNotTriples
    ;; (:? (lit/sp "."))
    ;; (:? TriplesBlock))))

(define GroupGraphPattern 
  (vac
   (->alist
    '|@{}|
    (:: (drop-consumed (lit/sp "{"))
        GroupGraphPatternSub
        ;;(alternatives SubSelect GroupGraphPatternSub)
        (drop-consumed (lit/sp "}"))))))

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
  (bind-consumed-values->list
   (::
    (lit/sym "WHERE")
    GroupGraphPattern)))

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
  (:: (->alist 
       '@SelectClause SelectClause)
      (->alist
       '@DatasetClause (:* DatasetClause))
      (->alist
       '@WhereClause WhereClause)
      ;; SolutionModifier
      ))

(define PrefixDecl
  (bind-consumed-values->list
   (concatenation
    (lit/sym "PREFIX")
    PNAME_NS
    IRIREF)))

;; BaseDecl

(define Prologue
  ;;(bind-consumed-values->alist
   ;;'*PROLOGUE*
   (repetition PrefixDecl))

;; UpdateUnit

(define Query
  (concatenation
   (->alist
    '@Prologue Prologue)
   (->alist
    '@SelectQuery SelectQuery)))
;; ConstructQuery DescribeQuery AskQuery )
;;	       ValuesClause))

;; QueryUnit

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
    ?p ?o ?v .
    ?p ?x ?x, ?zoop  .
     ?p ?z ?l, ?m, ?o; ?zan ?zim, ?zing, ?zot;
  } 
")))

(define (nested-alist-ref alist #!rest keys)
  (nested-alist-ref* alist keys))

(define (nested-alist-ref* alist keys)
  (if (null? keys)
      alist
      (nested-alist-ref
       (alist-ref (car keys) alist)
       (cdr keys))))

(print r)

(print (lex GroupGraphPattern err " { ?p ?z ?l, ?m, ?o; ?zan ?zim, ?zing, ?zot; }"))
