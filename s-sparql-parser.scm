;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

(module s-sparql-parser *
  (import chicken scheme extras data-structures srfi-1) 

(use srfi-1 srfi-13 matchable irregex)

(require-extension typeclass input-classes abnf abnf-charlist abnf-consumers
                   lexgen)

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

(define consumed-chars->number
  (consumed-chars->list 
   (compose string->number list->string)))

(define-syntax ->number
  (syntax-rules () 
    ((_ p)    (bind consumed-chars->number p))
    ))

(define fws
  (concatenation
   (optional-sequence 
    (concatenation
     (repetition char-list/wsp)
     (drop-consumed 
      (alternatives char-list/crlf char-list/lf char-list/cr))))
   (optional-sequence
    (repetition char-list/wsp))))
;;   (repetition char-list/wsp)))

(define (between-fws p)
  (concatenation
   (optional-sequence (drop-consumed fws)) 
   ;; (drop-consumed (optional-sequence fws)) 
   p
   (optional-sequence (drop-consumed fws)) ))
;;(drop-consumed (optional-sequence fws))))

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
;; simplified 

(define STRING_LITERAL1
  (:: (char-list/lit "\"")
      (:+ (alternatives char-list/alpha
                        char-list/decimal))
      (char-list/lit "\"")))

(define STRING_LITERAL2
  (:: (char-list/lit "'")
      (:+ (alternatives char-list/alpha
                        char-list/decimal))
      (char-list/lit "'")))


;; Exponent

;; DOUBLE_NEGATIVE

(define DECIMAL_NEGATIVE
  (vac
   (:: (lit/sym "-") DECIMAL)))

(define INTEGER_NEGATIVE
  (vac
   (:: (lit/sym "-") INTEGER)))

;; DOUBLE_POSITIVE

(define DECIMAL_POSITIVE
  (vac
   (:: (lit/sym "+") DECIMAL)))

(define INTEGER_POSITIVE
  (vac
   (:: (lit/sym "+") INTEGER)))

;; DOUBLE

(define DECIMAL
  (->number
   (:: 
    (:* char-list/decimal)
    (char-list/lit ".")
    (:+ char-list/decimal))))

(define INTEGER (->number (:+ char-list/decimal)))

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
     char-list/decimal
     (set-from-string "-_&?#./")))
;     (char-list/lit "/")))
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
    (between-fws IRIREF)) ;; whitespace...
    ;; IRIREF)
   PrefixedName))

(define String
  (bind-consumed->string
   (alternatives
    (:: (drop-consumed (char-list/lit "\""))
	(repetition
	 (alternatives
	  STRINGCHAR ECHAR))
        (drop-consumed (char-list/lit "\"")))
   (:: (drop-consumed (char-list/lit "'"))
	(repetition
	 (alternatives
	  STRINGCHAR ECHAR))
        (drop-consumed (char-list/lit "'"))))))

(define BooleanLiteral
   (alternatives
    (lit/sp "true")
    (lit/sp "false")))

(define NumericLiteralUnsigned
  (alternatives INTEGER DECIMAL)) ;; DOUBLE

(define NumericLiteralPositive
  (alternatives INTEGER_POSITIVE DECIMAL_POSITIVE)) ;; DOUBLE_POSITIVE))

(define	NumericLiteralNegative 
  (alternatives INTEGER_NEGATIVE DECIMAL_NEGATIVE)) ;; DOUBLE_NEGATIVE))

(define NumericLiteral
  (alternatives NumericLiteralUnsigned NumericLiteralPositive NumericLiteralNegative))

(define RDFLiteral
  (:: String)) ;; ( LANGTAG | ( '^^' iri ) )?

(define iriOrFunction
  (vac
   (:: iri (:? ArgList))))

(define (bracketted-function label content)
  (vac
   (->list 
    (:: (lit/sym label)
        (drop-consumed (lit/sp "("))
        content
        (drop-consumed (lit/sp ")"))))))

(define (aggregate-expression label content)
  (vac
   (bracketted-function label
                        (:: (:? (lit/sym "DISTINCT")) 
                            content))))

(define Aggregate
  (vac
   (alternatives
    (aggregate-expression "COUNT" (alternatives (lit/sym "*") 
                                                Expression))
    (aggregate-expression "SUM" Expression)
    (aggregate-expression "MIN" Expression)
    (aggregate-expression "MAX" Expression)
    (aggregate-expression "AVG" Expression)
    (aggregate-expression "SAMPLE" Expression)
    (aggregate-expression "GROUP_CONCAT"
                          (:: Expression 
                              (:? (:: (lit/sym ";")
                                      (lit/sym "SEPARATOR")
                                      (lit/sym "=")
                                      String)))))))

(define NotExistsFunc
  (vac
   (:: (lit/sym "NOT") (lit/sym "EXISTS") GroupGraphPattern)))

(define ExistsFunc
  (vac
   (:: (lit/sym "EXISTS") GroupGraphPattern)))

(define StrReplaceExpression
  (vac
   (bracketted-function 
    "REPLACE"
    (:: Expression
        (drop-consumed (lit/sp ",")) Expression
        (drop-consumed (lit/sp ",")) Expression
        (:? (:: (drop-consumed (lit/sp ",")) Expression))))))

(define SubstringExpression
  (vac
   (bracketted-function 
    "SUBSTR"
    (:: Expression
        (drop-consumed (lit/sp ",")) Expression
        (:? (:: (drop-consumed (lit/sp ",")) Expression))))))

(define RegexExpression
  (vac
   (bracketted-function 
    "REGEX"
    (:: Expression
        (drop-consumed (lit/sp ",")) Expression
        (:? (:: (drop-consumed (lit/sp ",")) Expression))))))

(define BuiltInCall
  (vac
   (alternatives
    Aggregate
    (bracketted-function "STR" Expression)
    (bracketted-function "LANG" Expression)
    (bracketted-function "LANGMATCHES" (:: Expression (drop-consumed (lit/sp ",")) Expression))
    (bracketted-function "DATATYPE" Expression)
    (bracketted-function "BOUND" Expression)
    (bracketted-function "IRI" Expression)
    (bracketted-function "URI" Expression)
    (bracketted-function "BNODE" Expression)
    (:: (lit/sp "RAND") NIL)
   (bracketted-function "ABS" Expression)
   (bracketted-function "CEIL" Expression)
   (bracketted-function "FLOOR" Expression)
   (bracketted-function "ROUND" Expression)
   (bracketted-function "CONCAT" Expression)
   SubstringExpression
   (bracketted-function "STRLEN" Expression)
   StrReplaceExpression
   (bracketted-function "UCASE" Expression)
   (bracketted-function "LCASE" Expression)
   (bracketted-function "ENCODE_FOR_URI" Expression)
   (bracketted-function "CONTAINS" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRSTARTS" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRENDS" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRBEFORE" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRAFTER" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "YEAR" Expression)
   (bracketted-function "MONTH" Expression)
   (bracketted-function "DAY" Expression)
   (bracketted-function "HOURS" Expression)
   (bracketted-function "MINUTES" Expression)
   (bracketted-function "SECONDS" Expression)
   (bracketted-function "TIMEZONE" Expression)
   (bracketted-function "TZ" Expression)
   (:: (lit/sym "NOW") NIL)
   (:: (lit/sym "UUID") NIL)
   (:: (lit/sym "STRUUID") NIL)
   (bracketted-function "MD5" Expression)
   (bracketted-function "SHA1" Expression)
   (bracketted-function "SHA256" Expression)
   (bracketted-function "SHA384" Expression)
   (bracketted-function "SHA512" Expression)
   (:: (lit/sym "COALESCE") ExpressionList)
   (bracketted-function "IF" (:: Expression (drop-consumed (lit/sp ",")) Expression
                                 (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRLANG" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "STRDT" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "sameTerm" (:: Expression (drop-consumed (lit/sp ",")) Expression))
   (bracketted-function "isIRI" Expression)
   (bracketted-function "isURI" Expression)
   (bracketted-function "isBLANK" Expression)
   (bracketted-function "isLITERAL" Expression)
   (bracketted-function "isNUMERIC" Expression)
   RegexExpression
   ExistsFunc
   NotExistsFunc)))

(define BrackettedExpression
  (vac
   ;;(->list ;; '|@()| ;; ??
    (::
     (drop-consumed (lit/sp "("))
     Expression
     (drop-consumed (lit/sp ")")))))

(define PrimaryExpression
  (vac
  (alternatives
   (->alist '|@()| BrackettedExpression)
   BuiltInCall iriOrFunction RDFLiteral
   NumericLiteral BooleanLiteral Var)))

;; To Polish Notation!!
(define UnaryExpression
  (alternatives
   (:: (lit/sym "!") PrimaryExpression)
   (:: (lit/sym "+") PrimaryExpression)
   (:: (lit/sym "-") PrimaryExpression)
   PrimaryExpression))

;; To Polish Notation!!
(define MultiplicativeExpression
  (::
   UnaryExpression
   (:*
    (alternatives
     (:: (lit/sym "*")
         UnaryExpression)
     (:: (lit/sym "/")
         UnaryExpression)))))

;; To Polish Notation!!
(define AdditiveExpression
  (:: MultiplicativeExpression
      (:*
       (alternatives
        (:: (lit/sym "+") MultiplicativeExpression)
        (:: (lit/sym "-") MultiplicativeExpression)
        (:: 
         (alternatives  NumericLiteralPositive NumericLiteralNegative)
         (:*
          (alternatives
           (:: (lit/sym "*") UnaryExpression)
           (:: (lit/sym "/") UnaryExpression))))))))
				
(define NumericExpression AdditiveExpression)

;; To Polish Notation!!
(define RelationalExpression 
  (vac
   (:: NumericExpression
       (:? 
        (alternatives
         (:: (lit/sym "=") NumericExpression)
         (:: (lit/sym "!=") NumericExpression)
         (:: (lit/sym "<") NumericExpression)
         (:: (lit/sym ">") NumericExpression)
         (:: (lit/sym ">=") NumericExpression)
         (:: (lit/sym "IN") ExpressionList)
         (:: (lit/sym "NOT") (lit/sym "IN") ExpressionList))))))

(define ValueLogical RelationalExpression)

(define ConditionalAndExpression
  (:: ValueLogical (:* (:: (lit/sym "&&") ValueLogical))))

(define ConditionalOrExpression
  (:: ConditionalAndExpression
      (:* (:: (lit/sym "||") ConditionalAndExpression)))) ;; => Polish notation?

(define Expression ConditionalOrExpression)
  
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
     (bind-consumed->symbol 
      (char-list/lit "a"))
     ;;(lit/sym "a")
     ;; (:: (char-list/lit "!") PathNegatedPropertySet) ;; ** !!
     (:: (lit/sp "(") Path (lit/sp ")")))))

(define PathMod
  (set-from-string "?*+"))

(define PathEltOrInverse
  (vac
   (alternatives PathElt 
                 (:: (char-list/lit "^") PathElt))))

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

(define ExpressionList
  (alternatives
   NIL
   (->list
    (:: (drop-consumed (lit/sp "("))
        Expression
        (:* (::
             (drop-consumed (lit/sp "("))
             Expression))
        (drop-consumed (lit/sp ")"))))))

(define ArgList
  (alternatives
   NIL
   (->list
    (:: (drop-consumed (lit/sp "("))
        (:? (lit/sym "DISTINCT"))
        Expression
        (:* (::
             (drop-consumed (lit/sp "("))
             Expression))
        (drop-consumed (lit/sp ")"))))))

(define FunctionCall (:: iri ArgList))

(define Constraint (->list (alternatives BrackettedExpression BuiltInCall FunctionCall)))

(define Filter (->list (:: (lit/sym "FILTER") Constraint)))

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
   (->list
    (:: (lit/sym "OPTIONAL")
        GroupGraphPattern))))

(define GraphPatternNotTriples
  (alternatives GroupOrUnionGraphPattern
                OptionalGraphPattern
		MinusGraphPattern
                GraphGraphPattern
                ;;ServiceGraphPattern
                Filter
                ;;Bind InlineData
                ))

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

(define UsingClause
  (:: 
   (lit/sym "USING")
   (alternatives 
    iri 
    (:: (lit/sym "NAMED") iri))))
  

(define InsertClause
  (:: 
   (lit/sym "INSERT")
   QuadData))

(define DeleteClause
  (:: 
   (lit/sym "DELETE")
   QuadData))

(define Modify
  (:: (:? (->alist '@Dataset (:: (lit/sym "WITH") iri)))
      (->list
       (alternatives 
        (:: DeleteClause
            (:? InsertClause))
        InsertClause))
      (->alist '@Using (:* UsingClause))
      (->list
       (::
        (lit/sym "WHERE")
        GroupGraphPattern))))

(define DeleteWhere
  (->list
   (:: 
    (bind-consumed->symbol
     (::
      (lit/sp "DELETE ")
      (drop-consumed fws)
      (lit/sp "WHERE")))
    QuadData)))

(define DeleteData 
  (->list
   (:: 
    (bind-consumed->symbol
     (::
      (lit/sp "DELETE ")
      (drop-consumed fws)
      (lit/sp "DATA")))
    QuadData)))

(define InsertData
  (->list
   (:: 
    (bind-consumed->symbol
     (::
      (lit/sp "INSERT ")
      (drop-consumed fws)
      (lit/sp "DATA")))
    QuadData)))

;; Copy

;; Move

;; Add

;; Create

;; Drop

;; Clear

;; Load

(define Update1
  (alternatives ;;Load Clear Drop Add Move Copy Create 
   InsertData DeleteData DeleteWhere Modify))

(define Update
  (vac
   (:: (->alist '@Prologue Prologue)
       (->alist '@Update
                (:? (:: Update1 (:? (:: (char-list/lit ";") Update))))))))

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
    (bind-consumed->symbol 
     (alternatives
      (lit/sp "SELECT DISTINCT")
      (lit/sp "SELECT REDUCED")
      (lit/sp "SELECT")))
    ;; (lit/sym "SELECT")
    ;; (:?
    ;; (alternatives (lit/sym "DISTINCT") (lit/sym "REDUCED")))
    (alternatives
;     (->list
      (:+
       (between-fws 
        (alternatives
         Var
         (:: (lit/sp "(") Expression (lit/sym "AS") Var (lit/sp ")")))))
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

;;  BaseDecl

(define Prologue
  ;; (bind-consumed-values->alist
  ;;'*PROLOGUE*
   (repetition PrefixDecl))

(define UpdateUnit
(->alist '@Unit  Update))

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
  (->alist '@Unit Query))

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
           (let ((nested (alist-ref (car keys) alist)))
             (and nested
                  (nested-alist-ref* nested (cdr keys)))))))

(define (assoc-when sym alist)
  (and alist
      (assoc sym alist)))

(define (unit-prologue QueryUnit)
  (nested-alist-ref QueryUnit '@Unit '@Prologue))

(define (unit-query QueryUnit)
  (nested-alist-ref QueryUnit '@Unit '@Query))

(define (unit-query-dataset QueryUnit)
  (nested-alist-ref QueryUnit '@Unit '@Query '@Dataset))

(define (unit-query-select QueryUnit)
  (assoc-when 'SELECT (nested-alist-ref QueryUnit '@Unit '@Query)))

(define (unit-query-where QueryUnit)
  (assoc-when 'WHERE (nested-alist-ref QueryUnit '@Unit '@Query)))

(define (unit-update QueryUnit)
  (nested-alist-ref QueryUnit '@Unit '@Update))

(define (unit-update-delete QueryUnit)
  (assoc-when 'DELETE (unit-update QueryUnit)))

(define (unit-update-where QueryUnit)
  (assoc-when 'WHERE (unit-update QueryUnit)))

(define (unit-update-insert QueryUnit)
  (assoc-when 'INSERT (unit-update QueryUnit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(define r (parse-query "PREFIX pre: <http://www.home.com/> 
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
"))

(define s (parse-query "PREFIX pre: <http://www.home.com/> 
                  PREFIX rdf: <http://www.gooogle.com/> 
SELECT ?a ?a
FROM <http://www.google.com/>
FROM NAMED <http://www.google.com/>  
  WHERE {
     GRAPH ?g { ?s ?p ?o }
  } 
"))

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

DELETE WHERE {
   ?a mu:uuid ?b . ?c mu:uuid ?e . ?f ?g ?h
  } 

"))


(define vs
"PREFIX dc: <http://schema.org/dc/> 
PREFIX mu: <http://mu.semte.ch/vocabularies/core/>

DELETE {
   ?a mu:uuid ?b, ?c, ?d . ?c mu:uuid ?e . ?f ?g ?h
  } 
WHERE {
  ?s ?p ?o.
FILTER( ?s < 10)
}

")

(define v (parse-query vs))
(use s-sparql)

)
