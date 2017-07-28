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

(define-syntax ->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)  p))
    ))

(define-syntax ->alist
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
    ))

(define (2list->cons vals)
  (print "Vals " vals)
  (and (list? vals)
         (= (length vals) 2)
         (print (cons (car vals) (cadr vals)))
         (cons (car vals) (cadr vals))))

(define-syntax ->cons
  (syntax-rules () 
    ((_ p)    (bind (consumed-values->list 2list->cons) p))
    ))

(define consumed-chars->number
  (consumed-chars->list 
   (compose string->number list->string)))

(define-syntax ->number
  (syntax-rules () 
    ((_ p)    (bind consumed-chars->number p))
    ))

;; really buggy
(define (rel->polish lst)
  (print "Polish? " lst)
  (and (pair? lst)
       (pair? (car lst))
       (let ((lst (car lst)))  ;;(or (and (equal? (caar lst) '|@()|)
         (if (= (length lst) 3)
             (list (cadr lst)
                   (car lst)
                   (caddr lst))
             lst))))

;; (define consumed-values (consumed-objects pair?))

(define consumed-pairs->polish
  (consumed-pairs->list rel->polish))

(define-syntax ->polish
  (syntax-rules () 
    ((_ p)    (bind consumed-pairs->polish p))
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
    (:: (drop-consumed (lit/sp "["))
        (drop-consumed (:? fws))
        (drop-consumed (lit/sp "]")))))

;; WS

(define NIL
  (->alist
   `|@()|
   (:: 
    (drop-consumed (lit/sp "("))
    (drop-consumed (lit/sp ")")))))

(define ECHAR
  (:: (char-list/lit "\\")
      (set-from-string "tbnrf\\\"'")))

;; ??? for stringliterals
(define STRINGCHAR
  (alternatives
   (set-from-string " -_.")
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

(define LANGTAG 
  (bind-consumed->symbol
   (::    
     (char-list/lit "@")
     (:+ char-list/alpha)
     (:*
      (:: 
       (char-list/lit "-")
       (:+ char-list/alpha))))))

;; VAR2

;; VAR1

(define BLANK_NODE_LABEL
  (vac
   (::
   (char-list/lit "_:")
   (alternatives
    PN_CHARS_U
    char-list/decimal
    char-list/alpha) ;; **
   (:?
    (::
     (:*
      (alternatives
       PN_CHARS
       (char-list/lit ".")))
     PN_CHARS)))))

(define PNAME_LN
  (vac
   (concatenation PNAME_NS PN_LOCAL)) )

(define PNAME_NS
  (concatenation
   (repetition1 
    ;; should be PN_PREFIX
    (alternatives
     char-list/decimal
     char-list/alpha
     (char-list/lit "-")))
   (char-list/lit ":")))

(define IRIREF ;; ** really cheating
  (concatenation
   (char-list/lit "<")
   (repetition1 
    (alternatives ;; should be list-from-string
     char-list/alpha
     char-list/decimal
     (set-from-string "-_&?#.:/+")))
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
    (::
     (drop-consumed (char-list/lit "\""))
     (repetition
      (alternatives
       STRINGCHAR ECHAR))
     (drop-consumed (char-list/lit "\"")))
    (::
     (drop-consumed (char-list/lit "'"))
     (repetition
      (alternatives
       STRINGCHAR ECHAR))
     (drop-consumed (char-list/lit "'"))))))

(define BooleanLiteral
   (alternatives
    (bind (lambda (s) (list #t))
          (lit/sp "true"))
    (bind (lambda (s) (list #f))
          (lit/sp "false"))))

(define NumericLiteralUnsigned
  (alternatives INTEGER DECIMAL)) ;; DOUBLE

(define NumericLiteralPositive
  (alternatives INTEGER_POSITIVE DECIMAL_POSITIVE)) ;; DOUBLE_POSITIVE))

(define	NumericLiteralNegative 
  (alternatives INTEGER_NEGATIVE DECIMAL_NEGATIVE)) ;; DOUBLE_NEGATIVE))

(define NumericLiteral
  (alternatives NumericLiteralUnsigned NumericLiteralPositive NumericLiteralNegative))

(define RDFLiteral
  (alternatives
   (->cons
    (:: 
     String
     (alternatives
      LANGTAG
      (::
       (drop-consumed (char-list/lit "^^"))
       iri))))
   String))

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
   (bracketted-function 
    label
    (->list
     (::
      (:? (lit/sym "DISTINCT")) 
      content)))))

(define Aggregate
  (vac
   (alternatives
    (aggregate-expression
     "COUNT" (alternatives (lit/sym "*")  Expression))
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
   (bracketted-function "IF" (::
                              Expression
                              (drop-consumed (lit/sp ",")) 
                              Expression
                              (drop-consumed (lit/sp ",")) 
                              Expression))
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
   (->alist
    '|@()|
    (::
     (drop-consumed (lit/sp "("))
     Expression
     (drop-consumed (lit/sp ")"))))))

(define PrimaryExpression
  (vac
  (alternatives
   BrackettedExpression
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
  ;;(->polish
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
  ;;(->polish
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

(define RelationalExpression 
  (vac
   ;;(->polish
    (:: 
      NumericExpression
      (:? 
       (alternatives
        (:: (lit/sym "=") NumericExpression)
        (:: (lit/sym "!=") NumericExpression)
        (:: (lit/sym "<") NumericExpression)
        (:: (lit/sym ">") NumericExpression)
        (:: (lit/sym ">=") NumericExpression)
        (:: (lit/sym "<=") NumericExpression)
        (:: (lit/sym "IN") ExpressionList)
        (:: (lit/sym "NOT") (lit/sym "IN") ExpressionList))))))

(define ValueLogical RelationalExpression)

(define ConditionalAndExpression
  (:: ValueLogical (:* (:: (lit/sym "&&") ValueLogical))))

(define ConditionalOrExpression
  (:: ConditionalAndExpression
      (:* (:: (lit/sym "||") ConditionalAndExpression)))) ;; => Polish notation?

(define Expression (->list ConditionalOrExpression))
  
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
   (between-fws 
    (alternatives
     VarOrTerm TriplesNodePath))))

(define GraphNode
  (vac
   (between-fws 
    (alternatives
     VarOrTerm TriplesNode))))

(define CollectionPath
  (vac
   (->alist
    '|@()|
    (:: 
     (drop-consumed (lit/sp "("))
     (:+ GraphNodePath)
     (drop-consumed (lit/sp ")"))))))

(define Collection
  (vac
   (->alist
    '|@()|
    (:: (lit/sp "(")
	(:+ GraphNode)
	(lit/sp ")")))))

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

(define Integer INTEGER)

;; check priorities and grouping for paths,
;;  e.g., ^dc:title*

(define PathOneInPropertySet
  (alternatives
   iri
   (lit/sym "a")
   (->list
    (::
     (bind-consumed->symbol
      (char-list/lit "^"))
     (alternatives
      iri
      (lit/sym "a"))))))

(define PathNegatedPropertySet
  (alternatives
   PathOneInPropertySet
   (->alist
    '|@()| 
    (::
     (drop-consumed (lit/sp "("))
     (alternatives
      (->alist
       '||
       (:: 
        PathOneInPropertySet
        (:+
         (::
          (drop-consumed (lit/sp "|"))
          PathOneInPropertySet))))
      PathOneInPropertySet)
     (drop-consumed (lit/sp ")"))))))

(define PathPrimary
  (vac
    (alternatives
     iri
     (lit/sym "a")
     (->alist
      '!
      (:: 
       (drop-consumed (char-list/lit "!"))
       PathNegatedPropertySet))
     (->alist
      '|@()| 
      (::
       (drop-consumed (lit/sp "("))
       Path
       (drop-consumed (lit/sp ")")))))))

(define PathMod
  (bind-consumed->symbol
   (::
    (set-from-string "?*+")
    (drop-consumed
     (char-list/lit " "))))) ;; necessary to avoid following ?var

(define PathEltOrInverse
  (vac
   (alternatives 
    PathElt 
    (->list
     (::
      (bind-consumed->symbol
       (char-list/lit "^"))
      PathElt)))))

(define PathElt
  ;; (:: PathPrimary (:? PathMod)))
  (alternatives
   (->alist '? (:: PathPrimary (drop-consumed (lit/sp "? "))))
   (->alist '+ (:: PathPrimary (drop-consumed (lit/sp "+ "))))
   (->alist '* (:: PathPrimary (drop-consumed (lit/sp "* "))))
   PathPrimary))

(define PathSequence
  (vac
   (alternatives
    (->alist 
     '/
     (:: 
      PathEltOrInverse
      (drop-consumed (lit/sp "/"))
      PathSequence))
    PathEltOrInverse)))

(define PathAlternative
  (alternatives
   (->alist
    '||
    (:: PathSequence
        (:+
         (::
          (drop-consumed (lit/sp "|"))
          PathSequence))))
   PathSequence))

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
      (:: 
       (drop-consumed (lit/sp ";"))
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
   (->alist
    '|@()|
    (:: (drop-consumed (lit/sp "("))
        Expression
        (:*
         (->alist
          '|@()|
          (::
           (drop-consumed (lit/sp "("))
           Expression
           (drop-consumed (lit/sp ")")))))
        (drop-consumed (lit/sp ")"))))))

(define ArgList
  (alternatives
   NIL
   (->alist
    '|@()|
    (:: (drop-consumed (lit/sp "("))
        (:? (lit/sym "DISTINCT"))
        Expression
        (:*
         (->alist
          '|@()|
          (::
           (drop-consumed (lit/sp "("))
             Expression
             (drop-consumed (lit/sp ")")))))
        (drop-consumed (lit/sp ")"))))))

(define FunctionCall 
  (:: iri ArgList))

(define Constraint
  (alternatives BrackettedExpression BuiltInCall FunctionCall))

(define Filter (->list (:: (lit/sym "FILTER") Constraint)))

(define GroupOrUnionGraphPattern
  (vac
   (alternatives
     (->alist
      'UNION
      (::
       (->list GroupGraphPattern)
       (:+ (::
            (drop-consumed (lit/sp "UNION"))
            (->list GroupGraphPattern)))))
     (->list GroupGraphPattern))))

(define MinusGraphPattern
  (vac
   (:: (lit/sym "MINUS") GroupGraphPattern)))

(define DataBlockValue
  (alternatives iri RDFLiteral NumericLiteral BooleanLiteral (lit/sym "UNDEF")))

(define InlineDataFull
  (::
   (alternatives 
    NIL 
   (->alist
    '|@()|
    (::      
     (drop-consumed (lit/sp "("))
     (:* (between-fws Var))
     (drop-consumed (lit/sp ")")))))
   (::
    (drop-consumed (lit/sp "{"))
    (:*
     (alternatives
      (->alist
       '|@()|
       (::
        (drop-consumed (lit/sp "("))
        (:* (between-fws DataBlockValue))
        (drop-consumed (lit/sp ")"))))
      NIL))
     (drop-consumed (lit/sp "}")))))

(define InlineDataOneVar
  (::
   Var
   (drop-consumed (lit/sp "{"))
   (:* DataBlockValue)
   (drop-consumed (lit/sp "}"))))

(define DataBlock
  (alternatives InlineDataOneVar InlineDataFull))

;; InlineData

;; bug - BIND( ?a + ?b AS ?c) is not parsed into polish notation
(define Bind
  (->list
   (:: 
    (lit/sym "BIND")
    (->alist
     'AS
     (:: 
      (drop-consumed (lit/sp "("))
      Expression
      (drop-consumed (lit/sym "AS"))
      Var
      (drop-consumed (lit/sp ")")))))))

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
                Bind ;; InlineData
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
          (:? (drop-consumed (lit/sp ".")))
          (:? TriplesBlock)))))

(define GroupGraphPattern 
  (vac
    (:: (drop-consumed (lit/sp "{"))
        (alternatives
         SubSelect
         GroupGraphPatternSub)
        (drop-consumed (lit/sp "}")))))

(define TriplesTemplate
  (vac
   (::
    (->list TriplesSameSubject)
    (:? (:: (drop-consumed (lit/sp "."))
            (:? TriplesTemplate))))))
  
(define QuadsNotTriples
  (->list
   (::
    (lit/sym "GRAPH")
    VarOrIri
    (:: (drop-consumed (lit/sp "{"))
        (:? TriplesTemplate)
        (drop-consumed (lit/sp "}"))))))

(define Quads
  (::
   (:? TriplesTemplate)
   (:* (::
        QuadsNotTriples
        (:? (drop-consumed (lit/sp ".")))
        (:? TriplesTemplate)))))

(define QuadData
  ;; (->list
   (:: 
    (drop-consumed (lit/sp "{"))
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
  (->list
   (:: 
    (lit/sym "INSERT")
    QuadData)))

(define DeleteClause
  (->list
   (:: 
    (lit/sym "DELETE")
    QuadData)))

(define Modify
  (:: 
   (:?
    (->alist
     '@Dataset
     (:: (lit/sym "WITH") iri)))
   (alternatives 
    (:: DeleteClause
        (:? InsertClause))
    InsertClause)
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
   (::
    (->list
     (:: 
      (->alist '@Prologue Prologue)
      (:? (->alist '@Update Update1))))
    (:? 
     (:: 
      (drop-consumed (char-list/lit ";"))
      Update)))))

(define ValuesClause
  (:?
   (->list
    (::
     (lit/sym "VALUES")
     DataBlock))))

(define OffsetClause
  (->list
   (::
    (lit/sym "OFFSET")
    (between-fws INTEGER))))

(define LimitClause
  (->list
   (::
    (lit/sym "LIMIT")
    (between-fws INTEGER))))

(define LimitOffsetClauses
  (alternatives
   (::
    LimitClause
    (:? OffsetClause))
   (::
    OffsetClause
    (:? LimitClause))))

(define OrderCondition
  (alternatives
   (::
    (alternatives
     (lit/sym "ASC")
     (lit/sym "DESC"))
    BrackettedExpression)
   (alternatives Constraint Var)))

(define OrderClause
  (->list
   (::
    (bind-consumed->symbol 
     (::
      (lit/sp "ORDER ")
      (lit/sp "BY")))
    (:+ (between-fws OrderCondition)))))
    
;; HavingClause

;; GroupCondition

;; GroupClause

(define SolutionModifier
  (::
   ;; (:? GroupClause)
   ;; (:? HavingClause)
   (:? OrderClause)
   (:? LimitOffsetClauses)))

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
         (->alist
          'AS
          (:: 
           (drop-consumed (lit/sp "("))
           Expression
           (drop-consumed (lit/sym "AS"))
           Var
           (drop-consumed (lit/sp ")")))))))
     (lit/sym "*"))))

(define SubSelect
   (:: (->list SelectClause)
       (->list WhereClause)
       ;; SolutionModifier ValuesClause
       ))

(define SelectQuery
  (:: (->list SelectClause) ;; '@SelectClause 
      (->alist '@Dataset (:* DatasetClause)) ;;'@DatasetClause
      (->list WhereClause) 
      SolutionModifier
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
  (->list
   (concatenation
    (->alist '@Prologue Prologue)
    (->alist '@Query
             (alternatives
              SelectQuery
              ;; ConstructQuery DescribeQuery AskQuery )
              ))
    ValuesClause
    )))

(define QueryUnit
  (->alist '@Unit Query))

(define TopLevel (alternatives QueryUnit UpdateUnit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

;; should also read from ports
(define (parse-query query)
  ;;(cons '@TOP
  (car (lex TopLevel err query)))

(define (read-sparql query)
  ;;(cons '@TOP
  (car (lex TopLevel err query)))

(define (read-triples query)
  ;;(cons '@TOP
  (car (lex GroupGraphPatternSub err query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors

(define (nested-alist-ref alist #!rest keys)
  (nested-alist-ref* keys alist))

(define (nested-alist-ref* keys alist)
  (and keys       
       (if (null? keys)
           alist
           (let ((nested (alist-ref (car keys) alist)))
             (and nested
                  (nested-alist-ref* (cdr keys) nested))))))

(define (nested-alist-update* keys val alist)
  (let ((key (car keys)))
    (if (null? (cdr keys))
        (alist-update key val alist)
        (alist-update 
         key
         (nested-alist-update*
          (cdr keys) val (or (alist-ref key alist) '()))
          alist))))

(define (nested-alist-update alist val #!rest keys)
  (nested-alist-update* keys val alist))

(define (nested-alist-replace* keys proc alist)
  (nested-alist-update* keys (proc (nested-alist-ref* keys alist)) alist))

(define (nested-alist-replace alist proc #!rest keys)
  (nested-alist-replace* keys proc alist))

(define (assoc-when sym alist)
  (and alist
      (assoc sym alist)))

(define (unit-prologue QueryUnit)
  ;;(nested-alist-ref QueryUnit '@Unit '@Prologue))
  (join
   (map (lambda (unit)
          (alist-ref '@Prologue unit))
        (alist-ref '@Unit QueryUnit))))

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

)
