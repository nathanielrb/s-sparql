;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

;; Representation Questions 
;; [ ]  cdr or cadr for list (arglist vs SELECT): (SELECT DISTINCT ?a ?b) vs <iri> (DISTINCT ?a ?b)
;; [ ] X  (|X DISTINCT| ..) or (X (DISTINCT ..)) ? same for NAMED, SILENT (be consistent)
;; - numbers > numbers? record? symbol
;; - RdfLiteral - cons pair?
;; - @blank or record
;; - triples ... list or record?

;; graphordefault - what usage/list?

;; - combine (->node-if (->node into single bind function (?)

(use sparql-query
     srfi-13 srfi-69 http-client intarweb uri-common medea cjson matchable irregex)

(require-extension utf8 utf8-srfi-14
                   typeclass input-classes abnf abnf-charlist abnf-consumers
                   lexgen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boilerplate
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backtracking
(define (bstar p)
  (lambda (sk fk strm)
    (let ((try
           (lambda (s)
             (let ((ss (sk s)))		      
               (if (equal? ss '(error)) #f
                   ss)))))
      (p (lambda (strm1)
           (or 
            ((bstar p) try try strm1)
            (sk strm)))
         sk
         strm))))

(define (sandbox p)
  (lambda (sk fk strm)
    (let ((s (p values err strm)))
      (if (equal? s '(error))
          (fk strm)
          (sk s)))))

;; (define (bbar p1 p2)
;;   (lambda (sk fk strm)
;;     (p1 sk (lambda (s)
;;              (let ((ss (p1 sk fk strm)))
;;                (if (equal? ss '(error)) 
;;                    (p2 sk fk strm)
;;                    ss)))
;;         strm)))

;; (define balternatives bbar)

;; (define (bopt pat) (bbar pat pass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binding functions
;; To be cleaned up and abstracted
(define consumed-values->list
  (consumed-objects-lift consumed-values))

(define-syntax ->list
  (syntax-rules () 
    ((_ l p)    (bind (consumed-values->list l)  p))
    ((_ p)      (bind (consumed-values->list)  p))
    ))

(define-syntax ->node
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
    ))

;; list if >1 children (abstract this & next 2)
;; but this often requires (->list-if (->list (concatenation ...))) which is redundant.
;; it should combine both behaviors.
(define (list-if lst)
  (and (pair? lst)
       (pair? (car lst))
       (let ((lst (car lst)))
         (if (> (length lst) 1)
             lst
             (car lst)))))

(define consumed-values->list-if
  (consumed-pairs->list list-if))

(define-syntax ->list-if
  (syntax-rules () 
    ((_ p)    (bind consumed-values->list-if  p))
    ))

;; node if >1 children

(define (node-if lst)
  (and (pair? lst)
       (pair? (car lst))
       (let ((lst (car lst)))
         (if (> (length lst) 2)
             lst
             (second lst)))))

(define consumed-values->node-if
  (consumed-pairs->list node-if))

(define-syntax ->node-if
  (syntax-rules () 
    ((_ p)    (bind consumed-values->node-if  p))
    ))

;; infix

(define (infix-if lst)
  (and (pair? lst)
       (pair? (car lst))
       (let ((lst (car lst)))
         (cond ((= (length lst) 3)
                (list (second lst) (first lst) (third lst)))
               ((= (length lst) 2)
                (list (second lst) (first lst)))
               (else (car lst))))))

(define consumed-values->infix-if
  (consumed-pairs->list infix-if))

(define-syntax ->infix-if
  (syntax-rules () 
    ((_ p)    (bind consumed-values->infix-if  p))
    ))

;; cons

(define (2list->cons vals)
  (and (list? vals)
         (= (length vals) 2)
         (cons (car vals) (cadr vals))))

(define-syntax ->cons
  (syntax-rules () 
    ((_ p)    (bind (consumed-values->list 2list->cons) p))
    ))

(define consumed-chars->number
  (consumed-chars->list 
   (compose string->symbol list->string)))

(define-syntax ->number
  (syntax-rules () 
    ((_ p)    (bind consumed-chars->number p))
    ))

(define (number->negative l)
  (and (number? (car l)) (- (car l))))

(define consumed-numbers (consumed-objects number?))

(define consumed-values->negative
  ((consumed-objects-lift consumed-numbers) number->negative))   

(define-syntax ->negative
  (syntax-rules ()
    ((_ p) (bind consumed-values->negative p))))

;; polish arithmetic

(define (polish-arithmetic stream)
  (let rec ((ops  '(/ * - +)) (stream stream))
    (if (null? ops) 
        (if (equal? (length stream) 1)
            (car stream)
            (abort "Polish error"))
        (rec (cdr ops)
             (let loop ((stream stream))
               (if (or (null? stream) (null? (cdr stream)))
                   stream
                   (match stream
                     ((a op b . rest)
                      (if (equal? op (car ops))
                          (loop
                           `((,(car ops) ,a ,b) ,@rest))
                          `(,a ,op
                               ,@(loop (cddr stream))))))))))))

(define (polish lst)        
  (and (pair? lst)
       (pair? (car lst))
       (let ((lst (car lst)))
         (if (= (length lst) 1)
             (car lst)
             (polish-arithmetic lst)))))

(define consumed-values->polish
  (consumed-pairs->list polish))

(define-syntax ->polish
  (syntax-rules () 
    ((_ p)    (bind consumed-values->polish  p))
    ))

;; whitespace

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
   p
   (optional-sequence (drop-consumed fws)) ))

(define (err s)
  ;; (print "lexical error on stream: " s)
  ;; `(error . ,s))
  `(error))

(define (lit/sp str)
  (between-fws (char-list/lit str)))

(define (lit/sym str)
  (bind-consumed->symbol
   (between-fws (char-list/lit str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminals
(define PN_LOCAL_ESC
  (concatenation
   (char-list/lit "\\")
   (set-from-string "_~.-!$&'()*+,;=/?#@%")))

(define HEX hexadecimal)

;; how should this be represented?
(define PERCENT
  (concatenation
   (char-list/lit "%") HEX HEX))

(define PLX
  (alternatives PERCENT PN_LOCAL_ESC))

(define PN_LOCAL
  (vac
   (concatenation
    (alternatives 
     PN_CHARS_U
     (char-list/lit ":")
     char-list/decimal
     PLX)
    (optional-sequence
     (sandbox
      (concatenation
       (bstar
        (alternatives 
         (set-from-string ":.") 
         PN_CHARS PLX))
       (alternatives 
        PN_CHARS
        (char-list/lit ":") 
        PLX)))))))

(define PN_PREFIX
  (vac
   (concatenation
    PN_CHARS_BASE
    (optional-sequence
     (sandbox
      (concatenation
       (bstar
        (alternatives
         (char-list/lit ".") 
         PN_CHARS)) 
       PN_CHARS))))))

(define PN_CHARS
  (vac
   (alternatives
    PN_CHARS_U
    (set-from-string "-")
    char-list/decimal
    (char-list/lit "·")
    (set
     (char-set-union
      (ucs-range->char-set #x0300 #x036F)
      (ucs-range->char-set #x203F #x2040))))))

(define VARNAME
  (vac
   (concatenation
    (alternatives
     PN_CHARS_U
     char-list/decimal)
    (repetition
     (alternatives
      PN_CHARS_U
      char-list/decimal
      (set
       (char-set-union
        (ucs-range->char-set #x00B7 #x00B8)
        (ucs-range->char-set #x0300 #x036F)
        (ucs-range->char-set #x203F #x2040))))))))

(define PN_CHARS_U
  (vac
   (alternatives
    PN_CHARS_BASE
    (set-from-string "_"))))

(define PN_CHARS_BASE
  (alternatives
   char-list/alpha
   (set
    (char-set-union
     (ucs-range->char-set #x00C0 #x00D6)
     (ucs-range->char-set #x00D8 #x00F6)
     (ucs-range->char-set #x00F8 #x02FF)
     (ucs-range->char-set #x0370 #x037D)
     (ucs-range->char-set #x037F #x1FFF)
     (ucs-range->char-set #x200C #x200D)
     (ucs-range->char-set #x2070 #x218F)
     (ucs-range->char-set #x2C00 #x2FEF)
     (ucs-range->char-set #x3001 #xD7FF)
     (ucs-range->char-set #xF900 #xFDCF)
     (ucs-range->char-set #xFDF0 #xFFFD)
     (ucs-range->char-set #x10000 #xEFFFF)))))

(define ANON
  (->node
   '@Blank
    (concatenation (drop-consumed (lit/sp "["))
        (drop-consumed (optional-sequence fws))
        (drop-consumed (lit/sp "]")))))

(define WS
  (set
   (list->char-set
    (list #\space #\tab #\newline #\return))))

(define NIL
  (->node
   '@NIL
   (concatenation 
    (drop-consumed (lit/sp "("))
    (drop-consumed (lit/sp ")")))))

(define ECHAR
  (concatenation (char-list/lit "\\")
      (set-from-string "tbnrf\\\"'")))

(define STRING_LITERAL_LONG2
  (concatenation
   (char-list/lit "\"\"\"")
   (repetition
    (concatenation
     (optional-sequence
      (alternatives 
       (char-list/lit "\"")
       (char-list/lit "\"\"")))
     (alternatives
      (set
       (char-set-complement
        (string->char-set "\"\\")))
      ECHAR)))
   (char-list/lit "\"\"\"")))

(define STRING_LITERAL_LONG1
  (concatenation
   (char-list/lit "'''")
   (repetition
    (concatenation
     (optional-sequence
      (alternatives 
       (char-list/lit "'")
       (char-list/lit "''")))
     (alternatives
      (set 
       (char-set-complement
        (string->char-set "\"\\")))
      ECHAR)))
   (lit/sp "'''")))

(define STRING_LITERAL2
  (concatenation
   (drop-consumed (char-list/lit "\""))
   (repetition
    (alternatives
     (set
      (char-set-complement
       (list->char-set
        (list #\" #\\ #\newline #\return))))
     ECHAR))
   (drop-consumed (char-list/lit "\""))))

(define STRING_LITERAL1
  (concatenation
   (char-list/lit "'")
   (repetition
    (alternatives
     (set 
      (char-set-complement
       (list->char-set
        (list #\' #\\ #\newline #\return))))
     ECHAR))
   (char-list/lit "'")))

(define EXPONENT
  (concatenation
   (set-from-string "eE")
   (optional-sequence (set-from-string "+-"))
   (repetition1 decimal)))

(define DOUBLE_NEGATIVE
  (vac
   (concatenation
    (drop-consumed (char-list/lit "-"))
    (->negative DOUBLE))))

(define DECIMAL_NEGATIVE
  (vac
   (concatenation
    (drop-consumed (char-list/lit "-"))
    (->negative DECIMAL))))

(define INTEGER_NEGATIVE
  (vac
   (concatenation
    (drop-consumed (char-list/lit "-") )
    (->negative INTEGER))))

(define DOUBLE_POSITIVE
  (vac
   (concatenation
    (drop-consumed (char-list/lit "+") )
    DOUBLE)))

(define DECIMAL_POSITIVE
  (vac
   (concatenation (lit/sym "+") DECIMAL)))

(define INTEGER_POSITIVE
  (vac
   (concatenation (lit/sym "+") INTEGER)))

(define DOUBLE 
  (->number
   (alternatives
    (concatenation
     (repetition1 decimal)
     (char-list/lit ".")
     (repetition decimal)
     EXPONENT)
    (concatenation
     (char-list/lit ".")
     (repetition1 decimal)
     EXPONENT)
    (concatenation
     (repetition1 decimal)
     EXPONENT))))

(define DECIMAL
  (->number
   (concatenation 
    (repetition char-list/decimal)
    (char-list/lit ".")
    (repetition1 char-list/decimal))))

(define INTEGER (->number (repetition1 char-list/decimal)))

(define LANGTAG 
  (bind-consumed->symbol
   (concatenation    
     (char-list/lit "@")
     (repetition1 char-list/alpha)
     (repetition
      (concatenation 
       (char-list/lit "-")
       (repetition1 char-list/alpha))))))

(define VAR2
  (concatenation
   (char-list/lit "$")
   VARNAME))

(define VAR1
  (concatenation
   (char-list/lit "?")
   VARNAME))

(define BLANK_NODE_LABEL
  (vac
   (concatenation
   (char-list/lit "_:")
   (alternatives
    PN_CHARS_U
    char-list/decimal)
   (optional-sequence
    (sandbox
     (seq
      (bstar 
       (alternatives 
        (char-list/lit ".") 
        PN_CHARS))
      PN_CHARS))))))

(define PNAME_LN
  (vac
   (concatenation PNAME_NS PN_LOCAL)) )

(define PNAME_NS
  (concatenation 
   (optional-sequence PN_PREFIX)
   (char-list/lit ":")))

(define IRIREF 
  (concatenation
   (char-list/lit "<")
   (repetition
    (set
     (char-set-difference
      (char-set-complement
       (list->char-set (list #\< #\> #\{ #\} #\| #\^ #\` #\\)))
      (ucs-range->char-set #x00 #x20))))
   (char-list/lit ">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar
(define BlankNode
  (between-fws 
   (alternatives 
    BLANK_NODE_LABEL
    ANON)))

(define PrefixedName
  (bind-consumed->symbol
   (between-fws
    (alternatives PNAME_LN PNAME_NS))))

(define iri
  (alternatives 
   (bind-consumed->symbol (between-fws IRIREF))
   PrefixedName))

(define String
  (bind-consumed->string
   (between-fws 
    (alternatives
     STRING_LITERAL1 STRING_LITERAL2 STRING_LITERAL_LONG1 STRING_LITERAL_LONG2))))

(define BooleanLiteral
   (alternatives
    (bind (lambda (s) (list #t))
          (lit/sp "true"))
    (bind (lambda (s) (list #f))
          (lit/sp "false"))))

(define NumericLiteralUnsigned
  (between-fws 
   (alternatives
    INTEGER DECIMAL DOUBLE)))

(define NumericLiteralPositive
  (between-fws 
   (alternatives
    INTEGER_POSITIVE DECIMAL_POSITIVE DOUBLE_POSITIVE)))

(define	NumericLiteralNegative 
  (between-fws 
   (alternatives
    INTEGER_NEGATIVE DECIMAL_NEGATIVE DOUBLE_NEGATIVE)))

(define NumericLiteral
  (alternatives
   NumericLiteralUnsigned NumericLiteralPositive NumericLiteralNegative))

(define RDFLiteral
  (bind 
   (consumed-values->list 
    (lambda (c) 
      (if (equal? (length c) 1) (car c)
          (cons (car c) (cadr c)))))
    (concatenation 
     String
     (optional-sequence
      (alternatives
       LANGTAG
       (concatenation
        (drop-consumed (char-list/lit "^^"))
        iri))))))

(define iriOrFunction
  (vac
   (concatenation iri (optional-sequence ArgList))))

(define (bracketted-function label content)
  (vac
   (->list 
    (concatenation
     (lit/sym label)
     (drop-consumed (lit/sp "("))
     content
     (drop-consumed (lit/sp ")"))))))

(define (aggregate-expression label content)
  (vac
   (bracketted-function 
    label
    (alternatives
     (->list
      (concatenation
       (lit/sym "DISTINCT")
       content))
     content))))

(define Aggregate
  (vac
   (alternatives
    (aggregate-expression 
     "COUNT" 
     (alternatives 
      (lit/sym "*")  
      Expression))
    (aggregate-expression "SUM" Expression)
    (aggregate-expression "MIN" Expression)
    (aggregate-expression "MAX" Expression)
    (aggregate-expression "AVG" Expression)
    (aggregate-expression "SAMPLE" Expression)
    (bracketted-function 
     "GROUP_CONCAT"
     (concatenation
      (alternatives
       (->list
        (concatenation
         (lit/sym "DISTINCT")
         Expression))
       Expression)
      (optional-sequence
       (->list
        (concatenation 
         (drop-consumed (lit/sp ";"))
         (lit/sym "SEPARATOR")
         (drop-consumed (lit/sym "="))
         String))))))))

(define NotExistsFunc
  (vac
  (->list
   (concatenation
    (bind-consumed->symbol
     (concatenation 
      (lit/sp "NOT ")
      (lit/sp "EXISTS")))
    GroupGraphPattern))))

(define ExistsFunc
  (vac
   (concatenation
    (lit/sym "EXISTS") 
    GroupGraphPattern)))

(define (bracketted-call label N #!optional last-optional?)
  (bracketted-function
   label
   (vac
    (let rec ((n N))
      (cond ((= n 0) pass)
            ((= n N) (concatenation Expression (rec (- n 1))))
            ((and last-optional? (= n 1))
             (optional-sequence (concatenation (drop-consumed (lit/sp ",")) 
                     Expression (rec (- n 1)))))
            (else (concatenation (drop-consumed (lit/sp ",")) 
                      Expression (rec (- n 1)))))))))

(define StrReplaceExpression (bracketted-call "REPLACE"  4 #t))

(define SubstringExpression (bracketted-call "SUBSTR" 3 #t))

(define RegexExpression (bracketted-call "REGEX" 3 #t))

(define BuiltInCall
  (vac
   (alternatives
    Aggregate
    (bracketted-call "STR" 1)
    (bracketted-call "LANG" 1)
    (bracketted-call "LANDMATCHES" 2)
    (bracketted-call "DATATYPE" 1)
    (bracketted-call "BOUND" 1)
    (bracketted-call "IRI" 1)
    (bracketted-call "URI" 1)
    (bracketted-call "BNODE" 1)
    (bracketted-call "RAND" 0)
    (bracketted-call "ABS" 1)
    (bracketted-call "CEIL" 1)
    (bracketted-call "FLOOR" 1)
    (bracketted-call "ROUND" 1)
    (bracketted-call "CONCAT" 1)
    SubstringExpression
    (bracketted-call "STRLEN" 1)
    StrReplaceExpression
    (bracketted-call "UCASE" 1)
    (bracketted-call "LCASE" 1)
    (bracketted-call "ENCODE_FOR_URI" 1)
    (bracketted-call "CONTAINS" 2)
    (bracketted-call "STRSTARTS" 2)
    (bracketted-call "STRENDS" 2)   
    (bracketted-call "STRBEFORE" 2)
    (bracketted-call "STRAFTER" 2)
    (bracketted-call "YEAR" 1)
    (bracketted-call "MONTH" 1)
    (bracketted-call "DAY" 1)
    (bracketted-call "HOURS" 1)
    (bracketted-call "MINUTES" 1)
    (bracketted-call "SECONDS" 1)
    (bracketted-call "TIMEZONE" 1)
    (bracketted-call "TZ" 1)
    (concatenation (lit/sym "NOW") NIL)
    (concatenation (lit/sym "UUID") NIL)
    (concatenation (lit/sym "STRUUID") NIL)
    (bracketted-call "MD5" 1)
    (bracketted-call "SHA1" 1)
    (bracketted-call "SHA256" 1)
    (bracketted-call "SHA384" 1)
    (bracketted-call "SHA512" 1)
    (concatenation (lit/sym "COALESCE") ExpressionList)
    (bracketted-call "IF" 3)
    (bracketted-call "STRLANG" 2)
    (bracketted-call "STRDT" 2)
    (bracketted-call "sameTerm" 2)
    (bracketted-call "isIRI" 1)
    (bracketted-call "isURI" 1)
    (bracketted-call "isBLANK" 1)
    (bracketted-call "isLITERAL" 1)
    (bracketted-call "isNUMERIC" 1)
    RegexExpression
    ExistsFunc
    NotExistsFunc)))

(define BrackettedExpression
  (vac
   (concatenation
    (drop-consumed (lit/sp "("))
    Expression
    (drop-consumed (lit/sp ")")))))

(define PrimaryExpression
  (vac
  (alternatives
   NumericLiteral
   BooleanLiteral
   Var
   RDFLiteral
   BrackettedExpression
   BuiltInCall
   iriOrFunction 
   )))

(define UnaryExpression
  (vac 
   (->list-if
    (->list
     (alternatives
      (concatenation (lit/sym "!") PrimaryExpression)
      (concatenation (lit/sym "+") PrimaryExpression)
      (concatenation (lit/sym "-") PrimaryExpression)
      PrimaryExpression)))))

(define MultiplicativeExpression
  (->polish
    (->list
     (concatenation
      UnaryExpression
      (repetition
       (alternatives
        (concatenation (lit/sym "*")
            UnaryExpression)
        (concatenation (lit/sym "/")
            UnaryExpression)))))))

(define AdditiveExpression
  (->polish
    (->list
     (concatenation
      MultiplicativeExpression
      (repetition
       (alternatives
        (concatenation (lit/sym "+") MultiplicativeExpression)
        (concatenation (lit/sym "-") MultiplicativeExpression)))))))

(define NumericExpression
  (vac AdditiveExpression))

(define RelationalExpression 
  (vac
   (->infix-if
    (->list
     (concatenation 
      NumericExpression
      (optional-sequence 
       (alternatives
        (concatenation (lit/sym "=") NumericExpression)
        (concatenation (lit/sym "!=") NumericExpression)
        (concatenation (lit/sym "<") NumericExpression)
        (concatenation (lit/sym ">") NumericExpression)
        (concatenation (lit/sym ">=") NumericExpression)
        (concatenation (lit/sym "<=") NumericExpression)
        (concatenation (lit/sym "IN") ExpressionList)
        (concatenation 
         (bind-consumed->symbol
          (concatenation
           (lit/sp "NOT ") (lit/sp "IN")))
         ExpressionList))))))))

(define ValueLogical RelationalExpression)

(define ConditionalAndExpression
  (vac 
   (->node-if
    (->node
     '&&
     (concatenation 
      ValueLogical
      (repetition
       (concatenation
        (drop-consumed (lit/sp "&&"))
        ValueLogical)))))))

(define ConditionalOrExpression
  (->node-if
   (->node
    '||
    (concatenation 
     ConditionalAndExpression
     (repetition
      (concatenation
       (drop-consumed (lit/sp "||"))
       ConditionalAndExpression))))))

(define Expression
  (vac 
   ConditionalOrExpression))
  
(define GraphTerm
   (alternatives
    iri RDFLiteral NumericLiteral BooleanLiteral BlankNode NIL))

(define Var
  (bind-consumed->symbol
   (between-fws
    (alternatives VAR1 VAR2))))

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
   (->list
    (concatenation 
     (drop-consumed (lit/sp "("))
     (repetition1 GraphNodePath)
     (drop-consumed (lit/sp ")"))))))

(define Collection
  (vac
   (->list
    (concatenation 
     (lit/sp "(")
     (repetition1 GraphNode)
     (lit/sp ")")))))

(define BlankNodePropertyListPath
  (vac
   (->node
    '@Blank
    (concatenation 
     (drop-consumed (lit/sym "["))
     PropertyListPathNotEmpty
     (drop-consumed (lit/sym "]"))))))

(define TriplesNodePath
  (alternatives CollectionPath BlankNodePropertyListPath))

(define BlankNodePropertyList
  (vac
   (->node
    '@Blank
    (concatenation 
     (drop-consumed (lit/sym "["))
     PropertyListNotEmpty
     (drop-consumed (lit/sym "]"))))))

(define TriplesNode
   (alternatives Collection BlankNodePropertyList))

(define Integer INTEGER)

(define PathOneInPropertySet
  (alternatives
   iri
   (lit/sym "a")
   (->list
    (concatenation
     (lit/sp "^")
     (alternatives
      iri
      (lit/sym "a"))))))

(define PathNegatedPropertySet
  (alternatives
   PathOneInPropertySet
   (->list
    (concatenation
     (drop-consumed (lit/sp "("))
     (->node-if
      (->node
       '||
       (concatenation 
        PathOneInPropertySet
        (repetition
         (concatenation
          (drop-consumed (lit/sp "|"))
          PathOneInPropertySet)))))
     (drop-consumed (lit/sp ")"))))))

(define PathPrimary
  (vac
    (alternatives
     iri
     (lit/sym "a")
     (->node
      '!
      (concatenation 
       (drop-consumed (char-list/lit "!"))
       PathNegatedPropertySet))
     (->list
      (concatenation
       (drop-consumed (lit/sp "("))
       Path
       (drop-consumed (lit/sp ")")))))))

(define PathMod
  (bind-consumed->symbol
   (concatenation
    (set-from-string "?*+")
    (drop-consumed (char-list/lit " "))))) ; avoid matching following ?var

(define PathEltOrInverse
  (vac
   (alternatives 
    PathElt 
    (->list
     (concatenation
      (lit/sym "^")
      PathElt)))))

(define PathElt
  (->infix-if
   (->list
    (concatenation
     PathPrimary
     (optional-sequence PathMod)))))

(define PathSequence
  (vac
   (->node-if
    (->node 
     '/
     (concatenation 
      PathEltOrInverse
      (repetition
       (concatenation
        (drop-consumed (lit/sp "/"))
        PathEltOrInverse)))))))

(define PathAlternative
  (->node-if
   (->node
    '||
    (concatenation 
     PathSequence
     (repetition
      (concatenation
       (drop-consumed (lit/sp "|"))
       PathSequence))))))

(define Path PathAlternative)

(define ObjectPath GraphNodePath)

(define	ObjectListPath
  (->list-if
   (->list
    (concatenation 
     ObjectPath
     (repetition
      (concatenation
       (drop-consumed (lit/sp ","))
       ObjectPath))))))

(define VerbSimple Var)

(define VerbPath Path)

(define PropertyListPathNotEmpty
  (vac
   (->list
    (concatenation
     (->list
      (concatenation
       (alternatives VerbPath VerbSimple)
       ObjectListPath))
     (repetition
      (concatenation 
       (drop-consumed (lit/sp ";"))
       (optional-sequence
        (->list
         (concatenation
          (alternatives VerbPath VerbSimple)
          ObjectList)))))))))

(define PropertyListPath
  (optional-sequence PropertyListPathNotEmpty))

(define TriplesSameSubjectPath
  (alternatives
   (concatenation 
    VarOrTerm
    PropertyListPathNotEmpty)
   (concatenation
    TriplesNodePath
    PropertyListPath)))

(define Object GraphNode)

(define ObjectList
  (->list-if
   (->list
    (concatenation
     Object
     (repetition
      (concatenation
       (drop-consumed (lit/sp ","))
       Object))))))

(define Verb (alternatives VarOrIri (lit/sym "a")))

(define PropertyListNotEmpty
  (->list
   (concatenation
    (->list 
     (concatenation
      Verb
      ObjectList))
    (repetition
     (concatenation
      (drop-consumed (lit/sp ";"))
      (optional-sequence
       (->list
        (concatenation
          Verb
          ObjectList))))))))

(define PropertyList (optional-sequence PropertyListNotEmpty))

(define TriplesSameSubject
  (alternatives
   (concatenation 
    VarOrTerm
    PropertyListNotEmpty)
   (concatenation
    TriplesNode
    PropertyList)))

(define ConstructTriples
  (vac
   (concatenation
    (->list TriplesSameSubject)
    (optional-sequence
     (concatenation
      (drop-consumed (lit/sp "."))
      (optional-sequence ConstructTriples))))))

(define ConstructTemplate
  (concatenation
   (drop-consumed (lit/sp "{"))
   (optional-sequence ConstructTriples)
   (drop-consumed (lit/sp "}"))))

(define ExpressionList
  (alternatives
   NIL
   (->list
    (concatenation
     (drop-consumed (lit/sp "("))
     Expression
     (repetition
      (concatenation
       (drop-consumed (lit/sp ","))
       Expression))
     (drop-consumed (lit/sp ")"))))))

(define ArgList
  (alternatives
   NIL
   (->list
    (concatenation 
     (drop-consumed (lit/sp "("))
     (optional-sequence (lit/sym "DISTINCT"))
     (concatenation
      Expression
      (repetition
       (concatenation
        (drop-consumed (lit/sp ","))
        Expression)))
     (drop-consumed (lit/sp ")"))))))

(define FunctionCall 
  (concatenation iri ArgList))

(define Constraint
  (alternatives BrackettedExpression BuiltInCall FunctionCall))

(define Filter 
  (->list (concatenation (lit/sym "FILTER") Constraint)))

(define GroupOrUnionGraphPattern
  (vac
   (->node-if
    (->node
     'UNION
     (concatenation
      (->list GroupGraphPattern)
      (repetition 
       (concatenation
        (drop-consumed (lit/sp "UNION"))
        (->list GroupGraphPattern))))))))

(define MinusGraphPattern
  (vac
   (concatenation (lit/sym "MINUS") GroupGraphPattern)))

(define DataBlockValue
  (alternatives iri RDFLiteral NumericLiteral BooleanLiteral (lit/sym "UNDEF")))

(define InlineDataFull
  (concatenation
   (alternatives 
    NIL 
    (->list
     (concatenation      
      (drop-consumed (lit/sp "("))
      (repetition Var)
      (drop-consumed (lit/sp ")")))))
   (concatenation
    (drop-consumed (lit/sp "{"))
    (repetition
     (alternatives
      (->list
       (concatenation
        (drop-consumed (lit/sp "("))
        (repetition  DataBlockValue)
        (drop-consumed (lit/sp ")"))))
      NIL))
    (drop-consumed (lit/sp "}")))))

(define InlineDataOneVar
  (concatenation
   Var
   (drop-consumed (lit/sp "{"))
   (repetition DataBlockValue)
   (drop-consumed (lit/sp "}"))))

(define DataBlock
  (alternatives InlineDataOneVar InlineDataFull))

(define InlineData
  (->list
   (concatenation
    (lit/sym "VALUES")
    DataBlock)))

(define Bind
  (->list
   (concatenation 
    (lit/sym "BIND")
    (->infix-if
     (->list
      (concatenation 
       (drop-consumed (lit/sp "("))
       Expression
       (lit/sym "AS")
       Var
       (drop-consumed (lit/sp ")"))))))))

(define ServiceGraphPattern
  (vac
   (->list
    (concatenation
     (alternatives
      (bind-consumed->symbol
       (concatenation 
        (lit/sp "SERVICE ") 
        (lit/sp "SILENT")))
      (lit/sym "SERVICE"))
     VarOrIri
     GroupGraphPattern))))

(define GraphGraphPattern
  (vac
   (->list
    (concatenation
     (lit/sym "GRAPH")
     VarOrIri
     GroupGraphPattern))))

(define OptionalGraphPattern
  (vac
   (->list
    (concatenation 
     (lit/sym "OPTIONAL")
     GroupGraphPattern))))

(define GraphPatternNotTriples
  (alternatives 
   GroupOrUnionGraphPattern
   OptionalGraphPattern
   MinusGraphPattern
   GraphGraphPattern
   ServiceGraphPattern
   Filter
   Bind 
   InlineData))

(define TriplesBlock
  (vac
   (concatenation
    (->list
     TriplesSameSubjectPath)
    (optional-sequence
     (concatenation
      (drop-consumed (lit/sp "."))
      (optional-sequence TriplesBlock))))))

(define GroupGraphPatternSub
  (concatenation
   (optional-sequence TriplesBlock)
   (repetition
    (concatenation
     GraphPatternNotTriples
     (optional-sequence (drop-consumed (lit/sp ".")))
     (optional-sequence TriplesBlock)))))

(define GroupGraphPattern 
  (vac
   (concatenation
    (drop-consumed (lit/sp "{"))
    (alternatives
     SubSelect
     GroupGraphPatternSub)
    (drop-consumed (lit/sp "}")))))

(define TriplesTemplate
  (vac
   (concatenation
    (->list TriplesSameSubject)
    (optional-sequence 
     (concatenation
      (drop-consumed (lit/sp "."))
      (optional-sequence TriplesTemplate))))))
  
(define QuadsNotTriples
  (->list
   (concatenation
    (lit/sym "GRAPH")
    VarOrIri
    (concatenation 
     (drop-consumed (lit/sp "{"))
     (optional-sequence TriplesTemplate)
     (drop-consumed (lit/sp "}"))))))

(define Quads
  (concatenation
   (optional-sequence TriplesTemplate)
   (repetition 
    (concatenation
     QuadsNotTriples
     (optional-sequence (drop-consumed (lit/sp ".")))
     (optional-sequence TriplesTemplate)))))

(define QuadData
  (concatenation 
   (drop-consumed (lit/sp "{"))
   Quads
   (drop-consumed (lit/sp "}"))))

(define QuadPattern
  (concatenation 
   (drop-consumed (lit/sp "{"))
   Quads
   (drop-consumed (lit/sp "}"))))

(define GraphRefAll
  (vac
   (alternatives
    GraphRef
    (lit/sym "DEFAULT")
    (lit/sym "NAMED")
    (lit/sym "ALL"))))

(define GraphRef
  (->list
   (concatenation
    (lit/sym "GRAPH")
    iri)))

(define GraphOrDefault
  (alternatives
   (lit/sym "DEFAULT")
   (->list
    (concatenation
     (optional-sequence (lit/sym "GRAPH"))
     iri))))

(define UsingClause
  (->list
   (concatenation 
    (lit/sym "USING ")
    (alternatives
     (->list
      (concatenation
       (lit/sym "NAMED")
       iri))
     iri))))
  
(define InsertClause
  (->list
   (concatenation 
    (lit/sym "INSERT")
    QuadPattern)))

(define DeleteClause
  (->list
   (concatenation 
    (lit/sym "DELETE")
    QuadPattern)))

(define Modify
  (concatenation 
   (optional-sequence
    (->node
     '@Dataset
     (->list (concatenation (lit/sym "WITH") iri))))
   (alternatives 
    (concatenation DeleteClause
        (optional-sequence InsertClause))
    InsertClause)
   (->node '@Using (repetition UsingClause))
   (->list
    (concatenation
     (lit/sym "WHERE")
     GroupGraphPattern))))

(define DeleteWhere
  (->list
   (concatenation 
    (bind-consumed->symbol
     (concatenation
      (lit/sp "DELETE ")
      (lit/sp "WHERE")))
    QuadData)))

(define DeleteData 
  (->list
   (concatenation 
    (bind-consumed->symbol
     (concatenation
      (lit/sp "DELETE ")
      (lit/sp "DATA")))
    QuadData)))

(define InsertData
  (->list
   (concatenation 
    (bind-consumed->symbol
     (concatenation
      (lit/sp "INSERT ")
      (lit/sp "DATA")))
    QuadData)))

(define Copy
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation 
       (lit/sp "COPY ")
       (lit/sp "SILENT")))
     (lit/sym "COPY"))
    GraphOrDefault
    (lit/sym "TO")
    GraphOrDefault)))

(define Move
  (concatenation
   (alternatives
    (bind-consumed->symbol
     (concatenation
      (lit/sp "MOVE ")
      (lit/sp "SILENT")))
    (lit/sym "MOVE"))
   GraphOrDefault
   (lit/sym "TO")
   GraphOrDefault))

(define Add
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation 
       (lit/sp "ADD ")
       (lit/sp "SILENT")))
     (lit/sym "ADD"))
    GraphOrDefault
    (lit/sym "TO")
    GraphOrDefault)))

(define Create
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation
       (lit/sp "CREATE ")
       (lit/sp "SILENT")))
     (lit/sym "CREATE"))
    GraphRef)))

(define Drop
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation
       (lit/sp "DROP ")
       (lit/sp "SILENT")))
     (lit/sym "DROP"))
    GraphRefAll)))

(define Clear
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation
       (lit/sp "CLEAR ")
       (lit/sp "SILENT")))
     (lit/sym "CLEAR"))
    GraphRefAll)))

(define Load
  (->list
   (concatenation
    (alternatives
     (bind-consumed->symbol
      (concatenation
       (lit/sp "LOAD ")
       (lit/sp "SILENT")))
     (lit/sym "LOAD"))
    iri
    (lit/sym "INTO")
    GraphRef)))

(define Update1
  (alternatives Load Clear Drop Add Move Copy Create 
   InsertData DeleteData DeleteWhere Modify))

(define Update
  (vac
   (concatenation
    (->node
     '@Update
     (concatenation 
      (->node '@Prologue Prologue)
      Update1))
    (optional-sequence
     (concatenation 
      (drop-consumed (lit/sp ";"))
      Update)))))

(define ValuesClause
  (optional-sequence
   (->list
    (concatenation
     (lit/sym "VALUES")
     DataBlock))))

(define OffsetClause
  (->list
   (concatenation
    (lit/sym "OFFSET")
    (between-fws INTEGER))))

(define LimitClause
  (->list
   (concatenation
    (lit/sym "LIMIT")
    (between-fws INTEGER))))

(define LimitOffsetClauses
  (alternatives
   (concatenation
    LimitClause
    (optional-sequence OffsetClause))
   (concatenation
    OffsetClause
    (optional-sequence LimitClause))))

(define OrderCondition
  (alternatives
   (concatenation
    (alternatives
     (lit/sym "ASC")
     (lit/sym "DESC"))
    BrackettedExpression)
   (alternatives Constraint Var)))

(define OrderClause
  (->list
   (concatenation
    (bind-consumed->symbol 
     (concatenation
      (lit/sp "ORDER ")
      (lit/sp "BY")))
    (repetition1 OrderCondition))))
    
(define HavingCondition Constraint)

(define HavingClause
  (->list
   (concatenation
    (lit/sym "HAVING")
    (repetition1 HavingCondition))))

(define GroupCondition
  (alternatives
   BuiltInCall
   FunctionCall
   (concatenation
    (->infix-if
     (->list
      (concatenation 
       (drop-consumed (lit/sp "("))
       Expression
       (optional-sequence
        (concatenation
         (lit/sym "AS")
         Var))
       (drop-consumed (lit/sp ")"))))))
   Var))

(define GroupClause
  (->list
   (concatenation
    (bind-consumed->symbol 
     (concatenation
      (lit/sp "GROUP ")
      (lit/sp "BY")))
    (repetition1 GroupCondition))))

(define SolutionModifier
  (concatenation
   (optional-sequence GroupClause)
   (optional-sequence HavingClause)
   (optional-sequence OrderClause)
   (optional-sequence LimitOffsetClauses)))

(define WhereClause
  (concatenation
   (lit/sym "WHERE")
   GroupGraphPattern))

(define SourceSelector iri)

(define NamedGraphClause
  (->list
   (concatenation
    (lit/sym "NAMED")
    SourceSelector)))

(define DefaultGraphClause SourceSelector)

(define DatasetClause
  (->list
   (concatenation
    (lit/sym "FROM")
    (alternatives DefaultGraphClause NamedGraphClause))))

(define AskQuery
  (->list
   (concatenation
    (lit/sym "ASK")
    (repetition DatasetClause)
    WhereClause
    SolutionModifier)))

(define DescribeQuery
  (concatenation
   (->list
    (concatenation
     (lit/sym "DESCRIBE")
     (alternatives 
      (repetition1 VarOrIri)
      (lit/sym "*"))))
    (->node '@Dataset (repetition DatasetClause))
    (optional-sequence (->list WhereClause))
    SolutionModifier))

(define ConstructQuery
  (alternatives
   (concatenation
    (->list
     (concatenation
      (lit/sym "CONSTRUCT")
      ConstructTemplate))
    (->node '@Dataset (repetition DatasetClause))
    (->list WhereClause)
    SolutionModifier)
   (concatenation
    (->list (lit/sym "CONSTRUCT"))
    (->node '@Dataset (repetition DatasetClause))
    (->list
     (concatenation 
      (lit/sym "WHERE")
      (drop-consumed (lit/sp "{"))
      (optional-sequence TriplesTemplate)
      (drop-consumed (lit/sp "}"))
      SolutionModifier)))))

(define SelectClause
   (concatenation
    (lit/sym "SELECT")
    (optional-sequence                  ; in a list?
     (alternatives 
      (lit/sym "DISTINCT")
      (lit/sym "REDUCED")))
    (alternatives
     (repetition1
      (alternatives
       Var
       (->infix-if
        (->list
	 (concatenation 
	  (drop-consumed (lit/sp "("))
	  Expression
          (lit/sym "AS")
	  Var
	  (drop-consumed (lit/sp ")")))))))
     (lit/sym "*"))))

(define SubSelect
  (->node
   '@SubSelect
   (concatenation 
    (->list SelectClause)
    (->list WhereClause)
    SolutionModifier
    ValuesClause)))

(define SelectQuery
   (concatenation
    (->list SelectClause)
    (->node '@Dataset (repetition DatasetClause))
    (->list WhereClause) 
    SolutionModifier))

(define PrefixDecl
  (->list
   (concatenation
    (lit/sym "PREFIX")
    (bind-consumed->symbol 
     (between-fws PNAME_NS))
    (bind-consumed->symbol
     (between-fws IRIREF)))))

(define  BaseDecl
  (concatenation
   (lit/sym "BASE")
   (between-fws IRIREF)))

(define Prologue
  (repetition
   (alternatives BaseDecl PrefixDecl)))

(define UpdateUnit
(->node '@UpdateUnit  Update))

(define Query
  (->node
   '@Query
   (concatenation
    (->node '@Prologue Prologue)
    (alternatives
     SelectQuery
     ConstructQuery
     DescribeQuery 
     AskQuery)
    ValuesClause)))

(define QueryUnit
  (->node '@QueryUnit Query))

(define TopLevel (alternatives QueryUnit UpdateUnit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

;; should also read from ports
(define (parse-query query)
  (let ((parsed-query (lex TopLevel err query)))
    (if (equal? parsed-query '(error))
        (error "Parse error.")
        (caar parsed-query))))

(define (read-sparql query)
  ;;(cons '@TOP
  (caar (lex TopLevel err query)))

(define (read-triples query)
  ;;(cons '@TOP
  (caar (lex GroupGraphPatternSub err query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors

(define (assoc-when sym alist)
  (and alist
      (assoc sym alist)))

;; (define (unit-query QueryUnit)
;;   (nested-alist-ref QueryUnit '@Unit '@Query))

;; (define (unit-query-dataset QueryUnit)
;;   (nested-alist-ref QueryUnit '@Unit '@Query '@Dataset))

;; (define (unit-query-select QueryUnit)
;;   (assoc-when 'SELECT (nested-alist-ref QueryUnit '@Unit '@Query)))

;; (define (unit-query-where QueryUnit)
;;   (assoc-when 'WHERE (nested-alist-ref QueryUnit '@Unit '@Query)))

;; (define (unit-update QueryUnit)
;;   (nested-alist-ref QueryUnit '@Unit '@Update))

;; (define (unit-update-delete QueryUnit)
;;   (assoc-when 'DELETE (unit-update QueryUnit)))

;; (define (unit-update-where QueryUnit)
;;   (assoc-when 'WHERE (unit-update QueryUnit)))

;; (define (unit-update-insert QueryUnit)
;;   (assoc-when 'INSERT (unit-update QueryUnit)))
