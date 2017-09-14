;; Docs:
;; http://wiki.call-cc.org/eggref/4/abnf

;; Example:
;; https://code.call-cc.org/svn/chicken-eggs/release/4/json-abnf/trunk/json-abnf.scm

;; Questions
;; - (|X DISTINCT| ..) or (X (DISTINCT ..)) ? (be consistent

(use sparql-query
     srfi-13 srfi-69 http-client intarweb uri-common medea cjson matchable irregex)

(require-extension utf8 utf8-srfi-14
                   typeclass input-classes abnf abnf-charlist abnf-consumers
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

(define-syntax ->node
  (syntax-rules () 
    ((_ label l p)    (bind (consumed-values->list label l)  p))
    ((_ label p)      (bind (consumed-values->list label)  p))
    ))

;; list if >1 children (abstract this & next 2)
;; but this often requires (->list-if (->list (:: ...))) which is redundant.
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
   (compose string->number list->string)))

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

(define PN_LOCAL_ESC
  (set-from-string "\\_~.-!$&'()*+,;=/?#@%"))

(define HEX hexadecimal)

;; how should this be represented?
(define PERCENT
  (concatenation
   (char-list/lit "%") HEX HEX))

(define PLX
  (alternatives PERCENT PN_LOCAL_ESC))

;; (define PN_LOCAL  ;; **
;;   (:+
;;    (alternatives
;;     char-list/decimal
;;     char-list/alpha
;;     (set-from-string "-_%"))))

(define (lstar p1 p2)
  (letrec ((rec
            (vac
             (alternatives
              (seq p1 rec)
              p2))))
    rec))

(use amb amb-extras)

(define (bstar p)
  (lambda (sk fk strm)
    (or
     (call/cc
      (lambda (k)
	(p (lambda (strm1)
	     ((bstar p) sk (lambda (s)
			     (print "failed at " strm1) 
			     (print "going back to " strm)
			     (print "=>" (sk strm))
				   (k #f)) strm1))
	   fk strm)))
     (sk strm))))

(define PN_LOCAL
  (vac
   (concatenation
    (alternatives 
     PN_CHARS_U
     (char-list/lit ":")
     char-list/decimal
     PLX)
    (:?
     (concatenation
      (bstar
       (alternatives 
	(set-from-string ":.") 
	PN_CHARS PLX))
      (alternatives 
       PN_CHARS
       (char-list/lit ":") 
       PLX))))))


(define PN_PREFIX
  (vac
   (concatenation
    PN_CHARS_BASE
    (:?
     ;; (letrec ((rec
     ;;           (vac
     ;;            (alternatives
     ;;             (seq (alternatives 
     ;;                   (char-list/lit ".") 
     ;;                   PN_CHARS)
     ;;                  rec)
     ;;             PN_CHARS))))
     ;;   rec)))))
     (lstar (alternatives  (char-list/lit ".")  PN_CHARS) PN_CHARS)))))

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
   (::
    (alternatives
     PN_CHARS_U
     char-list/decimal)
    (:*
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
    (:: (drop-consumed (lit/sp "["))
        (drop-consumed (:? fws))
        (drop-consumed (lit/sp "]")))))

(define WS
  (set
   (list->char-set
    (list #\space #\tab #\newline #\return))))

(define NIL
  (->node
   '@NIL
   (:: 
    (drop-consumed (lit/sp "("))
    (drop-consumed (lit/sp ")")))))

(define ECHAR
  (:: (char-list/lit "\\")
      (set-from-string "tbnrf\\\"'")))

(define STRING_LITERAL_LONG2
  (concatenation
   (char-list/lit "\"\"\"")
   (:*
    (::
     (:?
      (alternatives 
       (char-list/lit "\"")
       (char-list/lit "\"\"")))
     (alternatives
      (set (char-set-complement (string->char-set "\"\\")))
      ECHAR)))
   (lit/sp "\"\"\"")))

(define STRING_LITERAL_LONG1
  (concatenation
   (char-list/lit "'''")
   (:*
    (::
     (:?
      (alternatives 
       (char-list/lit "'")
       (char-list/lit "''")))
     (alternatives
      (set (char-set-complement (string->char-set "\"\\")))
      ECHAR)))
   (lit/sp "'''")))

(define STRING_LITERAL2
  (concatenation
   (drop-consumed (char-list/lit "\""))
   (:*
    (alternatives
     (set (char-set-complement (list->char-set (list #\" #\\ #\newline #\return))))
     ECHAR))
   (drop-consumed (char-list/lit "\""))))

(define STRING_LITERAL1
  (concatenation
   (char-list/lit "'")
   (:*
    (alternatives
     (set (char-set-complement (list->char-set (list #\' #\\ #\newline #\return))))
     ECHAR))
   (char-list/lit "'")))

(define EXPONENT
  (concatenation
   (set-from-string "eE")
   (:? (set-from-string "+-"))
   (:+ decimal)))

(define DOUBLE_NEGATIVE
  (vac
   (concatenation
    (drop-consumed (char-list/lit "-"))
    (->negative DOUBLE))))

(define DECIMAL_NEGATIVE
  (vac
   (::
    (drop-consumed (char-list/lit "-"))
    (->negative DECIMAL))))

(define INTEGER_NEGATIVE
  (vac
   (::
    (drop-consumed (char-list/lit "-") )
    (->negative INTEGER))))

(define DOUBLE_POSITIVE
  (vac
   (::
    (drop-consumed (char-list/lit "+") )
    DOUBLE)))

(define DECIMAL_POSITIVE
  (vac
   (:: (lit/sym "+") DECIMAL)))

(define INTEGER_POSITIVE
  (vac
   (:: (lit/sym "+") INTEGER)))

(define DOUBLE 
  (->number
   (alternatives
    (concatenation
     (:+ decimal)
     (char-list/lit ".")
     (:* decimal)
     EXPONENT)
    (concatenation
     (char-list/lit ".")
     (:+ decimal)
     EXPONENT)
    (concatenation
     (:+ decimal)
     EXPONENT))))

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
   (::
   (char-list/lit "_:")
   (alternatives
    PN_CHARS_U
    char-list/decimal)
   (:?
    (lstar 
     (alternatives (char-list/lit ".") PN_CHARS) 
     PN_CHARS)))))

(define PNAME_LN
  (vac
   (concatenation PNAME_NS PN_LOCAL)) )

(define PNAME_NS
  (:: (:? PN_PREFIX) (char-list/lit ":")))

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
   (alternatives BLANK_NODE_LABEL ANON))

(define PrefixedName
  (bind-consumed->symbol
   (alternatives PNAME_LN PNAME_NS)))

(define iri
  (alternatives 
   (bind-consumed->symbol
    ;; (between-fws IRIREF)) ;; whitespace...
    IRIREF)
   PrefixedName))

(define String
  (bind-consumed->string
   (alternatives
    STRING_LITERAL1 STRING_LITERAL2 STRING_LITERAL_LONG1 STRING_LITERAL_LONG2)))

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
    (alternatives
     (->list
      (::
       (lit/sym "DISTINCT")
       content))
     content))))

(define Aggregate
  (vac
   (alternatives
    (aggregate-expression "COUNT" (alternatives (lit/sym "*")  Expression))
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
  (->list
   (::
    (bind-consumed->symbol
     (:: (lit/sp "NOT ")
         (lit/sp "EXISTS")))
    GroupGraphPattern))))

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
   (::
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

;; To Prefix Notation!!
(define UnaryExpression
  (vac 
  (alternatives
   (:: (lit/sym "!") PrimaryExpression)
   (:: (lit/sym "+") PrimaryExpression)
   (:: (lit/sym "-") PrimaryExpression)
   PrimaryExpression)))

;; To Prefix Notation!!
(define MultiplicativeExpression
  (->polish
    (->list
     (::
      UnaryExpression
      (:*
       (alternatives
        (:: (lit/sym "*")
            UnaryExpression)
        (:: (lit/sym "/")
            UnaryExpression)))))))

;; AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | ( NumericLiteralPositive | NumericLiteralNegative ) ( ( '*' UnaryExpression ) | ( '/' UnaryExpression ) )* )*
(define AdditiveExpression
  ;;(->infix-if
  ;;   (->list-if
  (->polish
    (->list
     (::
      MultiplicativeExpression
      (:*
       (alternatives
        (:: (lit/sym "+") MultiplicativeExpression)
        (:: (lit/sym "-") MultiplicativeExpression)
        ;; ??
        ;; (:: 
        ;;  (alternatives  NumericLiteralPositive NumericLiteralNegative)
        ;;  (:*
        ;;   (alternatives
        ;;    (:: (lit/sym "*") UnaryExpression)
        ;;    (:: (lit/sym "/") UnaryExpression))))))))))
        ))))))

(define NumericExpression
  (vac 
 AdditiveExpression))

(define (relexp label op a b)
  (->node
   label
   (::
    (between-fws a)
    (::
     (drop-consumed op)
     (between-fws b)))))

(define RelationalExpression 
  (vac
   (->infix-if
    (->list
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
        (::  (bind-consumed->symbol
	      (::
	       (lit/sp "NOT ") (lit/sp "IN")))
	     ExpressionList))))))))
  ;; (vac 
  ;;  (begin (print "RelationalExpression")
  ;;  (alternatives

  ;;   (relexp '= (lit/sp "=") NumericExpression NumericExpression)
  ;;   (relexp '!= (lit/sp "!=") NumericExpression NumericExpression)
  ;;   (relexp '< (lit/sp "<") NumericExpression NumericExpression)
  ;;   (relexp '> (lit/sp ">") NumericExpression NumericExpression)
  ;;   (relexp '<= (lit/sp "<=") NumericExpression NumericExpression)
  ;;   (relexp '>= (lit/sp ">=") NumericExpression NumericExpression)
  ;;   (relexp 'IN (lit/sp "IN")  NumericExpression ExpressionList)
  ;;   (relexp '|NOT IN| (:: (lit/sp "NOT") (lit/sp "IN"))  NumericExpression ExpressionList)
  ;;   NumericExpression
  ;;   )))

(define ValueLogical RelationalExpression)

(define ConditionalAndExpression
  (vac 
   (->node-if
    (->node
     '&&
     (:: 
      ValueLogical
      (:*
       (::
        (drop-consumed (lit/sp "&&"))
        ValueLogical)))))))

(define ConditionalOrExpression
  ;; (:: ConditionalAndExpression
  ;;     (:* (:: (lit/sym "||") ConditionalAndExpression))))
  (->node-if
   (->node
    '||
    (:: 
     ConditionalAndExpression
     (:*
      (::
       (drop-consumed (lit/sp "||"))
       ConditionalAndExpression))))))

(define Expression
  (vac 
 ConditionalOrExpression))
  
(define GraphTerm
   (alternatives
    iri RDFLiteral NumericLiteral BooleanLiteral BlankNode NIL))

;; (define Var
;;   (bind-consumed->symbol
;;    (alternatives
;;     (concatenation (char-list/lit "?") varname)
;;     (concatenation (char-list/lit "$") varname))))

(define Var
  (bind-consumed->symbol
   (alternatives VAR1 VAR2)))

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
   (->list
    (:: 
     (drop-consumed (lit/sp "("))
     (:+ GraphNodePath)
     (drop-consumed (lit/sp ")"))))))

(define Collection
  (vac
   (->list
    (:: (lit/sp "(")
	(:+ GraphNode)
	(lit/sp ")")))))

(define BlankNodePropertyListPath
  (vac
   (->node
    '@Blank
   (:: (drop-consumed (lit/sym "["))
       PropertyListPathNotEmpty
        (drop-consumed (lit/sym "]")))) ))

(define TriplesNodePath
  (alternatives CollectionPath BlankNodePropertyListPath))

(define BlankNodePropertyList
  (vac
   (->node
    '@Blank
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
   (->list
    (::
     (drop-consumed (lit/sp "("))
     (->node-if
      (->node
       '||
       (:: 
        PathOneInPropertySet
        (:*
         (::
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
      (:: 
       (drop-consumed (char-list/lit "!"))
       PathNegatedPropertySet))
     (->list
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
  (->infix-if
   (->list
    (:: PathPrimary
        (:? PathMod)))))

(define PathSequence
  (vac
   (->node-if
    (->node 
     '/
     (:: 
      PathEltOrInverse
      (:*
       (::
        (drop-consumed (lit/sp "/"))
        PathEltOrInverse)))))))

(define PathAlternative
;;  (alternatives
  (->node-if
   (->node
    '||
    (:: PathSequence
        (:*
         (::
          (drop-consumed (lit/sp "|"))
          PathSequence))))))
;;   PathSequence))

(define Path PathAlternative)

(define ObjectPath GraphNodePath)

(define	ObjectListPath
;;  (alternatives
  (->list-if
   (->list
    (:: ObjectPath
	(:*
	 (:: (drop-consumed (lit/sp ","))
	     ObjectPath))))))
;;   ObjectPath))

(define VerbSimple Var)

(define VerbPath Path)

(define PropertyListPathNotEmpty
  (vac
;;   (alternatives 
;;   (->list-if
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
    ;; (::
    ;;  (between-fws (alternatives VerbPath VerbSimple))
    ;;  ObjectListPath))))

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
;;  (alternatives
  (->list-if
   (->list
    (:: Object
	(:*
	 (:: (drop-consumed (lit/sp ","))
	     Object))))))
;;   Object))

(define Verb (alternatives VarOrIri (lit/sym "a")))

(define PropertyListNotEmpty
;;  (alternatives
;;  (->list-if
   (->list
    (:: (->list 
	 (:: (between-fws Verb)
	     ObjectList))
	(:* (:: (drop-consumed (lit/sp ";"))
               (:? (->list
                    (:: (between-fws Verb)
                        ObjectList))))))))
    ;; (::
    ;;  (:: (between-fws Verb)
    ;;      ObjectList))))

(define PropertyList (:? PropertyListNotEmpty))

(define TriplesSameSubject
  (alternatives (:: (between-fws VarOrTerm)
                    (between-fws PropertyListNotEmpty))
                (:: (between-fws TriplesNode)
                    (between-fws PropertyList))))

(define ConstructTriples
  (vac
   (::
    (->list TriplesSameSubject)
    (:?
     (::
      (drop-consumed (lit/sp "."))
      (:? ConstructTriples))))))

(define ConstructTemplate
  (::
   (drop-consumed (lit/sp "{"))
   (:? ConstructTriples)
   (drop-consumed (lit/sp "}"))))

(define ExpressionList
  (alternatives
   NIL
   (->list
    (:: (drop-consumed (lit/sp "("))
        Expression
        (:*
	 (::
	  (drop-consumed (lit/sp ","))
           Expression))
        (drop-consumed (lit/sp ")"))))))

(define ArgList
  (alternatives
   NIL
   (->list
    (:: (drop-consumed (lit/sp "("))
        (:? (lit/sym "DISTINCT"))
        Expression
        (:*
          (::
           (drop-consumed (lit/sp ","))
	   Expression))
        (drop-consumed (lit/sp ")"))))))

(define FunctionCall 
  (:: iri ArgList))

(define Constraint
  (alternatives BrackettedExpression BuiltInCall FunctionCall))

(define Filter (->list (:: (lit/sym "FILTER") Constraint)))

(define GroupOrUnionGraphPattern
  (vac
   (->node-if
    (->node
     'UNION
      (::
       (->list GroupGraphPattern)
       (:* (::
            (drop-consumed (lit/sp "UNION"))
            (->list GroupGraphPattern))))))))

(define MinusGraphPattern
  (vac
   (:: (lit/sym "MINUS") GroupGraphPattern)))

(define DataBlockValue
  (alternatives iri RDFLiteral NumericLiteral BooleanLiteral (lit/sym "UNDEF")))

(define InlineDataFull
  (::
   (alternatives 
    NIL 
   (->list
    (::      
     (drop-consumed (lit/sp "("))
     (:* (between-fws Var))
     (drop-consumed (lit/sp ")")))))
   (::
    (drop-consumed (lit/sp "{"))
    (:*
     (alternatives
      (->list
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
   (:* (between-fws DataBlockValue))
   (drop-consumed (lit/sp "}"))))

(define DataBlock
  (alternatives InlineDataOneVar InlineDataFull))

(define InlineData
  (->list
   (::
    (lit/sym "VALUES")
    DataBlock)))

;; bug - BIND( ?a + ?b AS ?c) is not parsed into prefix notation
(define Bind
  (->list
   (:: 
    (lit/sym "BIND")
    (->node
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
                Bind 
                InlineData
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
     (->node
      '@Dataset
      (->list (:: (lit/sym "WITH") iri))))
    (alternatives 
     (:: DeleteClause
	 (:? InsertClause))
     InsertClause)
    (->node '@Using (:* UsingClause))
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
    (->node
     '@Update
     (:: 
      (->node '@Prologue Prologue)
      Update1))
    (:?
     (:: 
      (drop-consumed (lit/sp ";"))
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
    
(define HavingClause
  (->list
   (::
    (lit/sym "HAVING")
    Constraint)))

(define GroupCondition
  (alternatives
   BuiltInCall
   FunctionCall
   (::
    (->node
     'AS
     (:: 
      (drop-consumed (lit/sp "("))
      Expression
      (:?
       (::
	(drop-consumed (lit/sym "AS"))
	Var))
      (drop-consumed (lit/sp ")")))))
   Var))

(define GroupClause
  (->list
   (::
    (bind-consumed->symbol 
     (::
      (lit/sp "GROUP ")
      (lit/sp "BY")))
    (:+ GroupCondition))))

(define SolutionModifier
  (::
   (:? GroupClause)
   (:? HavingClause)
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

(define ConstructQuery
  (alternatives
   (::
    (->list
     (::
      (lit/sym "CONSTRUCT")
      ConstructTemplate))
    (->node '@Dataset (:* DatasetClause))
    (->list WhereClause)
    SolutionModifier)
   (::
    (->list (lit/sym "CONSTRUCT"))
    (->node '@Dataset (:* DatasetClause))
    (->list
     (:: 
      (lit/sym "WHERE")
      (drop-consumed (lit/sp "{"))
      (:? TriplesTemplate)
      (drop-consumed (lit/sp "}"))
      SolutionModifier)))))

(define SelectClause
   (::
    (bind-consumed->symbol 
     (alternatives
      (lit/sp "SELECT DISTINCT")
      (lit/sp "SELECT REDUCED")
      (lit/sp "SELECT")))
    (alternatives
     (:+
      (between-fws 
       (alternatives
	Var
	(->node
	 'AS
	 (:: 
	  (drop-consumed (lit/sp "("))
	  Expression
	  (drop-consumed (lit/sym "AS"))
	  Var
	  (drop-consumed (lit/sp ")")))))))
     (lit/sym "*"))))

(define SubSelect
  (->node
   '@SubSelect
   (:: 
    (->list SelectClause)
    (->list WhereClause)
    SolutionModifier
    ValuesClause)
   ))

(define SelectQuery
   (::
    (->list SelectClause)
    (->node '@Dataset (:* DatasetClause)) ;;'@DatasetClause
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
  ;; (bind-consumed-values->node
  ;;'*PROLOGUE*
   (repetition PrefixDecl))

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
     ;; DescribeQuery AskQuery )
     )
    ValuesClause)))

(define QueryUnit
  (->node '@QueryUnit Query))

(define TopLevel (alternatives QueryUnit UpdateUnit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

;; should also read from ports
(define (parse-query query)
  ;;(cons '@TOP
  (caar (lex TopLevel err query)))

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
