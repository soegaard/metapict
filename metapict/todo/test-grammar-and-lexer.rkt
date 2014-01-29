#lang racket
(require "grammar-expressions.rkt"
         "lexer.rkt")

(require racket/pretty)

(define (lex-and-parse str)
  (syntax->datum (parse (lex (open-input-string str)))))

(define strs 
  '("1" "1+2" "1*2" "1+2*3" "1+2x" "1+2x3" "1+x[2]"
        "(1,2)" "(1,2,3)"
        "(1,2)..(3,4)--(5,6)...(7,8)"))

(for ([s strs])
  (displayln s)
  (pretty-print (lex-and-parse s))
  (newline))

(require (for-syntax
          "grammar-expressions.rkt"
          "lexer.rkt"
          syntax/parse
          racket/base))

(begin-for-syntax
  (require (for-syntax syntax/parse racket/base)
           racket/base)
(define-syntax (expression stx)
  ;  expression : subexpression
  ;         | expression tertiary-binop tertiary
  ;         | path-subexpression direction-specifier
  ;         | path-subexpression path-join "cycle"
  (syntax-parse stx
    #:datum-literals 
    (subexpression tertiary-binop tertiary direction-specifier
                   path-subexpression path-join)
    [(_ (subexpression s)) 
     #'(list s)]
    [(_ (expression e) (tertiary-binop o) (tertiary t))
     #'(list o e t)]
    [(_ (path-subexpression p) (direction-specifier d))
     #'(list p d)]
    [(_ (path-subexpression p) (path-join j))
     #'(list p j)])))

(define-syntax ($ stx)
  (syntax-parse stx
    [(_ str)
     (parse (lex (open-input-string (syntax->datum #'str))))]))

($ "1")
