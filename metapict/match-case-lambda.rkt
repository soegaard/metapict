#lang racket/base
(require (for-syntax racket/base racket/syntax syntax/parse)
         racket/match)

(define-syntax (match-case-lambda stx)
  (syntax-parse stx
    [(_ [(pat ...) expr ...] ...)
     (with-syntax ([((v ...) ...) 
                    (map generate-temporaries
                         (syntax->list #'((pat ...) ...)))])
       #'(case-lambda
           [(v ...) (match* (v ...)
                      [(pat ...) expr ...])]
           ...))]))

(struct pt   (x y) #:transparent)
(struct vec (x y) #:transparent)

(define pt- 
  (match-case-lambda
   [((pt x y))           (pt (- x) (- y))]
   [((pt x y) (vec s t)) (pt (- x s) (- y t))]
   [((pt x y) (pt s t))  (pt (- x s) (- y t))]))

; (pt- (pt 1 2) (pt 4 8))