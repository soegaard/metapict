#lang racket/base
(provide (except-out (all-defined-out) def defv defm)
         (rename-out [define        def]
                     [define-values defv]
                     [match-define  defm]))

(require (for-syntax racket/base) racket/match racket/math)

; shorter names
(define-syntax defv (make-rename-transformer #'define-values))
(define-syntax defm (make-rename-transformer #'match-define))
(define-syntax def  (make-rename-transformer #'define))

; short names for multiples of pi
(defm (list -pi -3pi/4 -pi/2 -pi/4 _ pi/4 pi/2 3pi/4) (for/list ([n (in-range -1   1 1/4)]) (* pi n)))
(defm (list -π -3π/4 -π/2 -π/4 _ π/4 π/2 3π/4 π)      (for/list ([n (in-range -1 5/4 1/4)]) (* pi n)))
(defm (list 2pi -2pi) (list (* 2 pi) (* -2 pi)))
(defv (2π -2π) (values 2pi -2pi))

(define-syntax (first-value stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       (call-with-values (λ() expr ...) 
                         (λ xs (car xs))))]))

(module+ test (require rackunit)
  (check-equal? (first-value (values 1 2)) 1))