#lang racket/base
;;; List Sequence

   ;  (in-path (p q r) a-path) 
   ;    q iterates over the knots in a path.
   ;    p and r are the previous and next node respectively
   ;    If q is the first node, then p is #f.
   ;    If q is the last  node, then r is #f.

(provide make-3-sequence)

(require racket/match racket/list)

(define (make-3-sequence xs)
  (make-do-sequence ; this assumes a path consists of at least two knots
   (λ ()
     (define first-pos? #t)
     (values 
      (λ(t) ; pos->element
        (cond 
          [first-pos? (match t 
                        [(list* p q _) (values #f p q)]
                        [(list  p)     (values #f p #f)])]
          [else       (match t 
                        [(list* p q r _) (values p q r)]
                        [(list  p q)     (values p q #f)])]))
      (λ(t) ; next-post
        (cond [first-pos? (set! first-pos? #f) t]
              [else       (rest t)]))
      xs                                ; initial pos   
      (λ(t) (or first-pos? (not (empty? (rest t))))) ; cont. with pos
      #f #f))))

(module+ test (require rackunit)
  (check-equal? (for/list ([(x- x x+) (make-3-sequence '(a b c d))])
                  ; x runs through the list
                  ; x- and x+ are the prev. and next element respectively
                  (list x- x x+))
                '((#f a b) (a b c) (b c d) (c d #f))))

