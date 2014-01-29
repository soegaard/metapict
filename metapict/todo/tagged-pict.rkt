#lang racket/base
(require pict)
(provide tag-pict
         find-tag
         find-tag*
         tag-path?
         pict-tag)

; This is a modified version of tag-pict.rkt from unstable-lib/gui .
; This version allows non-symbol tags (convenient for tables).
 
(struct tagged-pict pict (tag))
;; tag is any non-list value

(define (tag-pict p the-tag)
  (tagged-pict (pict-draw p)
               (pict-width p)
               (pict-height p)
               (pict-ascent p)
               (pict-descent p)
               (list (make-child p 0 0 1 1 0 0))
               #f
               (pict-last p)
               the-tag))

;; find-tag** : pict tagpath boolean -> (listof pict-path)
(define (find-tag** p tagpath all?)
  (let ([tagpath (if (list? tagpath) tagpath (list tagpath))])
    (define-syntax-rule (append* e1 e2)
      (let ([x e1])
        (cond [(or all? (null? x)) (append x e2)]
              [else x])))
    (define (loop p tagpath)
      (cond [(pair? tagpath)
             (childrenloop (pict-children p) tagpath)]
            [(null? tagpath)
             (list (list p))]))
    (define (pairloop p tagpath)
      (append*
       (cond [(and (tagged-pict? p)
                   (equal? (tagged-pict-tag p) (car tagpath)))
              (for/list ([r (in-list (loop p (cdr tagpath)))])
                (cons p r))]
             [else null])
       (childrenloop (pict-children p) tagpath)))
    (define (childrenloop children tagpath)
      (cond [(pair? children)
             (append* (pairloop (child-pict (car children)) tagpath)
                      (childrenloop (cdr children) tagpath))]
            [(null? children)
             null]))
    (loop p tagpath)))

(define (find-tag p tagpath)
  (let ([r (find-tag** p tagpath #f)])
    (and (pair? r) (car r))))

(define (find-tag* p tagpath)
  (find-tag** p tagpath #t))

(define (tag-path? x)
  (or (symbol? x)
      (and (list? x) (pair? x) (andmap symbol? x))))

(define (pict-tag p)
  (and (tagged-pict? p) (tagged-pict-tag p)))

