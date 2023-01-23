#lang racket/base
;;; 
;;; SVG Paths
;;;

;; The SVG path parser returns an s-expression representation of the SVG path.
;; The function `svg-path->paths` will convert this represenration into a list
;; of dc-path% instances. These paths can be drawn using the `draw-path` method
;; of a drawing context.

;; A dc-path% contains zero or more closed curves and possibly one open curve.
;; The method `move-to` thus closes the current curve and starts a new open curve.
;; Since svg paths can contain multiple open paths, we need multiple instances
;; of dc-path% to represent an svg path.

(require "elliptical-arc.rkt"
         "svg-path-lexer-and-parser.rkt")

(provide parse-svg-path
         svg-path->paths)

(require racket/gui)

(define (svg-path->paths p)
  ;; The output is represented as a list of dc-paths.
  ;; The "current" in in dc-path.
  (define dc-paths '())
  (define dc-path (new dc-path%))
  (define (new-dc-path)
    (set! dc-paths (cons dc-path dc-paths))
    (set! dc-path (new dc-path%)))
  (define (return-dc-paths)
    (reverse (cons dc-path dc-paths)))  
  ;; Helper
  (define (dc-path-start)
    ; Find the starting point of the current open curve.
    (define-values (closed open) (send dc-path get-datum))
    (cond
      [(empty? open) (error)]
      [else          (values (vector-ref (first open) 0)
                             (vector-ref (first open) 1))]))

  ;; The SVG commands all update the "current point".
  ;; Some commands also needs to know the previous current point and the previous control point.
  ;; The variables below use absolute coordinates.

  ; current point
  (define X 0)
  (define Y 0)
  ; previous current point
  (define X1 0)
  (define Y1 0)
  ; previous control point
  (define prev-control-x #f)
  (define prev-control-y #f)
  ;; Helpers for storing the current point and previous control point.
  (define (save) (set! X1 X) (set! Y1 Y))
  (define (save-control [x #f] [y #f]) (set! prev-control-x x) (set! prev-control-y y))
  (define (clear) (save-control))
  ;; The SVG commands.
  ;;   Commands with upper-case names use absolute coordinates
  ;;   commands with lower-case names use relative coordinates.
  (define (Z)    (clear) (save) (set!-values (X Y) (dc-path-start))                      (send dc-path close))
  (define (z)    (clear) (save) (set!-values (X Y) (dc-path-start))                      (send dc-path close))
  (define (M xy) (clear) (save) (new-dc-path)
                                (match xy [(list x y) (set! X x)       (set! Y y)        (send dc-path move-to X Y)]))
  (define (m xy) (clear) (save) (new-dc-path)
                                (match xy [(list x y) (set! X (+ X x)) (set! Y (+ Y y))  (send dc-path move-to X Y)]))
  (define (L xy) (clear) (save) (match xy [(list x y) (set! X x)       (set! Y y)        (send dc-path line-to X Y)]))
  (define (l xy) (clear) (save) (match xy [(list x y) (set! X (+ X x)) (set! Y (+ Y y))  (send dc-path line-to X Y)]))
  (define (H c)  (clear) (save) (set! X c)                                               (send dc-path line-to X Y))
  (define (h c)  (clear) (save) (set! X (+ X c))                                         (send dc-path line-to X Y))
  (define (V c)  (clear) (save) (set! Y c)                                               (send dc-path line-to X Y))
  (define (v c)  (clear) (save) (set! Y (+ Y c))                                         (send dc-path line-to X Y))
  (define (C xy1 xy2 xy)
    (save)
    (match (list xy1 xy2 xy)
      [(list (list x1 y1) (list x2 y2) (list x y))
       (send dc-path curve-to x1 y1  x2 y2  x y)
       (save-control x2 y2)
       (set! X x) (set! Y y)]))
  (define (c xy1 xy2 xy)
    (save)
    (match (list xy1 xy2 xy)
      [(list (list x1 y1) (list x2 y2) (list x y))
       (send dc-path curve-to (+ X x1) (+ Y y1)   (+ X x2) (+ Y y2)   (+ X x) (+ Y y))
       (save-control (+ X x2) (+ Y y2))
       (set! X (+ X x)) (set! Y (+ Y y))]))
  (define (reflected-control)
    ; Reflect the previous control point in the current point.
    ; This is used by "smooth-to" i.e. by S and s.
    ; Only C,c,S,s stores the previous control point.
    ; If the previous command wasn't C,c,S or s, then use the current point.    
    (define (reflect x x0) (+ x0 (- x0 x))) ; reflect x in x0
    (cond
      [prev-control-x (values (reflect prev-control-x X) (reflect prev-control-y Y))]
      [else           (values X Y)]))
  (define (S xy2 xy)
    ; Like C, but xy1 is given by the reflection of the previous control 
    ; in the current point (X,Y).
    (save)
    (match (list xy2 xy)
      [(list (list x2 y2) (list x y))
       (define-values (x1 y1) (reflected-control))
       (send dc-path curve-to x1 y1 x2 y2 x y)
       (save-control x2 y2)
       (set! X x) (set! Y y)]))
  (define (s xy2 xy)
    (save)
    (match (list xy2 xy)
      [(list  (list x2 y2) (list x y))
       (define-values (x1 y1) (reflected-control))
       (send dc-path curve-to x1 y1 (+ X x2) (+ Y y2) (+ X x) (+ Y y))
       (save-control (+ X x2) (+ Y y2))
       (set! X (+ X x)) (set! Y (+ Y y))]))
  (define (to-rad d) (* (/ d 180.) pi))
  (define (A rx ry x-axis-rotation large-arc-flag sweep-flag x y)
    (clear) (save)
    (set! rx (abs rx))
    (set! ry (abs ry))
    (cond
      ; omit arc if the current point and the end point are identical
      [(and (= X x) (= Y y))  (void)]
      ; if rx or ry is zero, then draw a straight line
      [(or (= rx 0) (= ry 0)) (L x y)]
      [else
       (send dc-path append
             (elliptical-arc-path X Y x y rx ry (to-rad x-axis-rotation) large-arc-flag sweep-flag))])
    (set! X x)
    (set! Y y))
  (define (a rx ry x-axis-rotation large-arc-flag sweep-flag x y)
    ; The bookkeeping is done by A.
    (A rx ry x-axis-rotation large-arc-flag sweep-flag (+ X x) (+ Y y)))
  ;; Multiple uses of the same command are handled here.
  (define (H* cs)    (for-each H cs))
  (define (h* cs)    (for-each h cs))
  (define (V* cs)    (for-each V cs))
  (define (v* cs)    (for-each v cs))
  (define (L* cs)    (for-each L cs))
  (define (l* cs)    (for-each l cs))
  (define (M* xys)   (for-each M xys))
  (define (m* xys)   (for-each m xys))
  (define (C* xyss)  (for-each (λ (xys)  (apply C xys))  xyss))
  (define (c* xyss)  (for-each (λ (xys)  (apply c xys))  xyss))
  (define (S* xyss)  (for-each (λ (xys)  (apply S xys))  xyss))
  (define (s* xyss)  (for-each (λ (xys)  (apply s xys))  xyss))
  (define (A* argss) (for-each (λ (args) (apply A args)) argss))
  (define (a* argss) (for-each (λ (args) (apply a args)) argss))
  ;; Loop through the svg path and call the relevant converter.
  (define (loop p)
    (cond
      [(empty? p) (void 'done)]
      [else       (match (first p)
                    [(cons 'M points)  (M* points)]
                    [(cons 'm points)  (m* points)]
                    [(cons 'L points)  (L* points)]
                    [(cons 'l points)  (l* points)]
                    [(cons 'H coords)  (H* coords)]
                    [(cons 'h coords)  (h* coords)]
                    [(cons 'V coords)  (V* coords)]
                    [(cons 'v coords)  (v* coords)]
                    [(cons 'C pointss) (C* pointss)]
                    [(cons 'c pointss) (c* pointss)]
                    [(cons 'S pointss) (S* pointss)]
                    [(cons 's pointss) (s* pointss)]
                    [(cons 'A argss)   (A* argss)]
                    [(cons 'a argss)   (a* argss)]
                    [(list 'Z)         (Z)]
                    [(list 'z)         (z)])
                  (loop (rest p))]))
  (loop p)
  (return-dc-paths))
