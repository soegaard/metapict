#lang racket/base
;;;
;;; Path Operations
;;;

(provide
 /-      ; p /- q   connect p and q with line(s) first vertical, then horizontal
 -/      ; p -/ p   connect p and q with line(s) first horizontal, then vertical
 --++    
 -arc    ; p -arc radius start stop   append a circle arc to p,
 ;                                    the arc has radius radius and angles start and stop
 ;                                    are in degrees
 -rectangle ; p -rectangle q  rectangle with p and q as opposite corners
 )   ; 


(require
 racket/match racket/format racket/list
 "def.rkt" "path.rkt" "structs.rkt" "curve.rkt" "pt-vec.rkt" "shapes.rkt")


;;; A few handy path operations

;;  /-
;;  -/
;;  --++
;; -arc

(define (handle/- spec)
  (match spec
    [(list* (? pt? p0) /- (? pt? p1) more)
     (defm (pt x0 _) p0)
     (defm (pt _ y1) p1)
     (list* p0 -- (pt x0 y1) -- p1 more)]
    [_ (error 'handle/- (~a "error in specification" spec))]))

(define (handle-/ spec)
  (match spec
    [(list* (? pt? p0) -/ (? pt? p1) more)
     (defm (pt _ y0) p0)
     (defm (pt x1 _) p1)
     (list* p0 -- (pt x1 y0) -- p1 more)]
    [_ (error 'handle-/ (~a "error in specification" spec))]))

(define (handle--++ spec)
  (match spec
    [(list* (? pt? p0) --++ (or (? vec? v) (? pt v)) more)
     (defm (pt x0 y0) p0)
     (defm (or (pt x1 y1) (vec x1 y1)) v)
     (list* p0 -- (pt+ p0 (vec x1 y1)) more)]
    [_ (error 'handle-++- (~a "error in specification" spec))]))


;; #(struct:curve #f (#(struct:bez
;;                      #(struct:pt 0.9998646417195167 2.002013423858278)
;;                      #(struct:pt 0.9986281766760592 2.0195363073119834)
;;                      #(struct:pt 0.9972386795796857 2.037035729505035)
;;                      #(struct:pt 0.9956969134691551 2.0545098482989133))
;;                    ))

(require "draw.rkt")

(define (bez->path b)
  (defm (bez p0 p1 p2 p3) b)
  (list p0 (controls-and p1 p2) p3))

(define (handle-arc spec)
  (match spec
    [(list* (? pt? p0) -arc radius start stop more)
     (defm (pt x0 y0) p0)
     (def c (arc-from p0 radius start stop))
     (defm (curve: closed? bs) c)
     (append (append* (add-between (map bez->path bs) (list ..)))
             more)]
    [_ (error 'handle-arc (~a "error in specification" spec))]))

(define (handle-rectangle spec)
  (match spec
    [(list* (? pt? p) -rectangle (? pt? q)  more)
     (list* p -/ q -/ p -/ q more)]
    [_ (error 'handle-rectangle (~a "error in specification" spec))]))


(define /-         (path-operation handle/-))
(define -/         (path-operation handle-/))
(define --++       (path-operation handle--++))
(define -arc       (path-operation handle-arc))
(define -rectangle (path-operation handle-rectangle))


