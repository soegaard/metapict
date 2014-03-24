#lang racket
; TODO: NOT DONE
;
; This module exports dc-path->curve, which converts a dc-path object
; to a curve (represented as a list of Bezier curves).
; This can be used to convert a text outline into a curve.

(require racket/draw "def.rkt" "structs.rkt" "curve.rkt" "path.rkt")

(define (dc-path->curve dcp)
  (def dc (new record-dc%))
  (send dc draw-path dcp)
  (def datum (send dc get-recorded-datum))
  (match (drawn-path datum)
    [(list segmentss x y fill-rule)
     (for/list ([segments segmentss])
       (displayln "X")
       (for/list ([s segments])
         (display ".")
         (recorded-segment->bezs s)))]))

(define (drawn-path datum)
  (for/or ([d datum])
    (match d [(list 'draw-path p ...) p] [_ #f])))

(define (recorded-segment->bezs segment)
  ; (displayln segment) (newline)
  (match segment
    [(list* (cons x0 y0) more)
     (define (to-curves curves bezs x0 y0 more)
       ; (displayln x0) (displayln y0) (displayln more) (newline)
       (match more
         [(list* (vector x1 y1 x2 y2) (cons x3 y3) more)
          (defv (p0 p1 p2 p3) (values (pt x0 y0) (pt x1 y1) (pt x2 y2) (pt x3 y3)))
          (to-curves curves (cons (bez p0 p1 p2 p3) bezs) x3 y3 more)]
         [(list* (cons x1 y1) more)
          (to-curves (cons (curve (pt x0 y0) -- (pt x1 y1)) curves) '() x1 y1 more)]
         [_ (reverse (cond [(empty? bezs) curves]
                           [else (cons (curve: #f (reverse bezs)) curves)]))]))
     (vector (to-curves '() '() x0 y0 more))]
    [_ (error 'recorded-segment->bezs "huh?")]))
    
    
  

(def dcp (new dc-path%))
(def default-font (make-object font%))
(send dcp text-outline default-font "Foo" 0 0 #f)
; (dc-path->curve dcp)

#;(draw-path
 (( ((1.1171875 . 11.6015625)   ;    p1
     (1.1171875 . 2.9296875)    ; -- p2
     (5.96484375 . 2.9296875)   ; -- p3
     (5.96484375 . 3.8515625)   ; ...
     (2.3515625 . 3.8515625)
     (2.3515625 . 6.7578125)
     (5.3828125 . 6.7578125)
     (5.3828125 . 7.6640625)
     (2.3515625 . 7.6640625)
     (2.3515625 . 11.6015625))
    
     ((9.64453125 . 11.75)
      #(8.734375 11.75 8.0078125 11.4453125)        ; control points for bezier curve
      (7.46484375 . 10.84375)
      #(6.921875 10.23828125 6.6484375 9.4296875)
      (6.6484375 . 8.421875)
      #(6.6484375 7.3984375 6.921875 6.5859375)
      (7.46875 . 5.9921875)
      #(8.01171875 5.39453125 8.75 5.09765625)
      (9.6875 . 5.09765625)
      #(10.62109375 5.09765625 11.359375 5.39453125)
      (11.90234375 . 5.9921875)
      #(12.44921875 6.5859375 12.71875 7.390625)
      (12.71875 . 8.40625)
      #(12.71875 9.4453125 12.4453125 10.265625)
      (11.8984375 . 10.859375)
      #(11.3515625 11.453125 10.6015625 11.75)
      (9.64453125 . 11.75))
     
     ((9.6640625 . 10.8828125)
      #(10.88671875 10.8828125 11.49609375 10.0546875)
      (11.49609375 . 8.40625)
      #(11.49609375 6.77734375 10.890625 5.96484375)
      (9.6875 . 5.96484375)
      #(8.484375 5.96484375 7.8828125 6.78125)
      (7.8828125 . 8.421875)
      #(7.8828125 10.0625 8.4765625 10.8828125)
      (9.6640625 . 10.8828125))
     
     ((16.64453125 . 11.75)
      #(15.734375 11.75 15.0078125 11.4453125)
      (14.46484375 . 10.84375)
      #(13.921875 10.23828125 13.6484375 9.4296875)
      (13.6484375 . 8.421875)
      #(13.6484375 7.3984375 13.921875 6.5859375)
      (14.46875 . 5.9921875)
      #(15.01171875 5.39453125 15.75 5.09765625)
      (16.6875 . 5.09765625)
      #(17.62109375 5.09765625 18.359375 5.39453125)
      (18.90234375 . 5.9921875)
      #(19.44921875 6.5859375 19.71875 7.390625)
      (19.71875 . 8.40625)
      #(19.71875 9.4453125 19.4453125 10.265625)
      (18.8984375 . 10.859375)
      #(18.3515625 11.453125 17.6015625 11.75)
      (16.64453125 . 11.75))
     
     ((16.6640625 . 10.8828125)
      #(17.88671875 10.8828125 18.49609375 10.0546875)
      (18.49609375 . 8.40625)
      #(18.49609375 6.77734375 17.890625 5.96484375)
      (16.6875 . 5.96484375)
      #(15.484375 5.96484375 14.8828125 6.78125)
      (14.8828125 . 8.421875)
      #(14.8828125 10.0625 15.4765625 10.8828125)
      (16.6640625 . 10.8828125))
     
     ((13.0 . 11.6015625))
     )))

(require metapict)
(set-curve-pict-size 300 300)
(with-window (window -10 30 30 -10)
  (dc-path->curve dcp)
  #;(draw (draw* (map fill (map (rotatedd 10) (dc-path->curve dcp))))
          (color "red" (draw* (map (rotatedd 10) (dc-path->curve dcp))))))
