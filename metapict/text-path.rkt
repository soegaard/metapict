#lang racket/base

;;;
;;; Text Path
;;; 

(provide text-outline    ; turn a string into a list of curves
         dc-path->curves ; convert a dc-path% into a list of curves
         )


(require racket/class racket/draw racket/match racket/list
         "def.rkt" "font.rkt" "draw.rkt" "structs.rkt" "device.rkt"
         "trans.rkt" "window.rkt")


; the position pos is the top left of the text in logical coordinates
(define (text-outline font text              [pos (pt 0 0)] ; logical
                      #:kerning?             [kerning? #f]
                      #:return-bounding-box? [bbox? #f])
  (def T (current-curve-transformation)) ; logical -> device
  (def S (inverse T))                    ; device  -> logical
  (defm (pt x y) (T pos)) ; dev

  (def p (new dc-path%))
  (send p text-outline font text 0 0 kerning?) ; 
  ; the coordinates in these curves are device coordinates,
  (def cs (dc-path->curves p)) ;
  ; the top left corner needs correcting
  ; - the bounding box does *not* have top left in (0,0) - sigh.

  ; now get the bounding box (in device coordinates)
  (defv (left top width height) (send p get-bounding-box))
  ; now we move the top left to (0,0) and then to (x,y)
  (def F ((shifted x y)
          (shifted (- left) (- top))))
  (def fixed-cs (map F cs)) ; still in device coords
  ; so we transform back to logical coordinates
  (def log-cs (map S fixed-cs))
  
  (def p0 (F (pt left top)))
  (def p1 (F (pt (+ left width) (+ top height))))
  (def bbox (window-from-opposite-corners (S p0) (S p1)))
  (cond
    [bbox? (values log-cs bbox)]
    [else  log-cs]))


(define (subpath->bezs sub)
  ; sub is a list of vectors  
  (define (point v) (defm (vector x y) v) (pt x y))
  (cond
    [(empty? sub) '()]
    [else  (def start (point (first sub))) 
           (let loop ([prev start]
                      [bezs '()]
                      [vecs (rest sub)])
             (match vecs
               ['()
                ; end of subpath
                (if (equal? start prev)
                    (reverse bezs)
                    ; connect the last point to the first point with a line
                    (reverse (cons (bez prev prev start start) bezs)))]
               [(list (and v (vector x y)) more ...)
                ; connect previous (end)point with line
                (def p (point v)) ; new end point
                (loop p 
                      (cons (bez prev prev p p) bezs)
                      more)]
               [(list (and v (vector x1 y1 x2 y2 x3 y3)) more ...)
                ; connect previous point with a bezier curve
                (def p1 (pt x1 y1))
                (def p2 (pt x2 y2))
                (def p3 (pt x3 y3))
                (def b  (bez prev p1 p2 p3))
                (loop p3 (cons b bezs) more)]))]))

(define (closed-subpath->curve sub)
  (curve: #t (subpath->bezs sub)))

(define (open-subpath->curve sub)
  (curve: #f (subpath->bezs sub)))

(define (dc-path->curves p)
  (defv (closeds open) (send p get-datum))
  (append (map closed-subpath->curve closeds)
          (list (open-subpath->curve open))))
