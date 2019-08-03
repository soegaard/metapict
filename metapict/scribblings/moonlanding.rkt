#lang racket 
(require metapict racket/draw racket/gui/dynamic)

(define black-color (make-object color% 0 0 0))

(define (mpbitmap filename/bitmap% . args)
  (def f filename/bitmap%)
  (def bm (cond [(is-a? f bitmap%) f]
                [(path-string? f)  (make-object bitmap% f 'unknown/alpha)]
                [(and (gui-available?) (is-a? f (gui-dynamic-require 'image-snip%)))
                 (send f get-bitmap)]
                [else #f]))
  (cond [(and bm (send bm ok?))
         (def w (send bm get-width))
         (def h (send bm get-height))
         (defv (x0 y0 width height) ; (x0,y0) and width in logical coordinates
           (match args
             [(or (list x0 y0 width height) (list (pt x0 y0) width height))
              (values x0 y0 width height)]
             [(or (list x0 y0 width) (list (pt x0 y0) width))
              (values x0 y0 width #f)]
             [(or (list x0 y0) (list (pt x0 y0)))
              (displayln (list 'b x0 y0))
              (values x0 y0 w h)] ; todo ...
             [(list)
              (values 0 0 w #f)] ; todo
             [_ (error 'mpbitmap (~a "yada yada, got: " args))]))
         (def win (curve-pict-window))
         (def T (stdtrans win w h)) ; logical -> device
         (defm (window xmin xmax ymin ymax) win)
         (def lw (- xmax xmin)) ; logical width
         (def lh (- ymax ymin)) ; logical height
         (dc (lambda (dc dx dy) ; (x,y) in device coordinates
               (defm (pt x y) (T (pt x0 y0)))
               (defm (pt dw _) (T (pt width 0)))
               (displayln (list 'dx dx 'dy dy 'x x 'y y 'w w 'h h 'dw dw))
               (defv (old-scale-x old-scale-y) (send dc get-scale))
               (def sx (* (/ width  lw) (/ (curve-pict-width)  w)))
               (def sy (if height 
                           (* (/ height lw) (/ (curve-pict-height) w))
                           sx))                           
               (send dc set-scale sx sy)
               (send dc draw-bitmap bm (/ (+ dx x) sx) (/ (+ dy y) sy)
                     'solid black-color ; only relevant for monochrome images
                     (send bm get-loaded-mask))
               (send dc set-scale old-scale-x old-scale-y))
             w h)]
        (frame (inset (colorize (text "bitmap failed x") "red") 2))))

(with-window (window -1 1 -1 1)
  (def bm (mpbitmap "moonlanding-scott-salutes-flag.jpg" 0 0))
  (def w (pict-width  bm))
  (def h (pict-height bm))
  (set-curve-pict-size w h)
  (def circ (shifted (/ w 2) (/ h 2) (scaled (/ w 2) unitcircle)))
  (draw (clipped circ bm)
        (color (change-alpha "red" 0.5) 
               (grid (pt 0 0) (pt w h) (pt 0 0) 20))))

(with-window (window -1 1 -1 1)
  (draw (mpbitmap "moonlanding-scott-salutes-flag.jpg" -1/4 -1/4 1/8)
       ; (mpbitmap "moonlanding-scott-salutes-flag.jpg"  1/4  1/4 1/8)
        (grid (pt -1 -1) (pt 1 1) (pt 0 0) 1/4)))

