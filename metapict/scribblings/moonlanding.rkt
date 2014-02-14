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
  (displayln args)
  (cond [(and bm (send bm ok?))
         (def w (send bm get-width))
         (def h (send bm get-height))
         (defv (x0 y0 width) ; (x0,y0) in logical coordinates
           (match args
             [(or (list x0 y0 width) (list (pt x0 y0) width)) 
              (displayln (list 'a x0 y0 width))
              (values x0 y0 width)]
             [(or (list x0 y0) (list (pt x0 y0)))
              (displayln (list 'b x0 y0))
              (values x0 y0 w)]
             [(list)
              (values 0 0 w)]
             [_ (error 'mpbitmap (~a "yada yada, got: " args))]))
         (def T (stdtrans (curve-pict-window) w h))
         (dc (lambda (dc dx dy) ; (x,y) in device coordinates
               (defm (pt x y) (T (pt dx dy)))
               (displayln (list 'dx dx 'dy dy 'x x 'y y 'w w 'h h))
               (send dc draw-bitmap bm x y
                     'solid black-color ; only relevant for monochrome images
                     (send bm get-loaded-mask)))
             w h)]
        (frame (inset (colorize (text "bitmap failed") "red") 2))))

(with-window (window -1 1 -1 1)
  (def bm (mpbitmap "moonlanding-scott-salutes-flag.jpg" 0 0))
  (def w (pict-width  bm))
  (def h (pict-height bm))
  (set-curve-pict-size w h)
  (def circ (shifted (/ w 2) (/ h 2) (scaled (/ w 2) unitcircle)))
  (draw (clipped bm circ)
        (color (change-alpha "red" 0.5) 
               (grid (pt 0 0) (pt w h) (pt 0 0) 20))))



