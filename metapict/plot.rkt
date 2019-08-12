#lang racket
(require metapict (except-in plot inverse line)
         racket/draw racket/contract racket/class
         racket/contract
         ; unstable/contract
         unstable/parameter-group
         unstable/latent-contract/defthing ; from unstable-latent-contract-lib

         ; pict
         plot/private/plot2d/plot-area
         
         plot/private/common/contract
         plot/private/common/draw
         ; plot/private/common/parameters
         plot/private/common/plot-element
         plot/private/common/file-type
         plot/private/plot2d/plot-area
         plot/private/no-gui/plot2d-utils)


(defproc (make-plot/dc 
          [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
          [dc (is-a?/c dc<%>)]
          [x real?] [y real?]
          [width (>=/c 0)] [height (>=/c 0)]

          [#:x-min         x-min   (or/c rational? #f)  #f]
          [#:x-max         x-max   (or/c rational? #f)  #f]
          [#:y-min         y-min   (or/c rational? #f)  #f]
          [#:y-max         y-max   (or/c rational? #f)  #f]
          [#:title         title   (or/c string? #f)    (plot-title)]
          [#:x-label       x-label (or/c string? #f)    (plot-x-label)]
          [#:y-label       y-label (or/c string? #f)    (plot-y-label)]
          [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)])  void?
  
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
    (get-ticks renderer-list bounds-rect))
  
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-legend-anchor  legend-anchor]
                 [plot-background     (change-alpha "white" 0.5)]
                 )
    (define area (make-object 2d-plot-area%
                              bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks dc x y width height))
    (define (plot->dc v) (send area plot-coords->dc v))
    (define (dc->plot v) (send area dc->plot v))
    (define (plot) 
      (parameterize ([plot-title          title]
                     [plot-x-label        x-label]
                     [plot-y-label        y-label]
                     [plot-legend-anchor  legend-anchor]
                     [plot-background     "white" #;(change-alpha "blue" 0.1)])
        (plot-area area renderer-list)))
    (values plot plot->dc dc->plot)))

(define (plotter f pict)
  (def w (pict-width pict))
  (def h (pict-height pict))
  (defm (window xmin xmax ymin ymax) (curve-pict-window))
  (dc (lambda (dc x y)
        (def old-pen   (send dc get-pen))
        (def old-brush (send dc get-brush))
        (defv (plot plot->dc dc->plot)
          (parameterize ([plot-width w] [plot-height  h] [plot-x-label #f] [plot-y-label #f])
            (make-plot/dc (function f xmin xmax) dc x y w h)))
        (displayln (list 'dc->plot (map dc->plot (list (vector 0 0) (vector w h)))))
        (plot)
        (send dc set-pen   old-pen)
        (send dc set-brush old-brush))
      w h))


(set-curve-pict-size 400 400)
(draw (plotter sin (draw))
      (curve (pt -1  0) -- (pt 1 0))
      (curve (pt 0  -1) -- (pt 0 1)))
