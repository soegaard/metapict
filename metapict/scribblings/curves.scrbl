#lang scribble/manual
@(require scribble/core scribble/html-properties)
@(require (for-label (except-in racket angle box open path? identity ...)
                      metapict)
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          "utils.rkt")

@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]

@(require (except-in metapict table) racket/format)
@(define example-points
     (let () 
       (set-curve-pict-size 60 30)
       (defv (p1 p2 p3 p4 p5 p6) 
         (values (pt 0 100) (pt 100 100) (pt 200 100) (pt 0 0) (pt 100 0) (pt 200 0)))
       (define (p n x y) (dot-label (~a "p" n) (pt x y)))
       (with-window (window -10 210 -5 105)
         (margin 20
                 (draw (p 1 0 100)
                       (p 2 100 100)
                       (p 3 200 100)
                       (p 4 0     0)
                       (p 5 100   0)
                       (p 6 200   0))))))

@title{Coordinates}
@; ----------------------------------------
@section[#:tag "coordinates" #:style svg-picts]{Curves}

All shapes in MetaPicts are built of curves. There are several ways
of describing curves, but the simplest and most common is to 
describe some points on the curves and how to connect them.

@margin-note[example-points]
In the chapter on points, we used @racket[(draw (curve p1 .. p6))] 
to connect the points @racket[p1] and @racket[p6] with a straight line.
If more than two points are connected with the connector @racket[..],
then MetaPict will connect the points with a "pretty", smooth curve.

@interaction-eval[#:eval eval 
                         (begin (set-curve-pict-size 100 50)
                                (defv (p1 p2 p3 p4 p5 p6)
                                  (values (pt 0 100) (pt 100 100) (pt 200 100) 
                                          (pt 0 0) (pt 100 0) (pt 200 0)))
                                (defv (xmin xmax ymin ymax) (values -110 310 -55 155))
                                (curve-pict-window (window xmin xmax ymin ymax))
                                (def red-todo-fix
                                  (pencolor 
                                   "red" 
                                   (draw 
                                    (fill 
                                     (curve (pt xmin ymin) -- (pt xmin ymax)
                                            -- (pt xmax ymax) -- (pt xmax ymin) -- cycle))))))]

@interaction[#:eval eval (draw (curve p4 .. p1 .. p2 .. p6))]
@interaction[#:eval eval (draw (curve p5 .. p4 .. p1 .. p3 .. p6 .. p5))]
The last curve is not smooth at @racket[p5] which is the start and ending point.
To modify the expression to get a smooth, closed curve, use @racket[cycle] as
the "end point" rather than repeat the start point.
@interaction[#:eval eval (draw (curve p5 .. p4 .. p1 .. p3 .. p6 .. cycle))]

There are various ways of controlling the curve. Adding a direction after
a point will make the curve leave the point in that direction.

@interaction[#:eval eval "TODO - fix me" 
                    (draw
                     red-todo-fix
                     (curve p5 .. p4 left .. p1 .. p3 .. p6 left .. cycle))]

@interaction[#:eval eval (draw (curve p5 .. p4 left .. p1 .. p3 .. p6 left .. p5 ))]

Here @racket[left] stands for the direction @racket[(vec -1 0)]. You can use
any vector you like. The expression @racket[(draw (curve p1 .. p2 (pt- p3 p1) .. p3))]
will draw a curve from @racket[p1] through @racket[p2] to @racket[p3] and the 
tangent in @racket[p2] will be parallel with the vector from @racket[p1] to @racket[p3].

@interaction[#:eval eval (draw (curve p4 .. p2 (pt- p3 p4) .. p3))]

Let us connect two points and see what happens when we vary the direction
from which the curve leaves the first point. We use @racket[(dir d)] to
specify a unit vector whose angle with the x-axis are @racket[d] degrees.

@interaction[#:eval eval (draw* (for/list ([d (in-range 0 100 10)])
                                  (curve (pt 0 0) (dir d) .. (pt 300 0))))]

Here the function @racket[draw*] draws all curves in a list on the same pict.
We notice that MetaPict thinks "circle arcs" are the pretties way of 
joining two point, when only one direction is specified.

Let us see what happens when we fix the direction in which the curve enters
the end point.
@interaction[#:eval eval (draw* (for/list ([d (in-range 0 100 15)])
                                  (curve (pt 0 0) (dir d) .. right (pt 300 0))))]

You can use @racket[curl] to change the how much the curve "curls" at the start and
end point.

@interaction[#:eval eval "TODO - fix this" 
                    (draw red-todo-fix
                          (curve (curl 0) p4 .. p2 (pt- p3 p4) .. p3 (curl 3)))
                    (list (draw (curve  p4 (curl 0) .. p6))
                          (draw (curve  p4 (curl 1) .. p6))
                          (draw (curve  p4 (curl 2) .. p6))
                          (draw (curve  p4 (curl 3) .. p6)))]


@interaction[#:eval eval 
                    "TODO - fix this" 
                    (draw red-todo-fix
                          (curve p4 (controls p1 p2) p6))]
