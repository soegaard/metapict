#lang scribble/manual
@(require scribble/core scribble/html-properties)
@(require (for-label (except-in racket angle open path? identity ...))
          scribble/extract
          scribble/eval
          scribble/base
          scribble/manual
          "utils.rkt")

@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)
@title{Coordinates}
@; ----------------------------------------
@section[#:tag "coordinates" #:style svg-picts]{Points}

In order to make a computer draw a shape, a way to specify the key points of 
the shape is needed. @margin-note{Note: This is different from @racket[racket/pict] 
which reverses the direction of the y-axis.}
MetaPict uses standard @math{(x,y)}-coordinates for this purpose. 
The location of a point is always relative to the reference point @math{(0,0)}.
The @math{x}-coordinate of a point is the number of units to the right of the reference point.
The @math{y}-coordinate of a point is the number of units upward from the reference point.

Consider these points:
@interaction-eval[#:eval eval (set-curve-pict-size 100 50)]
@interaction-eval-show[#:eval eval (let ()
                           (define (p n x y) (dot-label (~a "p" n) (pt x y)))
                           (with-window (window -10 210 -5 105)
                           (margin 20
                                   (draw (p 1 0 100)
                                         (p 2 100 100)
                                         (p 3 200 100)
                                         (p 4 0     0)
                                         (p 5 100   0)
                                         (p 6 200   0)))))]

The coordinates of these points are:
@tabular[#:sep @hspace[2]
         (list (list @math{p1=(0,100)} @math{p2=(100,100)} @math{p3=(200,100)})
               (list @math{p4=(0,0)}   @math{p5=(100,0)}   @math{p6=(200,0)}))]

Notice that the point @math{p4=(0,0)} is the reference point.
The point @math{p3=(200,100)} is located 200 units to the right of @math{p4} 
and 100 units upwards.

In order to write a MetaPict program to draw a shape, a good strategy is
to draw the shape on paper. Determine the coordinates for the key points, and
then write the MetaPict program that draws lines or curves between the points.

Let us write such a program, that connects point @math{p1} and @math{p6}.
@interaction[#:eval eval 
                    (with-window (window -10 210 -5 105)
                      (draw (curve (pt 0 100) .. (pt 200 0))))]

The @racket[..] between the two points connects the two points with a line.

If we are to use the points repeatedly, it is better give them names.
@margin-note{@racket[def] is shorthand for @racket[define]}
@interaction[#:eval eval 
                    (def p1 (pt   0 100))
                    (def p2 (pt 100 100))
                    (def p3 (pt 200 100))
                    (def p4 (pt   0   0))
                    (def p5 (pt 100   0))
                    (def p6 (pt 200   0))
                    (with-window (window -10 210 -5 105)
                      (draw (curve p1 .. p6)))]
Let us connect the point @math{p2} with @math{p5} and @math{p3} with @math{p4}.
@interaction[#:eval eval 
                    (with-window (window -10 210 -5 105)
                      (draw (curve p1 .. p6)
                            (curve p2 .. p5)
                            (curve p3 .. p4)))]

If you zoom, you will see that the lines have a thickness and
that the ends are rounded. Imagine that you have a pen with
a circular nib. The drawings produced by MetaPict will try
to mimick the result you get by drawing with such a pen.
In the chapter on pens you will learn to the control the 
thickness of the pen and the shape of the ends of lines.

@section[#:tag "displacements"]{Displacements}

In the example above the point @math{p2=(100,100)} was described as being 100 to 
the right and 100 upwards relative to the reference point (0,0).

An alternative way of describing the location of @math{p2} would be to say
that is located 100 to the right of @math{p1} (and 0 upwards). 

@interaction-eval-show[#:eval eval 
                              (let ()
                                (define t text)
                                (ahlength 8)
                                (define (p n x y) (dot-label (~a "p" n) (pt x y)))
                                (def p1->p2 (curve (pt 10 100) .. (pt 90 100)))
                                (define (cross p) 
                                  (draw (curve (pt- p (vec 1  1)) .. (pt+ p (vec  1 1)))
                                        (curve (pt- p (vec -1 1)) .. (pt+ p (vec -1 1)))))
                                (with-window (window -10 210 -5 105)
                           (margin 20
                                   (draw (p 1 0 100)
                                         (p 2 100 100)
                                         (p 3 200 100)
                                         (p 4 0     0)
                                         (p 5 100   0)
                                         (p 6 200   0)
                                         (label (beside (t "v=") (~vec (vec 0 100))) 
                                                (point-of p1->p2 0.5) (top))
                                         (draw-arrow (curve (pt 10 100) .. (pt 90 100)))))))]

Such a displacement can be  described with a vector. Since Racket uses the name 
"vector", we will represent displacement vectors with a @racket[vec] structure.
To displace a point @math{p} with a vector @math{v}, use @racket[pt+].

@interaction[#:eval eval 
                    (def v (vec 100 0))
                    (def p1 (pt 0 100))
                    (def p2 (pt+ p1 v))
                    (def p3 (pt+ p2 v))
                    (def p4 (pt 0 0))
                    (def p5 (pt+ p4 v))
                    (def p6 (pt+ p5 v))
                    (with-window (window -10 210 -5 105)
                      (draw (curve p1 .. p6)
                            (curve p2 .. p5)
                            (curve p3 .. p4)))]

@margin-note{Note: @racket[defv] is short for @racket[define-values].}
The point @math{p3} is @math{p1} displaced by @math{v} twice. Use 
@racket[vec*] to compute @math{2v}. At the same time, let's use
@racket[defv] to define multiple points at a time.

@interaction[#:eval eval 
                    (def  v (vec 100 0))
                    (def 2v (vec* 2 v))
                    (defv (p1 p2 p3) (values (pt 0 100) (pt+ p1 v) (pt+ p1 2v)))
                    (defv (p4 p5 p6) (values (pt 0   0) (pt+ p4 v) (pt+ p4 2v)))
                    (with-window (window -10 210 -5 105)
                      (draw (curve p1 .. p6) (curve p2 .. p5) (curve p3 .. p4)))]

The displacements @racket[left], @racket[right], @racket[up], and, @racket[down].
are predefined. As are the vector operations @racket[vec+],@racket[vec-], and, @racket[vec*].
The displacement that moves a point @racket[a] to point @racket[b] is given by @racket[(pt- b a)].

@interaction[#:eval eval
             (list left right up down)
             (vec+ left up)
             (vec- left up)
             (vec* 3 right)
             (pt- (pt 2 4) (pt 7 8))]

It is common to need points that lie between two point @racket[A] and @racket[B].
The mediation operation is called @racket[med]. The call @racket[(med 0.25 A B)]
will compute the point @racket[M] on the line from @racket[A] to @racket[B] whose 
distance from @racket[A] is 25% of the length of @racket[AB].

@interaction[#:eval eval 
                    (def A (pt 0 0))
                    (def B (pt 3 2))
                    (with-window (window -1 4 -1 3)
                      (draw (dot-label "A" A              (top))
                            (dot-label "C" (med 0.25 A B) (top))
                            (dot-label "D" (med 0.50  A B) (bot))
                            (dot-label "E" (med 0.75 A B) (bot))
                            (dot-label "B" B              (bot))))]

Note: @racket[(med x A B)] is equivalent to @racket[(pt+ A (vec* x (pt- B A)))].

Let's use the knowledge from this section to write a small program to generate
the character A. The shape depends on the parameters @racket[w] (width),
@racket[h] (height) and the placement of the bar @racket[α].

@interaction[#:eval eval 
                    (define (A w h α)
                      (set-curve-pict-size w h)
                      (def p1 (pt    0    0))
                      (def p2 (pt (/ w 2) h))
                      (def p3 (pt    w    0))
                      (def p4 (med α p1 p2))
                      (def p5 (med α p3 p2))
                      (with-window (window 0 w 0 h)
                        (draw (curve p1 .. p2)
                              (curve p2 .. p3)
                              (curve p4 .. p5))))
                      (list (A 10 20 0.3)
                            (A 10 20 0.4)
                            (A 10 20 0.5)
                            (A 10 20 0.6))]



