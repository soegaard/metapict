#lang scribble/manual
@(require (for-label metapict (except-in racket unit angle box open path? identity ...))
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@title[#:tag "ref-trans"]{Transformations}

@defmodule[metapict/trans]

@; ----------------------------------------

Given a curve @math{c}, one can use a transformation to calculate a new curve @math{c'}
with a different shape or placement. Transformations such as scaling, reflection, 
rotation and translation appear again and again in graphics programs. These transformations 
are all @emph{affine transformations}. An important property of affine transformations is 
that the composition of two affine transformations is a new affine transformation.
If a series of transformations are to be applied to a large set of points or curves,
it is more efficient to compute the composed transformation first, and then apply that
to the large set.

In MetaPict a transformation can be applied to instances of @racket[curve], @racket[pt], 
@racket[vec] and @racket[bez]. Furthermore a transformation can be applied to another transformation.


@interaction-eval[#:eval eval (set-curve-pict-size 100 100)]
@interaction[#:eval eval
  (def c (curve (pt -1/2 1/2) -- (pt -1/2 0) -- (pt 1/2 0)))
  (draw (color "black" (draw ((shifted 1/2 1/2) c)))
        (color "red"   (draw ((shifted 1/2 1/2) c)))
        (color "blue"  (draw ((rotated (/ π 2)) c)))
        (color "green" (draw (((shifted 0 -0.1) flipy) c))))]

Mathematically an affine transformation transforms a point @math{(x,y)} into the point

@centered{
@math{x' = ax + cy + e} @linebreak[]
@math{y' = bx + dy + f}}

where a, b, c, d, e and f are real numbers.


@defstruct[trans ([a real?] [b real?] [c real?] [d real?] [e real?] [f real?])]{
The structure @racket[trans] represents the affine transformation:

@centered{
@math{x' = ax + cy + e} @linebreak[]
@math{y' = bx + dy + f}}

The @racket[trans] struct is applicable and can be applied to 
instances of @racket[curve], @racket[pt], @racket[vec] and @racket[bez].
Also a @racket[trans] can be applied to another @racket[trans].

The most used transformations have specialized constructors (see below),
so @racket[trans] is seldom used directly.


@interaction[#:eval eval
  (def t (shifted 10 20))
  (t (pt 1 2))
  (t (vec 1 2))
  (t (curve (pt 1 2) -- (pt 3 4)))
  t
  (t t)
  ((t t) t)]
}

@defthing[identity trans? #:value (trans 1 0 0 1 0 0)]{
  The identity transformation.}

@defthing[rotated90 trans? #:value (trans 0 1 -1 1 0 0)]{
  Rotate 90 degrees counterclockwise. }

@defthing[rotated180 trans? #:value (trans -1 0 0 -1 0 0)]{
  Rotate 180 degrees counterclockwise. }

@defthing[rotated270 trans? #:value (trans 0 -1  1  0 0 0)]{
  Rotate 270 degrees counterclockwise. }

@interaction[#:eval eval
  (def c (curve (pt 0 0) -- (pt 1 0)))
  (penwidth 4
    (draw (color "black" (draw (identity   c)))
          (color "red"   (draw (rotated90  c)))
          (color "blue"  (draw (rotated180 c)))
          (color "green" (draw (rotated270 c)))))]


@defthing[flipx trans? #:value (trans 1  0  0 -1 0 0)]{
Reflect about the x-axis.}

@defthing[flipy trans? #:value (trans -1  0  0 1 0 0)]{
Reflect about the y-axis.}

@interaction[#:eval eval
  (def c (curve (pt 1/4 1/4) -- (pt 1 1)))
  (penwidth 4
    (draw (color "black" (draw (identity c)))
          (color "red"   (draw (flipx    c)))
          (color "blue"  (draw (flipy    c)))))]


@defproc[(slanted [a real?]) 
         trans?]{
The transformation @racket[(trans 1 0 a 1 0 0)]. @linebreak[]
@centered{@math{(x,y)} slanted a = @math{(x+ay,y)}}.
}

@interaction[#:eval eval
  (def c (curve (pt 0 0) -- (pt 0 1)))
  (penwidth 4
    (draw (color "black" (draw (identity    c)))
          (color "red"   (draw (slanted 1/3 c)))
          (color "blue"  (draw (slanted 1/2 c)))
          (color "green" (draw (slanted 1   c)))))]


@defproc[(scaled [a real?]) 
         trans?]{
The transformation @racket[(trans a 0 0 a 0 0)]. @linebreak[]
@centered{@math{(x,y)} scaled a = @math{(ax,ay)}}
}

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/100 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw             c))
          (color "red"   (draw (scaled 3/4 c)))
          (color "blue"  (draw (scaled 2/4 c)))
          (color "green" (draw (scaled 1/4 c)))))]

@defproc[(xscaled [a real?]) 
         trans?]{
The transformation @racket[(trans a 0 0 1 0 0)]. @linebreak[]
@centered{@math{(x,y)} xscaled a = @math{(ax,y)}}


@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/100 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw              c))
          (color "red"   (draw (xscaled 1/2 c)))))]
}

@defproc[(yscaled [a real?]) 
         trans?]{
The transformation @racket[(trans 1 0 0 a 0 0)]. @linebreak[]
@centered{@math{(x,y)} yscaled a = @math{(x,ay)}}

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/100 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw              c))
          (color "red"   (draw (yscaled 1/2 c)))))]
}

@defproc[(shifted [a real?] [b real?]) 
         trans?]{
The transformation @racket[(trans 1 0 0 1 a b)]. @linebreak[]
@centered{@math{(x,y)} yscaled a = @math{(x+a,y+b)}}

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw                  c))
          (color "red"   (draw (shifted 1/2 0   c)))
          (color "blue"  (draw (shifted 0   1/2 c)))))]
}

@defproc[(zscaled [a real?] [b real?]) 
         trans?]{
The transformation @racket[(trans a b -b a 0 0)]. @linebreak[]
@centered{@math{(x,y)} zscaled a b = @math{(ax-ay,bx+ay)}}

Note:  (1,0) zscaled (a,b) = (a,b) , thus (1,0) is rotated and 
scaled into (a,b). Think of zscaled as "complex multiplaction".

In the example the red curve is multiplied with @math{0+1i}, which
corresponds to a rotation of 90 degrees.

In the example the blue curve is multiplied with @math{1+1i}, which
corresponds to a rotation of 45 degrees. Also the since the
magnitude of @math{1+1i} is @math{sqrt(2)} the figure is scaled with
the factor @math{sqrt(2)}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw              c))
          (color "red"   (draw (zscaled 0 1 c)))
          (color "blue"  (draw (zscaled 1 1 c)))))]
}

@defproc[(rotated [θ real?]) 
         trans?]{
The transformation @racket[(rotated theta)] is a rotation of θ 
radian around @math{(0,0)}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 0 ; center in (0,100)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw             c))
          (color "red"   (draw (rotated 3.141592653589793 c)))))]
}

@defproc[(rotatedd [θ real?]) 
         trans?]{
The transformation @racket[(rotatedd theta)] is a rotation of θ 
degrees around @math{(0,0)}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 0 ; center in (0,100)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
  (penwidth 4
    (draw (color "black" (draw               c))
          (color "red"   (draw (rotatedd 180 c)))))]
}

@defproc[(rotated-about [θ real?] [p pt?]) 
         trans?]{
The transformation @racket[(rotated θ)] is a rotation of θ 
radian around the point @math{p}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))  
  (penwidth 4
    (draw (color "black" (draw               c))
          (color "red"   (draw (rotated-about 3.14159 (pt 0.25 0) c) 
                                                      (pt 0.25 0)))))]
}


@defproc[(rotatedd-about [θ real?] [p pt?]) 
         trans?]{
The transformation @racket[(rotatedd-about θ p)] is a rotation of θ 
degrees around the point @math{p}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))  
  (penwidth 4
    (draw (color "black" (draw               c))
          (color "red"   (draw (rotatedd-about 180 (pt 0.25 0) c) 
                                                   (pt 0.25 0)))
          (color "blue"  (draw (rotatedd-about  90 (pt 0.25 0) c) 
                                                   (pt 0.25 0)))))]
}

@defproc[(reflected [p pt?] [q pt?])
         trans?]{
The transformation @racket[(reflected p q)]
reflects in the line @math{l} through the points @math{p} and 
@math{q}.

@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 0 ; center in (0,100)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))
    (draw (curve (pt -1 -1) -- (pt 1 1))
          (penwidth 4
            (draw 
             (color "red"   (draw               c))
             (color "blue"  (draw (reflected (pt -1 -1) (pt 1 1) c))))))]
}

@defproc[(inverse [t trans?])
         trans?]{
If the transformation @racket[t] is invertible, 
then @racket[(inverse t)] is the inverse transformation of
 @racket[t].


@interaction[#:eval eval
  (def c ; a heart shaped curve
    (scaled 1/200 ; from -1 to 1
      (shifted -100 -100 ; center in (0,0)
          (curve (pt 100 162) .. (pt 140 178) right .. (pt 195 125) down
                 .. (pt 100 0) (curl 0) .. up (pt 5 125) 
                 .. right (pt 60 178) .. (pt 100 162)))))  
  (def t (shifted 1/2 0))
  (def s (inverse t))
  (penwidth 4
    (draw (color "black" (draw c))
          (color "red"   (draw (t c)))
          (color "blue"  (draw (s c)))))]

}

@defproc[(trans~ [t trans?] [s trans?] [ε real? 1e-15])
         boolean?]{
Compares the two transformations.                   

@interaction[#:eval eval
  (def t (shifted 1 0))
  (def s (shifted 2 0))
  (trans~ (t t) s)]
}

@defproc[(compose-trans [t trans?] [s trans?])
         trans?]{
Compose two transformations. Same as @racket[(t s)].
}

@defproc[(trans->vector [t trans?] )
         vector?]{
Compose two transformations. Same as @racket[(t s)].

@interaction[#:eval eval
  (def t (shifted 42 0))
  (trans->vector t)]

}

@defproc[(trans->transformation [t trans?])
         vector?]{
Return a transformation that can be used with the @racket[set-transformation]
method of a drawing context.

@interaction[#:eval eval
  (def t (shifted 42 0))
  (trans->transformation t)]
}






