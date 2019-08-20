#lang racket

(require metapict metapict/path-operations)


(draw-arrow (curve (pt 0 0) /- (pt 1/2 1/2) -/ (pt 1 -1)))
(draw-arrow (curve (pt 0 0) --++ (vec 1/2 1) --++ (vec 1/2 -1)))

