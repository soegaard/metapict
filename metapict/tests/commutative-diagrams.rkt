#lang racket
(require "../main.rkt" "../parameters.rkt" "../pict.rkt")


(curve-pict-window (window -0.5 2.5 -2.5 0.5))
(set-curve-pict-size  400 400)
(current-node-size 0.1)
(current-neighbour-distance 0.8)
(current-label-gap 0.1)
(ahlength 0.1)


(def A (text-node "A"))
(def B (text-node "B" #:right-of A))
(def C (text-node "C" #:below B))
(def D (text-node "D" #:right-of C))


(font-italic
   (margin 5
           (draw ; (color "gray" (grid (pt -3 -3) (pt 3 3)))
                 A B C D
                 (edge A B #:label "f")
                 (edge A C #:label "g○f")
                 (edge B C #:label "g"   #:arrow '-)
                 (edge B D #:label "h○g" #:arrow '<-) 
                 (edge C D #:label "h"   #:arrow '<->))))

;;; Example: A simple cycle

(curve-pict-window (window -2 2 -2 2))

(def n 5)  ; number of nodes in the cycle

; coordinates of the i'th point in the cycle
(define (point i)
  (def θ (* i (/ 2π n)))
  (pt@ 1.2 θ)) ; polar coordinates, radius=1 angle=θ

; return the next index   0 -> 1 -> ... -> n -> 0
(define (next i)
  (remainder (+ i 1) n))

; the nodes in the cycle
(def nodes (for/list ([i n])
             (circle-node (~a (+ i 1)) #:at (point i))))

; the i'th node
(define (cycle-node i)
  (list-ref nodes i))

; use straight arrows as edges for the first drawing
(def edges (for/list ([i 5])
             (edge (cycle-node i)
                   (cycle-node (next i)))))

(draw (draw* nodes)
      (draw* edges))


;;; Example: A simple cycle (now edges form a circle)

(curve-pict-window (window -2 2 -2 2))

(define (tangent-vector i)
  (rot90 (pt- (point i) origo)))

(def edges2 (for/list ([i 5])
              (edge (cycle-node i)
                    (cycle-node (next i))
                    (tangent-vector i)
                    (vec* -1 (tangent-vector (next i))))))
                   

(draw (draw* nodes)
      (draw* edges2))



;;; Zasssenhaus' Lemma

(def L1 (text-node "A⃰"))
(def L2 (text-node "A(A⃰∩B)"))
(def L3 (text-node "A(A⃰∩B)"))
(def L4 (text-node "A"))

(def M1 (text-node "A⃰∩B⃰"))
(def M2 (text-node "(A∩B⃰)(A⃰∩B)"))

(def R1 (text-node "B⃰"))
(def R2 (text-node "B(A⃰∩B)"))
(def R3 (text-node "A(A⃰∩B)"))
(def R4 (text-node "A"))

;;;

;;; Example: Chains with labeled edges

;; An example with an exact sequence.

(font-italic
(let ()
  (set-curve-pict-size  800 400)
  (curve-pict-window (window -1 5 -2 1))

  ; chain : list-of-strings -> list-of-nodes
  ;   return a list of text nodes placed horizontally 
  (define (chain xs)
    (reverse
     (for/fold ([ns (list (text-node (first xs)))])
               ([x  (rest xs)])
       (cons (text-node x #:right-of (first ns))
             ns))))

  ; chain-below : list-of-nodes list-of-strings -> list-of-nodes
  ;  return a list of nodes places below the nodes in ns
  ;  using labels from the strings in xs
  (define (chain-below ns xs)
    (for/list ([x xs] [n ns])
      (text-node x #:below n)))

  ; edges : list-of-nodes strings -> list-of-edges
  ;  return a list of horizontal edges between the nodes in the list chain,
  ;  use the strings in labels as labels (an #f means no label)
  (define (edges chain labels)
    (for/list ([n1 chain] [n2 (rest chain)] [l labels])
      (if l
          (edge n1 n2 #:label l)
          (edge n1 n2))))

  ; edges-vert : list-of-nodes list-of-nodes list-of-string -> list-of-edges
  ;   return a list of vertical edges from the nodes in ns to those in ms
  ;   using the labels
  (define (edges-vert ns ms labels)
    (for/list ([n ns] [m ms] [l labels]
                      #:when l)
      (edge n m #:label l)))

  ;; The diagram: A "ladder".
  (def upper (chain             '("0" "A"  "B"  "C"  "0")))
  (def lower (chain-below upper '("0" "A'" "B'" "C'" "0")))
  (draw* (append upper lower
                 (edges upper             '(#f "φ"  "ψ"  #f))
                 (edges lower             '(#f "φ'" "ψ'" #f))
                 (edges-vert upper lower  '(#f "η₁" "η₂" "η₃" #f))))))



(let ()
  (set-curve-pict-size  600 600)
  (curve-pict-window (window -2 2 -2 2))

  (def A (text-node "A" #:at (pt -1 0)))
  (def B (text-node "B" #:at (pt  1 0)))

  (draw A B
        (edge A B #:via (pt 0 1) #:label "AB")
        (edge A B #:via (list (pt 0 -0.5)) #:label "AB")))





