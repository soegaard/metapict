#lang racket
(require graph metapict json)

(define ref hash-ref)

;(define scale 20)  ; 72 ?

;;;
;;; Required properties
;;;

; strict:   If #t then multiple edges between two nodes are disallowed
; directed: If #t then the graph is directed (but individual node can have dir=none
; _subgraph_cnt : the number of subgraphs (at the beginning) of the objects array

(define (name l)           (ref l 'name))
(define (directed l)       (ref l 'directed))
(define (strict l)         (ref l 'strict))
(define (subgraph-count l) (ref l '_subgraph_cnt))

(define (bounding-box l)
  (define s (ref l 'bb))
  (match (parse-number-string s)
    [(list xmin ymin xmax ymax)
     (window xmin xmax ymin ymax)]))

(define (edges l)   (hash-ref l 'edges))
(define (objects l) (hash-ref l 'objects))

(define (convert-objects l)
  (define objects-ht (make-hash))
  (define (add-object! i obj)
    (hash-set! objects-ht i obj))
  (define (convert-nodes nodes)
    (map (λ (n) (convert-node n add-object!)) nodes))

  (define subgraphs (take (objects l) (subgraph-count l)))
  (define nodes     (drop (objects l) (subgraph-count l)))  
  (convert-nodes nodes)
  objects-ht)

(define (convert-node node add!)
  ; todo: use width and height
  (define scale 72) ; 72 points in an inch
  (define gvid   (ref node '_gvid))
  (define height (* scale (string->number (ref node 'height))))
  (define width  (* scale (string->number (ref node 'width))))
  (define label  (ref node 'label))
  (define name   (ref node 'name))
  (define pos-str (ref node 'pos))
  (define pos     (apply pt (parse-number-string pos-str)))  
  ; (define n (rectangle-node label
  ;  #:at pos
  ;   #:width width #:height height))
  #;(define n (circle-node (if (equal? label "") " " label)
                           #:at pos
                           #:min-width  (min width height)
                           #:min-height (min width height)))
  (define n (ellipse-node (if (equal? label "") " " label)
                          #:at pos
                          #:min-width  width
                          #:min-height height))
  (add! gvid n))




(define (parse-points s)
  (define coords (parse-number-string s))
  (let loop ([cs coords])
    (match cs
      [(list x y more ...)
       (cons (pt x y) (loop more))]
      [_ '()])))

(define (parse-number-string s)
  ; > (parse-number-string "10,20,30")
  ; (10 20 30)
  (with-input-from-string s
    (λ ()
      (let loop ()
        (define n (read))
        (cond
          [(eof-object? n) '()]
          [(list? n)       (match n
                             [(list 'unquote n) (cons n (loop))]
                             [_ (error)])]
          [(number? n)     (cons n (loop))]
          [(eq? n 'e)      ; indicate an endpoint - ignore for now
           (displayln "EEEE")
                           (loop)]
          [else            (displayln n)
                           (error 'parse-number-from-string
                                  (~a "unexpected input: " n))])))))

(define (parse-spline-type s)
  (define end-point   #f)
  (define start-point #f)
  (define point       #f)
  (define triples     '())
  (define (read-unquote-number)
    (match (read)
      [(list 'unquote n) n]
      [x (error 'parse-spline-type (~a "unquote expected, got: " x))]))
  (define (read-point)
    (define x (read))
    (cond
      [(eof-object? x) #f]
      [else            (define y (read-unquote-number))
                       (pt x y)]))
  (define (read-triple)
    (define p1 (read-point))
    (and p1
        (list p1 (read-point) (read-point))))
  (with-input-from-string s
    (λ ()
      (define peek (read))
      (when (eq? peek 'e) ; an endpoint
        (define x (read-unquote-number))
        (define y (read-unquote-number))
        (set! end-point (pt x y))
        (set! peek (read)))
      (when (eq? peek 's) ; a start point
        (define x (read-unquote-number))
        (define y (read-unquote-number))
        (set! start-point (pt x y))
        (set! peek (read)))
      ; now read the point - but use peek (not read-point)
      (let ()
        (define x peek)
        (define y (read-unquote-number))
        (set! point (pt x y)))
      ; now read some number of triples
      (let loop ()
        (define triple (read-triple))
        (when triple
          (set! triples (append (reverse triple) triples))
          (loop)))
      (set! triples (reverse triples))      
      (define (points->bezs ps)
        (match ps
          [(list p0 p1 p2 p3 more ...)
           (cons (bez p0 p1 p2 p3)
                 (points->bezs (cons p3 more)))]
          [(list _) '()]
          [(list) '()]))
      ; the curve without the arrow(s)
      (define partial-edge (curve: #f (points->bezs (cons point triples))))
      ; if start-point is present - the curve below the arrow head is missing
      ; if end-point   is present - the curve below the arrow head is missing
      (values start-point
              end-point
              partial-edge))))
      
             
; splineType
;   spline ( ';' spline )*
;   where spline	=	(endp)? (startp)? point (triple)+
;   and triple	=	point point point
;   and endp	=	"e,%f,%f"
;   and startp	=	"s,%f,%f"
; If a spline has points p1 p2 p3 ... pn, (n = 1 (mod 3)), the points correspond to
; the control points of a cubic B-spline from p1 to pn. If startp is given, it touches
; one node of the edge, and the arrowhead goes from p1 to startp. If startp is not
; given, p1 touches a node. Similarly for pn and endp.


(define (draw-objects-ht ht)
  (for/draw ([(id obj) (in-hash ht)])
            (draw obj)))

(define (convert-edges l ht)
  (for/list ([e (edges l)])
    (define gvid  (ref e '_gvid))
    (define dir   (ref e 'dir (λ () #f)))
    ; (define pos   (reverse (parse-points (ref e 'pos))))
    ; if start-point is present - we need to draw an arrow head
    ; if end-point   is present - we need to draw an arrow head    
    (define-values (start end partial-edge) (parse-spline-type (ref e 'pos)))
    ; (write (ref e 'pos)) (newline)
    ; (displayln pos) (newline)    
    ; (define pos-  (rest (reverse (rest pos))))
    (define head  (ref e 'head))
    (define tail  (ref e 'tail))
    (define lab   (ref e 'label (thunk #f)))
    (define arrow-type (match dir
                         [#f        '->]
                         ["none"    '-]
                         ["forward" '->]
                         ["back"    '<-]
                         ["both"    '<->]
                         [_ (error)]))
    ; Note: Graphiz has calculated the curve connecting the two nodes,
    ;       so we simply draw the curve. That is we do not use metapict edges.
    (def c partial-edge)
    (draw c
          (and start (draw-arrow (curve (start-point c) -- start)))
          (and end   (draw-arrow (curve (end-point c)   -- end))))))


(define (draw-edges l ht)
  (draw* (convert-edges l ht)))

;;;
;;; Example
;;;

; 1. Create graph
;(define g (unweighted-graph/directed '((a b) (c d) (a c) (f e) (f a) (a f))))
; (define g (unweighted-graph/directed '((a b))))
; (define g (unweighted-graph/undirected '((a b) (c d) (a c) (f e) (f a) (a f))))
; (define g (weighted-graph/undirected '((10 a b) (20 b c))))
; (define g (weighted-graph/directed '((10 a b) (20 b c))))
(define g (matrix-graph [[0 3 8 #f -4]
                         [#f 0 #f 1 7]
                         [#f 4 0 #f #f]
                         [2 #f -5 0 #f]
                         [#f #f #f 6 0]]))
; (graphviz g)

; 2. Write graph to temporary file
(with-output-to-file "graph.dot"
  (λ () (display (graphviz g)))
  #:exists 'replace)

; 3. Calculate layout using Graphviz
(system "/usr/local/bin/dot -Tjson0 graph.dot >graph.json")

; 4. Read layout
(define layout (with-input-from-file "graph.json"
                 (λ () (read-json))))

; 5.
(define w (bounding-box layout))
(curve-pict-window w)

(define ratio (/ (- (window-maxy w) (window-miny w))
                 (- (window-maxx w) (window-minx w))))

(define xsize 100)
(set-curve-pict-size xsize (* xsize ratio))
(define ht (convert-objects layout))
(define label-gap 6) ; used for directed graphs
(ahlength 10)
(margin 5
         (font-italic
          (scale 4
                 (draw (draw-objects-ht ht)
                       (draw-edges layout ht)))))




               
