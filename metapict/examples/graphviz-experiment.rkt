#lang racket
(require graph metapict json)

(define ref hash-ref)

(define scale 20)  ; 72 ?

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
  (define n (circle-node (if (equal? label "") " " label)
                         #:at pos
                         #:radius (* 0.5 (min width height))))
  (add! gvid n))


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
                           (loop)]
          [else            (displayln n)
                           (error 'parse-number-from-string
                                  (~a "unexpected input: " n))])))))

(define (parse-points s)
  (define coords (parse-number-string s))
  (let loop ([cs coords])
    (match cs
      [(list x y more ...)
       (cons (pt x y) (loop more))]
      [_ '()])))


(define (draw-objects-ht ht)
  (for/draw ([(id obj) (in-hash ht)])
            (draw obj)))

(define (convert-edges l ht)
  (for/list ([e (edges l)])
    (define gvid  (ref e '_gvid))
    (define dir   (ref e 'dir (λ () #f)))
    (define pos   (parse-points (ref e 'pos)))
    (define pos-  (rest (reverse (rest pos))))
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
    (define en  (if lab
                    (edge (ref ht head) (ref ht tail) #:via pos- #:arrow arrow-type #:label lab #:label-gap label-gap) 
                    (edge (ref ht head) (ref ht tail) #:via pos- #:arrow arrow-type)))
    #;(draw-arrow (curve* (add-between pos --)))
    en))


(define (draw-edges l ht)
  (draw* (convert-edges l ht)))

;;;
;;; Example
;;;

; 1. Create graph
(define g (unweighted-graph/directed '((a b) (c d) (a c) (f e) (f a) (a f))))
; (define g (unweighted-graph/undirected '((a b) (c d) (a c) (f e) (f a) (a f))))
; (define g (weighted-graph/undirected '((10 a b) (20 b c))))
;(define g (weighted-graph/directed '((10 a b) (20 b c))))
#;(define g (matrix-graph [[0 3 8 #f -4]
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
         (draw (draw-objects-ht ht)
               (draw-edges layout ht))))
