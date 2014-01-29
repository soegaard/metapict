#lang racket

(require racket/draw
         "bez.rkt" "curve.rkt" "dc.rkt" "def.rkt" "device.rkt" 
         "path.rkt"  "trans.rkt" "structs.rkt" "window.rkt")

(define (draw-path dc p #:transformation [t #f] #:pen-transformation [pent #f])
  (match p
    [(? path?) (draw-bezs dc (resolve-path-to-bezs p)
                          #:transformation t
                          #:pen-transformation pent)]))

(define (paths->bitmap paths  ; todo: add transformations ; todo fix!
                       #:window [win (window -1 1 -1 1)] ; logical
                       #:device-width [w 200] 
                       #:device-height [h 200]
                       #:pen [pen (send the-pen-list find-or-create-pen "black" 0 'solid)]
                       #:color [color #f]  ; line color
                       #:width [width #f]  ; logical linewidth
                       #:style [style #f]) ; line style
  (def dc (make-bitmap-dc w h))
  (set-transformation dc (stdtrans win w h))
  ; set the pen - if #:color, #:width or #:style is given, they
  ; override the pen
  (send dc set-pen pen)
  (def pc (send pen get-color))
  (def pw (send pen get-width))
  (def ps (send pen get-style))
  (send dc set-pen (or color pc) (or width pw) (or style ps))
  (for ([p (in-list paths)])
    (draw-path dc p))
  (send dc get-bitmap))

(define (path->bitmap p 
                      #:window [win (window -1 1 -1 1)] ; logical
                      #:device-width [w 200] #:device-height [h 200]  ; device width and height
                      #:pen [pen (send the-pen-list find-or-create-pen "black" 0 'solid)]
                      #:color [color #f]  ; line color
                      #:width [width #f]  ; logical linewidth
                      #:style [style #f])
  (paths->bitmap (list p)
                 #:window win #:device-width w #:device-height h
                 #:pen pen #:color color #:width width #:style style))

;;;
;;; Debug
;;; 

;; The following functions allow us to write a curve in MetaPost Syntax to a file,
;; let MetaPost compute the control points, and then read back the control points.

(define (path->mpost-file p file-path)
  ; write path to mpost file, which when run prints 
  ; an explicit path with control points in the log
  (with-output-to-file file-path
    (λ()
      (displayln "beginfig(1)")
      (displayln "  path p;")
      (display   "  p := ") (display-path p) (displayln ";")
      (displayln "  show p;")
      (displayln "  draw p;")
      (displayln "endfig;"))
    #:exists 'replace))

(define (run-mpost file-path)
  ; run the mpost file, the path 
  (with-output-to-string
      (λ() (system* "/opt/local/bin/mpost" file-path "end"))))

(define (read-mpost-log log-file-path)
  ; read the mpost path with explicit control points from the log file
  (define (fix-knot k)
    (define (fix-pt p) (and p (let () (defm (pt x y) p) (pt (string->number x) (string->number y)))))
    (defm (knot p p- p+ lt rt) k)
    (knot (fix-pt p) (fix-pt p-) (fix-pt p+) lt rt))  
  (def rx1 #rx"\\(([0-9.\\-]+),([0-9.\\-]+)\\)..controls \\(([0-9.\\-]+),([0-9.\\-]+)\\) and \\(([0-9.\\-]+),([0-9.\\-]+)\\)")
  (def rx  #rx" ..\\(([0-9.\\-]+),([0-9.\\-]+)\\)..controls \\(([0-9.\\-]+),([0-9.\\-]+)\\) and \\(([0-9.\\-]+),([0-9.\\-]+)\\)")
  (def rxn #rx" ..\\(([0-9.\\-]+),([0-9.\\-]+)\\)")
  (def rxcycle #rx"..cycle")
  (with-input-from-file log-file-path
    (λ() 
      (for ([_ 5]) (read-line))
      (def line1 (read-line))
      (displayln line1)
      (defm (list _ p0x p0y p0+x p0+y p1-x p1-y) (regexp-match rx1 line1))
      (def k0 (knot (pt p0x p0y) #f (pt p0+x p0+y) (endpoint) (explicit)))
      (def closed? #f)
      (def knots
        (let loop ([ks (list k0)] [p-x p1-x] [p-y p1-y])
          (def line (read-line))
          (displayln line)
          (unless (eof-object? line)
            (match line
              [(regexp rx (list x px py p+x p+y q-x q-y))
               (loop (cons (knot (pt px py) (pt p-x p-y) (pt p+x p+y) (explicit) (explicit)) ks) 
                     q-x q-y)]
              [(regexp rxn (list _ px py))
               (map fix-knot
                    (reverse 
                     (cons (knot (pt px py) (pt p-x p-y) #f (explicit) (endpoint)) ks)))]
              [(regexp rxcycle) (set! closed? #t) ks]
              [_ (error 'read-mpost-log (~a "can't parse the line: \n" line))]))))
      (when closed?
        (displayln "warning: closed paths aren't parsed correctly (todo)"))
      ((if closed? closed-path open-path) knots))))

(define (draw-path-via-mpost dc p #:transformation [t #f])
  (path->mpost-file p "foo.mp")
  (run-mpost "foo.mp")
  (def bs (segment->bezs (segment (path-knots (read-mpost-log "foo.log")))))
  (draw-bezs dc bs #:transformation t))

(define (path->curve/mpost p #:transformation [t #f])
  (path->mpost-file p "foo.mp")
  (run-mpost "foo.mp")
  (def bs (segment->bezs (segment (path-knots (read-mpost-log "foo.log")))))
  (curve: #f ; todo: closed or not?
          bs))

(define (path->mpost-bitmap p)
  (def dc (make-bitmap-dc 400 400))
  (def T (stdtrans (window -4 4 -4 4) 400 400))
  (send dc set-pen "blue" 1/100 'solid)
  (draw-path-via-mpost dc p #:transformation T)
  (send dc get-bitmap))

(define (draw-test p)
  ; the non-resolved path p (in logical coordinates) is drawn in two ways:
  ; - in red with control points calculated by MetaPost
  ; - in blue with control points calculated by resolve-path
  ; use this to debug the results of resolve-path
  (def dc (make-bitmap-dc 400 400))
  (def T (stdtrans (window -4 4 -4 4) 400 400))
  (set-transformation dc T)
  (send dc set-pen "red" 1/5 'solid)
  (draw-path-via-mpost dc p #:transformation T)
  (send dc set-pen "blue" 1/25 'solid)
  (draw-path dc p #:transformation T)
  (send dc get-bitmap))


