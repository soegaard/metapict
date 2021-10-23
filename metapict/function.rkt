#lang racket/base
;;;
;;; Functions 
;;;

(provide (all-defined-out))


;;; Requirements

(require racket/format racket/match racket/list
         "curve.rkt" "def.rkt" "domain.rkt" "draw.rkt" "path.rkt"
         (except-in "structs.rkt" open))



;;; Implementation

(define (fun-print fun port mode)
  (defm (fun: n d m) fun)
  (def str (~a "(function " (or n "anon") " " (format-domain d) " " m ")"))
  (def put (match mode [#t write] [#f display] [_ print]))
  (put str port))

(struct fun: (name domain map)
  #:property prop:procedure (struct-field-index map)
  #:methods  gen:custom-write [(define write-proc fun-print)]
  #:transparent)

(define fun-name   fun:-name)
(define fun-domain fun:-domain)
(define fun-map    fun:-map)

  
(define fun
  (let ()
    (define (err msg)           (error 'function msg))
    (define (domain-msg    val) (~a "expected domain, got: " val))
    (define (procedure-msg val) (~a "expected procedure, got: " val))
    (define (make-map proc dom)
      (λ (x)        
        (if (domain-member? x dom)
            (proc x)
            #f)))    
  (case-lambda
    [(domain map)
     (unless (domain? domain) (err (domain-msg domain)))       
     (unless (procedure? map) (err (procedure-msg map)))
     (fun: #f domain (make-map map domain))]
    [(name domain map)
     (unless (domain? domain) (err (domain-msg domain)))       
     (unless (procedure? map) (err (procedure-msg map)))
     (fun: (~a name) domain (make-map map domain))]
    [else (error 'function "expected (optional name), domain and procedure")])))


(define (cond-fun fun-dom-list #:name [name #f])
  (match fun-dom-list
    [(list (list (? procedure? procs) (? domain? doms)) ...)
     (def dom (domain-union* doms))
     (def f   (λ (x)
                (for/or ([d doms] [f procs])
                  (if (domain-member? x d)
                      (f x)
                      #f))))
     (fun name dom f)]))

