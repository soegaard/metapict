#lang racket
;;;
;;; This module implements 
;;;   i) the contracts rad? and deg?
;;;  ii) conversion between radiand and degrees
;;; iii) trigonometric function with input is degrees
(provide (contract-out
          [rad     (-> deg? rad?)]                ; convert from degrees to radians
          [deg     (-> rad? deg?)]                ; convert from radians to degrees
          [cosd    (-> deg? real?)]                ; (compose cos deg)
          [sind    (-> deg? real?)]                ; (compose sin deg)
          [tand    (-> deg? real?)]                ; (compose tan deg)
          [cos&sin (-> rad? (values real? real?))] ; return cos and sin of the given radians
          [rad?    (-> any/c boolean?)]             ; radian predicate (for contracts)
          [deg?    (-> any/c boolean?)]             ; degree predicate (for contracts)
          ))

(define rad? real?) 
(define deg? real?) 

(define (rad deg) (* (/ deg 180.0) pi))
(define (deg rad) (* (/ rad pi) 180.0))
(define (cosd d) (cos (rad d)))
(define (sind d) (sin (rad d)))
(define (tand d) (tan (rad d)))
(define (cos&sin θ) (values (cos θ) (sin θ)))

(module+ test (require rackunit)
  (check-equal? (map rad '(0 180 360))  (list 0 pi (* 2 pi)))
  (check-equal? (map deg (list 0 pi (* 2 pi))) '(0 180. 360.))
  (check-equal? (cosd 180) -1.)
  (check-equal? (sind 90) 1.)
  (check-= (tand 45) 1. 1e-15))
