#lang racket/base
;;;
;;; This module exports all identifiers from pict,
;;; that doesn't collide with MetaPict.
;;;
(require pict)

(provide (except-out (all-from-out pict)
                     arrow circle rectangle rounded-rectangle filled-rounded-rectangle scale text))

(provide (prefix-out pict: scale))






