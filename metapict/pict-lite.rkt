#lang racket/base
;;;
;;; This module exports all identifiers from pict,
;;; that doesn't collide with MetaPict.
;;;
(require pict)

(provide (except-out (all-from-out pict)
                     arrow circle rectangle scale))

(provide (prefix-out pict: scale))






