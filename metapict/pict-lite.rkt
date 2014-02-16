#lang racket/base
;;;
;;; This module exports all identifiers from pict,
;;; that doesn't collide with MetaPict.

(provide (except-out (all-from-out pict)
                     arrow rectangle))

(require pict)




