;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname colors) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "convert.ss" "teachpack" "htdp") (lib "webpages.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp") (lib "universe.ss" "teachpack" "2htdp") (lib "convert.ss" "teachpack" "htdp") (lib "webpages.ss" "installed-teachpacks")))))
#lang scribble/manual
@(require (for-label (except-in racket angle open path? identity ...) 
                     metapict metapict/metapict/color)
          scribble/extract scribble/eval scribble/base scribble/manual "utils.rkt")
@(define eval (make-metapict-eval))
@interaction-eval[#:eval eval (require metapict)]
@(define math-style tt)

@defmodule[metapict/color]

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "points-and-vectors"]{Points and Vectors}



#;(provide
 color         ; match-expander for color% objects and color names (strings)
 make-color*   ; fault tolerant make-color that also accepts color names
 color->list   ; return components as a list
 color+        ; add colors componentwise
 color*        ; scale componentwise
 color-med     ; mediate (interpolate) between colors
 change-red    ; change red component
 change-green  ; change green component
 change-blue   ; change blue component 
 change-alpha  ; change transparency
 )