#lang racket/base
;;;
;;; SVG Lexer and Parse
;;;

;; This module contains a lexer and parser for svg paths
;; as defined in the SVG2 standard.

(provide parse-svg-path)  ; Parses from input-port or string and returns an s-exp.

;; Example:

;; > (parse-svg-path "M100,200 C100,100 250,100 250,200 S400,300 400,200")
;; '((M (100 200)) (C ((100 100) (250 100) (250 200))) (S ((400 300) (400 200))))


;;;
;;; LEXER
;;;

;; The grammar is as follows:

;;   coordinate          ::= sign? number
;;   sign                ::= "+" | "-"
;;   exponent            ::= ("e" | "E") sign? digit+
;;   fractional-constant ::= (digit* "." digit+) | digit+
;;   number              ::= fractional-constant exponent?
;;   flag                ::= ("0" | "1")
;;   comma_wsp           ::= (wsp+ ","? wsp*) | ("," wsp*)
;;   wsp                 ::= (#x9 | #x20 | #xA | #xC | #xD)
;;   digit               ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

;; Note: Since 0 is both a coordinate and a flag,
;;       this lexer returns a coordinate token when it sees a flag.

;; Note: The grammar is from
;;                             https://svgwg.org/svg2-draft/paths.html#PathDataBNF
;;       The grammar at 
;                              https://www.w3.org/TR/SVG/paths.html#PathDataBNF
;;       has an incorrect rule for number.


(require parser-tools/lex)

;;;
;;; Lexer Abbreviations
;;;

;; Let's name the regular expressions we will need to specify the lexer.

(define-lex-abbrevs
  [sign                (union #\+ #\-)]
  [sign?               (repetition 0 1 sign)]
  [digit               (char-range #\0 #\9)]
  [digit+              (repetition 1 +inf.0 digit)]
  [digit*              (repetition 0 +inf.0 digit)]
  [exponent            (concatenation (union #\e #\E) sign? digit+)]
  [exponent?           (repetition 0 1 exponent)]
  [fractional-constant (union (concatenation digit* #\. digit+) digit+)]
  [number              (concatenation fractional-constant exponent?)]
  [wsp                 (union   #\tab #\space #\newline #\page #\return)]
  [wsp+                (repetition 1 +inf.0 wsp)]
  [wsp*                (repetition 0 +inf.0 wsp)]
  [comma               #\,]
  [comma?              (repetition 0 1 comma)]
  [comma-wsp           (union (concatenation wsp+ comma? wsp*)
                              (concatenation      comma  wsp*))]
  [coordinate          (concatenation sign? number)]
  [command             (union (char-range #\a #\z) (char-range #\A #\Z))])

;; The tokens fall in three classes.
(define-tokens        value-tokens (COORD))
(define-empty-tokens  svg-commands (M m Z z L l H h V v C c A a S s))
(define-empty-tokens  empty-tokens (EOF COMMA))
; The only value token, COORD, contains the coordinate (number) as the token value.


;; The user interacts with the lexer like this.
;;   (lex input-port)
;; returns a function, next-token, that when called
;; will return the next token from the input port.
;; When end of file is reached, the token EOF is returned.

; lex : input-port -> token
(define (lex ip)
  (port-count-lines! ip)
  (define svg-path-lexer
    (lexer-src-pos
     ; coordinate
     [coordinate (token-COORD (string->number lexeme))]
     ; commands
     [#\M (token-M)] [#\m (token-m)]
     [#\Z (token-Z)] [#\z (token-z)]
     [#\L (token-L)] [#\l (token-l)]
     [#\H (token-H)] [#\h (token-h)]
     [#\V (token-V)] [#\v (token-v)]
     [#\C (token-C)] [#\c (token-c)]
     [#\A (token-A)] [#\a (token-a)]
     [#\S (token-S)] [#\s (token-s)]
     ; delimiters and eof
     [wsp+                    (return-without-pos (next-token))] ; eat white space 
     [(union comma comma-wsp) (token-COMMA)]     
     [(eof)                   (token-EOF)]))
  (define (next-token)
    (svg-path-lexer ip))
  next-token)


;;;
;;; Support functions for the lexer
;;;

; eof-token? : token -> boolean
(define (eof-token? t)
  (eq? (token-name (position-token-token t)) 'EOF))

; svg-tokens : string -> listof token
(define (svg-tokens x)
  (cond
    [(string? x)     (svg-tokens (open-input-string x))]
    [(input-port? x) (define next (lex x))
                     (let loop ([t (next)])
                       (if (eof-token? t)
                           '()
                           (cons t (loop (next)))))]
    [else (error 'svg-tokens "expected a string or an input port, got: ~a" x)]))

;;;
;;; SVG Path Parser
;;;

;; The SVG path parser is an LR-parser built wuing parser-tools/yacc.

(require parser-tools/yacc)

;; The start production is `svg-path`.

;; The intput to the generated parser `parse` below is
;; a tokenizer returned by lex above

;; Example:

;; > (parse-svg-path "M100,200 C100,100 250,100 250,200 S400,300 400,200")
;; '((M (100 200))
;;   (C ((100 100) (250 100) (250 200)))
;;   (S ((400 300) (400 200))))

(define (parse-svg-path x)
  (cond
    [(string? x)     (parse (lex (open-input-string x)))]
    [(input-port? x) (parse (lex x))]
    [else            (error 'parse-svg-path
                            "expected a string or an input port, got: ~a" x)]))

(define parse
  (parser [tokens value-tokens svg-commands empty-tokens]
          [src-pos]
          [start svg-path]
          [end   EOF]
          [error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (displayln (list 'error
                                    'tok-ok?   tok-ok?
                                    'tok-name  tok-name
                                    'tok-value tok-value
                                    'start-pos start-pos
                                    'end-pos   end-pos))
                   (error 'parse-svg-path "an error occurred"))]
          (suppress) ; we have 1 reduce/reduce conflict
          
          [grammar
           ;; svg_path::= wsp* moveto? (moveto drawto_command*)?
           ;; Note: Our lexer eats white space, so the wsp* is not needed.
           [svg-path        [(moveto)                         $1]
                            [(svg-path-more)                  $1]]           
           [svg-path-more   [(moveto drawto-commands)         (cons $1 $2)]
                            [()                               '()]]
           [drawto-commands [(drawto-command drawto-commands) (cons $1 $2)]
                            [()                               '()]]
           
           ;; drawto_command::=
           ;;       moveto
           ;;     | closepath
           ;;     | lineto
           ;;     | horizontal_lineto
           ;;     | vertical_lineto
           ;;     | curveto
           ;;     | smooth_curveto
           ;;     | quadratic_bezier_curveto
           ;;     | smooth_quadratic_bezier_curveto
           ;;     | elliptical_arc
           [drawto-command [(moveto)         $1]
                           [(closepath)      $1]
                           [(lineto)         $1]
                           [(hor-lineto)     $1]
                           [(ver-lineto)     $1]
                           [(curveto)        $1]
                           [(smooth-curveto) $1]
                           [(elliptical-arc) $1]]

           ;; moveto ::= ( "M" | "m" ) wsp* coordinate_pair_sequence
           [moveto        [(M coord-pair-seq) (cons 'M $2)]
                          [(m coord-pair-seq) (cons 'm $2)]]
           
           ;; lineto ::= ("L"|"l") wsp* coordinate_pair_sequence
           [lineto        [(L coord-pair-seq) (cons 'L $2)]
                          [(l coord-pair-seq) (cons 'l $2)]]

           ;; closepath ::= ("Z" | "z")
           [closepath     [(Z)             (list 'Z)]
                          [(z)             (list 'z)]]
           
           ;; horizontal_lineto ::= ("H"|"h") wsp* coordinate_sequence
           [hor-lineto    [(H coord-seq) (cons 'H $2)]
                          [(h coord-seq) (cons 'h $2)]]
           
           ;; vertical_lineto ::= ("V"|"v") wsp* coordinate_sequence
           [ver-lineto    [(V coord-seq) (cons 'V $2)]
                          [(v coord-seq) (cons 'v $2)]]

           ;; curveto ::= ("C"|"c") wsp* curveto_coordinate_sequence
           [curveto       [(C curveto-coord-seq) (cons 'C $2)]
                          [(c curveto-coord-seq) (cons 'c $2)]]

           ;; smooth-curve-to ::= ("S"|"s") wsp* curveto_coordinate_sequence
           [smooth-curveto [(S smooth-curveto-coord-seq) (cons 'S $2)]
                           [(s smooth-curveto-coord-seq) (cons 's $2)]]

           ;; elliptical_arc ::= ( "A" | "a" ) wsp* elliptical_arc_argument_sequence
           [elliptical-arc [(A elliptical-arc-argument-seq) (cons 'A $2)]
                           [(a elliptical-arc-argument-seq) (cons 'a $2)]]


           [comma?         [(COMMA) 'ignore]
                           [()      'ignore]]

           ;; coordinate_pair ::= coordinate comma_wsp? coordinate
           [coord-pair    [(COORD comma? COORD)     (list $1 $3)]]

           ;; coordinate_sequence ::= coordinate | (coordinate comma_wsp? coordinate_sequence)
           [coord-seq     [(COORD)                  (list $1)]
                          [(COORD comma? coord-seq) (cons $1 $3)]]

           ;; coordinate_pair_sequence ::=
           ;;     coordinate_pair | (coordinate_pair comma_wsp? coordinate_pair_sequence)
           [coord-pair-seq [(coord-pair comma? coord-pair-seq) (cons $1 $3)]
                           [(coord-pair)                       (list $1)]]
           
           ;; coordinate_pair_double ::= coordinate_pair comma_wsp? coordinate_pair
           [coord-pair-double  [(coord-pair comma? coord-pair)  (list $1 $3)]]
           
           ;; coordinate_pair_triplet::=
           ;;     coordinate_pair comma_wsp? coordinate_pair comma_wsp? coordinate_pair
           [coord-pair-triplet [(coord-pair comma? coord-pair comma? coord-pair)  (list $1 $3 $5)]]

           ;; curveto_coordinate_sequence::=
           ;;     coordinate_pair_triplet
           ;;     | (coordinate_pair_triplet comma_wsp? curveto_coordinate_sequence)
           [curveto-coord-seq [(coord-pair-triplet)                           (list $1)]
                              [(coord-pair-triplet comma? curveto-coord-seq)  (cons $1 $3)]]

           ;; smooth_curveto_coordinate_sequence::=
           ;;     coordinate_pair_triplet
           ;;     | (coordinate_pair_triplet comma_wsp? smooth_curveto_coordinate_sequence)
           [smooth-curveto-coord-seq
            [(coord-pair-double)                                  (list $1)]
            [(coord-pair-double comma? smooth-curveto-coord-seq)  (cons $1 $3)]]

           ;; elliptical_arc_argument_sequence::=
           ;;        elliptical_arc_argument
           ;;      | (elliptical_arc_argument comma_wsp? elliptical_arc_argument_sequence)
           [elliptical-arc-argument-seq
            [(elliptical-arc-argument comma? elliptical-arc-argument-seq) (cons $1 $3)]
            [(elliptical-arc-argument)                                    (list $1)]]
           
           ;; elliptical_arc_argument::= number comma_wsp? number comma_wsp? number comma_wsp
           ;;                            flag comma_wsp? flag comma_wsp? coordinate_pair
           ;; Note: We are using COORD instead of FLAG here.
           ;;       Since the lexer sees 0 and 1 as coordinates.
           ;;       (rx ry x-axis-rotation large-arc-flag sweep-flag x y)+
           [elliptical-arc-argument [(COORD comma?  ; rx
                                      COORD comma?  ; ry
                                      COORD comma?  ; x-axis-rotation
                                      COORD comma?  ; large-arc-flag
                                      COORD comma?  ; sweep-flag
                                      coord-pair)   ; x y
                                     (list* $1 $3 $5 $7 $9 $11)]]]))



