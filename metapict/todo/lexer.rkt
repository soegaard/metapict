#lang racket
(provide lex)

(require ragg/support parser-tools/lex)

(define-lex-abbrevs
  [letter   (union (char-range #\a #\z) (char-range #\A #\Z))]
  [digit    (char-range #\0 #\9)]
  [name     (concatenation (repetition 1 +inf.0 letter))]
  [number   (repetition 1 +inf.0 digit)]
  [brackets (union "(" ")" "[" "]" "{" "}")]
  [reserved
   (union "," "." "of" "cycle" "--" ".." "..." "tension" "controls" "curl"
          "rotated" "scaled" "shifted" "slanted" "transformed"
          "xcaled" "yscaled" "zscaled" "reflectedabout" 
          "rotatedabout" "false" "true" "normaldeviate" "nullpen" 
          "nullpicture" "pencircle" "whatever"
          ; from <unary-op>
          "abs" "angle" "arclength" "ASCII" "bbox" 
          "blackpart" "bluepart" "bot" "bounded"
          "ceiling" "center" "char" "clipped" 
          "colormodel" "cosd" "cyanpart" "cycle"
          "dashpart" "decimal" "dir" "floor" 
          "filled" "fontpart" "fontsize"
          "greenpart" "greypart" "hex" "inverse" 
          "known"    "length" "lft" "llcorner"
          "lrcorner" "magentapart" "makepath" 
          "makepen" "mexp" "mlog" "not" "oct" "odd"
          "pathpart" "penpart" "readfrom" "redpart" 
          "reverse" "round" "rt" "sind" "sqrt"
          "stroked" "textpart" "textual" "top" "ulcorner"
          "uniformdeviate" "unitvector" "unknown" 
          "urcorner" "xpart" "xxpart"
          "xypart" "yellowpart" "ypart" "yxpart" "yypart"
          ; from <type>
          "boolean" "cmykcolor" "color" "numeric" "pair"
          "path" "pen" "picture" "rgbcolor" "string""transform"
          ; --
          "numeric" "string" 
          ; from <primary-binop"
          "*" "/" "**" "and" "dotprod" "div" "infont" "mod"
          ; from <secondary-binop>
          "+" "−" "++" "+−+" "or" "intersectionpoint" "intersectiontimes"
          ; from <tertiary-binop>
          "&" "<" "<=" "<>" "=" ">" ">=" "cutafter" "cutbefore"
          ; from <of-operator>
          "arctime" "direction" "directiontime" "directionpoint"
          "glyph" "penoffset" "point" "postcontrol" "precontrol"
          "subpath" "substring")])

(define (string-remove-ends str)
  (substring str 1 (sub1 (string-length str))))

(define (peek/ port)
  ; TODO: Handle whitespace?
  (eqv? (peek-char port) #\/))

;; Lexer for MiniPascal
(define (lex ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [reserved   (string-downcase lexeme)]
     [brackets   (string-downcase lexeme)]
     [name       (token 'SYMBOL (string-downcase lexeme))]
     [number     (if (peek/ input-port) 
                     (token 'NUMBER/ (string->number lexeme))
                     (token 'NUMBER  (string->number lexeme)))]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]          
     [(eof)      (void)]))
  (define (next-token)
    (my-lexer ip))
  next-token)
