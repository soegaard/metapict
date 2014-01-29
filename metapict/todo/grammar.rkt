#lang ragg
;;; Lexemes:

; SYMBOLIC-TOKEN
; NUMBER-NOT-FOLLOWED-BY-/NUMBER
; NUMBER
; NUMBER-OR-FRACTION-NOT-FOLLOWED-BY-ADD-OP-NUMBER
; TAG   (standard Racket symbol?)

; http://www.tug.org/tutorials/mp/mpman.pdf  [page 88]

;;; Figure 55 and 56: The syntax for expressions

empty : "" 

atom : variable 
     | argument 
     | number-or-fraction
     | internal-variable
     | "(" expression ")"
     | "begingroup" statement-list expression "endgroup"
     | nullary-op
     ; | "btex" typesetting-commands "etex"
     ; (typesetting commands are in MP passed directly to TeX)
     | pseudo-function
     
primary : atom
        | "(" numeric-expression "," numeric-expression ")"
        | "(" numeric-expression "," numeric-expression "," numeric-expression ")"
        | of-operator expression "of" primary
        | unary-op primary
        | "str" suffix
        | "z" suffix
        | numeric-atom "[" expression "," expression "]"
        | scalar-multiplication-op primary

secondary : primary
          | secondary primary-binop primary
          | secondary transformer

tertiary : secondary
         | tertiary secondary-binop secondary

subexpression : tertiary
              | path-expression path-join path-knot

expression : subexpression
           | expression tertiary-binop tertiary
           | path-subexpression direction-specifier
           | path-subexpression path-join "cycle"

path-knot : tertiary

path-join : "--" 
          | direction-specifier basic-path-join direction-specifier

direction-specifier : empty
                    | "{" "curl" numeric-expression "}"
                    | "{" pair-expression "}"
                    | "{" numeric-expression "," numeric-expression "}"

basic-path-join : ".." | "..." | ".." tension ".." | ".."  controls ".."

tension : "tension" numeric-primary
        | "tension" numeric-primary "and" numeric-primary

controls : "controls" pair-primary
         | "controls" pair-primary "and" pair-primary

argument : SYMBOLIC-TOKEN

number-or-fraction : NUMBER "/" NUMBER
                   | NUMBER-NOT-FOLLOWED-BY-/NUMBER

scalar-multiplication-op : "+" | "−"
                         | NUMBER-OR-FRACTION-NOT-FOLLOWED-BY-ADD-OP-NUMBER

transformer : "rotated" numeric-primary
            | "scaled"  numeric-primary
            | "shifted" pair-primary
            | "slanted" numeric-primary
            | "transformed" transform-primary
            | "xscaled" numeric-primary
            | "yscaled" numeric-primary
            | "zscaled" pair-primary
            | "reflectedabout" "(" pair-expression "," pair-expression ")"
            | "rotatedaround"  "(" pair-expression "," numeric-expression ")"

nullary-op : "false" | "normaldeviate" | "nullpen" | "nullpicture" | "pencircle"
| "true" | "whatever"

unary-op : type
         | "abs" | "angle" | "arclength" | "ASCII" | "bbox" 
         | "blackpart" | "bluepart" | "bot" | "bounded"
         | "ceiling"  | "center" | "char" | "clipped" 
         | "colormodel" | "cosd" | "cyanpart" | "cycle"
         | "dashpart" | "decimal" | "dir" | "floor" 
         | "filled"   | "fontpart" | "fontsize"
         | "greenpart" | "greypart" | "hex" | "inverse" 
         | "known"    | "length" | "lft" | "llcorner"
         | "lrcorner" | "magentapart" | "makepath" 
         | "makepen"  | "mexp" | "mlog" | "not" | "oct" | "odd"
         | "pathpart" | "penpart" | "readfrom" | "redpart" 
         | "reverse"  | "round" | "rt" | "sind" | "sqrt"
         | "stroked"  | "textpart" | "textual" | "top" | "ulcorner"
         | "uniformdeviate" | "unitvector" | "unknown" 
         | "urcorner" | "xpart" | "xxpart"
         | "xypart"   | "yellowpart" | "ypart" | "yxpart" | "yypart"

type : "boolean" | "cmykcolor" | "color" | "numeric" | "pair"
     | "path" | "pen" | "picture" | "rgbcolor" | "string" | "transform"

internal-type : "numeric" | "string"

primary-binop : "*" | "/" | "**" | "and"
              | "dotprod" | "div" | "infont" | "mod"

secondary-binop : "+" | "−" | "++" | "+−+" | "or"
                | "intersectionpoint" | "intersectiontimes"

tertiary-binop : "&" | "<" | "<=" | "<>" | "=" | ">" | ">="
               | "cutafter" | "cutbefore"

of-operator : "arctime" | "direction" | "directiontime" | "directionpoint"
            | "glyph" | "penoffset" | "point" | "postcontrol" | "precontrol"
            | "subpath" | "substring"                         

variable : TAG suffix

suffix : empty | suffix subscript | suffix TAG
       | suffix-parameter

subscript : NUMBER | "[" numeric-expression "]"

internal-variable : "ahangle" | "ahlength" | "bboxmargin"
                  | "charcode" | "day" | "defaultcolormodel" 
                  | "defaultpen" | "defaultscale"
                  | "hour" | "jobname" | "labeloffset" | "linecap" 
                  | "linejoin" | "minute" | "miterlimit" | "month"
                  | "outputformat" | "outputtemplate" | "pausing" 
                  | "prologues" | "showstopping"
                  | "time" | "tracingoutput" | "tracingcapsules" 
                  | "tracingchoices" | "tracingcommands"
                  | "tracingequations" | "tracinglostchars" 
                  | "tracingmacros" | "tracingonline" 
                  | "tracingrestores" | "tracingspecs"
                  | "tracingstats" | "tracingtitles" | "truecorners"
                  | "warningcheck" | "year"
                  ; | symbolic-token-defined-by-newinternal

;;; Figure 57: The syntax for function-like macros
                    
pseudo-function : "min"  "(" expression-list ")"
                | "max"  "(" expression-list ")"
                | "incr" "(" numeric-variable ")"
                | "decr" "(" numeric-variable ")"
                | "dashpattern" "(" on/off-list ")"
                | "interpath" "(" numeric-expression "," 
                              path-expression "," path-expression ")"
                | "buildcycle" "(" path-expression-list ")"
                | "thelabel" label-suffix "(" expression "," pair-expression ")"

path-expression-list : path-expression
                     | path-expression-list "," path-expression

on/off-list : on/off-list on/off-clause | on/off-clause

on/off-clause : "on" numeric-tertiary 
              | "off" numeric-tertiary

; Miscellaneous productions needed to complete the BNF
boolean-expression : expression
cmykcolor-expression : expression
color-expression : expression
numeric-atom : atom
numeric-expression : expression
numeric-primary : primary
numeric-tertiary : tertiary
numeric-variable : variable | internal-variable
pair-expression : expression
pair-primary : primary
path-expression : expression
path-subexpression : subexpression
pen-expression : expression
picture-expression : expression
picture-variable : variable
rgbcolor-expression : expression
string-expression : expression
suffix-parameter : parameter
transform-primary : primary

parameter : SYMBOLIC-TOKEN

;;; Figure 59: Overall syntax for MetaPost programs

program : statement-list "end"
statement-list : empty | statement-list ";" statement
statement : empty
          | equation | assignment
          | declaration 
          ; | macro-definition
          | compound | pseudo-procedure
          | command

compound : "begingroup" statement-list "endgroup"
         | "beginfig" "(" numeric-expression ")" ";" statement-list ";" "endfig"

equation : expression "=" right-hand-side

assignment : variable ":=" right-hand-side
           | internal-variable ":=" right-hand-side

right-hand-side : expression | equation | assignment

declaration : type declaration-list

declaration-list : generic-variable
                 | declaration-list "," generic-variable

generic-variable : SYMBOLIC-TOKEN generic-suffix

generic-suffix : empty | generic-suffix TAG
               | generic-suffix "[]"

; No need fro macro definitions... 
;macro-definition : macro-heading "=" replacement-text "enddef"
;
;macro-heading : "def" symbolic-token delimited-part undelimited-part
;              | "vardef" generic-variable delimited-part undelimited-part
;              | "vardef" generic-variable "@#" delimited-part undelimited-part
;              | binary-def parameter symbolic-token parameter
;
;delimited-part : empty
;               | delimited-part "(" parameter-type parameter-tokens ")"
;
;parameter-type : expr | suffix | text
;
;parameter-tokens : parameter | parameter-tokens "," parameter
;
;parameter : symbolic-token
;
;undelimited-part : empty
;                 | parameter-type parameter
;                 | precedence-level parameter
;                 | "expr" parameter "of" parameter
;
;precedence-level : primary | secondary | tertiary
;
;binary-def : primarydef | secondarydef | tertiarydef

pseudo-procedure : "drawoptions" "(" option-list ")"
                 | "label"     label-suffix "(" expression "," pair-expression ")"
                 | "dotlabel"  label-suffix "(" expression "," pair-expression ")"
                 | "labels"    label-suffix "(" point-number-list ")"
                 | "dotlabels" label-suffix "(" point-number-list ")"

point-number-list : suffix | point-number-list "," suffix

label-suffix : empty | "lft" | "rt" | "top" | "bot" | "ulft" | "urt" | "llft" | "lrt"

;;; Figure 60-61: The syntax for commands

command : "clip" picture-variable "to" path-expression
| "interim" internal-variable ":=" right-hand-side
| "let" SYMBOLIC-TOKEN "=" SYMBOLIC-TOKEN
| "pickup" expression
| "randomseed" ":=" numeric-expression
| "save" symbolic-token-list
| "setbounds" picture-variable "to" path-expression
| "shipout" picture-expression
| "write" string-expression "to" string-expression
| addto-command
| drawing-command
; | font-metric-command
| newinternal-command
| message-command
| mode-command
| show-command
| special-command
| tracing-command

show-command : "show" expression-list
| "showvariable" symbolic-token-list
| "showtoken" symbolic-token-list
| "showdependencies"

symbolic-token-list : SYMBOLIC-TOKEN
                    | SYMBOLIC-TOKEN "," symbolic-token-list

expression-list : expression | expression-list "," expression

addto-command : "addto" picture-variable "also" picture-expression option-list
              | "addto" picture-variable "contour" path-expression option-list
              | "addto" picture-variable "doublepath" path-expression option-list

option-list : empty | drawing-option  option-list

drawing-option : "withcolor" color-expression
               | "withrgbcolor" rgbcolor-expression
               | "withcmykcolor" cmykcolor-expression
               | "withgreyscale" numeric-expression
               | "withoutcolor"
               | "withprescript" string-expression
               | "withpostscript" string-expression
               | "withpen" pen-expression
               | "dashed" picture-expression

drawing-command : "draw" picture-expression option-list
                | fill-type path-expression option-list

fill-type : "fill" | "draw" | "filldraw" | "unfill" | "undraw" | "unfilldraw"
          | "drawarrow" | "drawdblarrow" | "cutdraw"

newinternal-command : "newinternal" internal-type symbolic-token-list
                    | "newinternal" symbolic-token-list

message-command : "errhelp" string-expression
                | "errmessage" string-expression
                | "filenametemplate" string-expression
                | "message" string-expression

mode-command : "batchmode" | "nonstopmode"
             | "scrollmode" | "errorstopmode"

special-command : "fontmapfile" string-expression
                | "fontmapline" string-expression
                | "special" string-expression

tracing-command : "tracingall" | "loggingall" | "tracingnone"

;;; Figure 62: The syntax for conditionals and loops

; if-test : "if" boolean-expression ":" balanced-tokens alternatives "fi"

; alternatives : empty
;              | "else:" balanced-tokens
;              | "elseif" boolean-expression ":" balanced-tokens alternatives

; loop : loop-header ":" loop-text "endfor"

loop-header : "for" SYMBOLIC-TOKEN "=" progression
            | "for" SYMBOLIC-TOKEN "=" for-list
            | "for" SYMBOLIC-TOKEN "within" picture-expression
            | "forsuffixes" SYMBOLIC-TOKEN "=" suffix-list
            | "forever"

progression : numeric-expression "upto" numeric-expression
            | numeric-expression "downto" numeric-expression
            | numeric-expression "step" numeric-expression "until" numeric-expression

for-list : expression | for-list "," expression

suffix-list : suffix | suffix-list "," suffix
