#lang ragg

expression : subexpression
           | expression tertiary-binop tertiary
           | path-subexpression direction-specifier
           | path-subexpression path-join "cycle"

atom : variable 
     | argument 
     | number-or-fraction
     | "(" expression ")"
     | nullary-op

     ; | internal-variable
     ; | "begingroup" statement-list expression "endgroup"
     ; | "btex" typesetting-commands "etex"
     ; | pseudo-function
     
primary : atom
        | "(" numeric-expression "," numeric-expression ")"
        | "(" numeric-expression "," numeric-expression "," numeric-expression ")"
        | scalar-multiplication-op primary
        | unary-op primary
        | numeric-atom "[" expression "," expression "]"

        ; | of-operator expression "of" primary
        ; | "str" suffix
        ; | "z" suffix

secondary : primary
          | secondary primary-binop primary
          | secondary transformer

tertiary : secondary
         | tertiary secondary-binop secondary

subexpression : tertiary
              | path-expression path-join path-knot


path-knot : tertiary

path-join : "--" 
          | direction-specifier basic-path-join direction-specifier

direction-specifier : 
        [    "{" "curl" numeric-expression "}"
           | "{" pair-expression "}"
           | "{" numeric-expression "," numeric-expression "}" ]

basic-path-join : ".." | "..." | ".." tension ".." | ".."  controls ".."

tension : "tension" numeric-primary
        | "tension" numeric-primary "and" numeric-primary

controls : "controls" pair-primary
         | "controls" pair-primary "and" pair-primary

argument : SYMBOLIC-TOKEN

; NUMBER/ is a number where with / as the next token.
; NUMBER  is a number where / is not the next token. 
number-or-fraction : NUMBER/ "/" number
                   | NUMBER 

scalar-multiplication-op : "+" | "−"
    | NUMBER ; should have been
    ; | number-or-fraction-not-followed-by-add-op-number

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

nullary-op : "false" 
           | "true" 
           ; | "normaldeviate" 
           ; | "nullpen" 
           ; | "nullpicture" 
           ; | "pencircle"
           ; | "whatever"

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

variable : tag suffix

suffix : [suffix subscript | suffix TAG | suffix-parameter ]

subscript : number | "[" numeric-expression "]"

number : NUMBER | NUMBER/

boolean-expression : expression
cmykcolor-expression : expression
color-expression : expression
numeric-atom : atom
numeric-expression : expression
numeric-primary : primary
numeric-tertiary : tertiary
numeric-variable :     variable 
                   ; | internal-variable
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

parameter : SYMBOL
tag : SYMBOL