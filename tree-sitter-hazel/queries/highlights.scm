;keywords

"fun" @keyword.function
"let" @definition
"in" @keyword

;literals
(int_lit) @number
(float_lit) @number
(string) @string
(bool_lit) @booleans
(infix_exp) @operator

;expression
(var) @variable

;types
(type) @type
