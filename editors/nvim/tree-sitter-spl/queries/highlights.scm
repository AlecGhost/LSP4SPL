; highlights.scm

"proc" @keyword
"type" @keyword
"var" @keyword
"ref" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"array" @keyword
"of" @keyword

(comment) @comment
(int_lit) @number
(proc_dec (ident) @function)
(call_stmt (ident) @function)
(param (ident) @parameter)
(type_expr (ident) @type)
(type_dec (ident) @type)
(var_dec (ident) @variable)
(variable (ident) @variable)
