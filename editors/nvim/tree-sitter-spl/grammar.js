module.exports = grammar({
    name: 'spl',

    // source: https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    extras: $ => [
        /\s|\\\r?\n/,
        $.comment
    ],

    rules: {
        source_file: $ => repeat($.global_dec),

        comment: $ => token(
            seq(
                "//",
                repeat(
                    /[^\n]/,
                ),
                "\n",
            ),
        ),

        global_dec: $ => choice(
            $.type_dec,
            $.proc_dec,
        ),

        type_dec: $ => seq(
            "type",
            $.ident,
            "=",
            $.type_expr,
            ";",
        ),

        proc_dec: $ => seq(
            "proc",
            $.ident,
            "(",
            optional($.param_list),
            ")",
            "{",
            repeat($.var_dec),
            repeat($._stmt),
            "}",
        ),

        type_expr: $ => choice(
            $.ident,
            seq(
                "array",
                "[",
                $.int_lit,
                "]",
                "of",
                $.type_expr,
            ),
        ),

        param_list: $ => seq(
            repeat(
                seq(
                    $.param,
                    ",",
                ),
            ),
            $.param,
        ),

        param: $ => seq(
            optional(
                "ref",
            ),
            $.ident,
            ":",
            $.type_expr,
        ),

        var_dec: $ => seq(
            "var",
            $.ident,
            ":",
            $.type_expr,
            ";",
        ),

        _stmt: $ => choice(
            $.asgn_stmt,
            $.block_stmt,
            $.call_stmt,
            $.empty_stmt,
            $.if_stmt,
            $.while_stmt,
        ),

        asgn_stmt: $ => seq(
            $.ident,
            ":=",
            $._expr,
            ";",
        ),

        block_stmt: $ => seq(
            "{",
            repeat($._stmt),
            "}",
        ),

        call_stmt: $ => seq(
            $.ident,
            "(",
            optional($.arg_list),
            ")",
            ";",
        ),

        arg_list: $ => seq(
            repeat(
                seq(
                    $._expr,
                    ",",
                ),
            ),
            $._expr,
        ),

        empty_stmt: $ => ";",

        if_stmt: $ => prec.left(
            1,
            seq(
                "if",
                $.bracketed_expr,
                $._stmt,
                optional(
                    seq(
                        "else",
                        $._stmt,
                    ),
                ),
            )
        ),

        while_stmt: $ => seq(
            "while",
            $.bracketed_expr,
            $._stmt,
        ),

        _expr: $ => choice(
            $.variable,
            $.int_lit,
            $.unary_expr,
            $.binary_expr,
            $.bracketed_expr,
        ),

        variable: $ => seq(
            $.ident,
            repeat(
                seq(
                    "[",
                    $._expr,
                    "]",
                ),
            ),
        ),

        unary_expr: $ => prec(
            2,
            seq(
                "-",
                $._expr,
            ),
        ),

        // Note: this does not resemble the correct precedence.
        // But in this case it does not matter,
        // because this parser is only used for syntax highlighting.
        binary_expr: $ => prec.left(
            1,
            choice(
                seq(
                    $._expr,
                    choice(
                        "+",
                        "-",
                        "*",
                        "/",
                        "=",
                        "#",
                        "<=",
                        "<",
                        ">=",
                        ">",
                    ),
                    $._expr,
                ),
            )
        ),

        bracketed_expr: $ => seq(
            "(",
            $._expr,
            ")",
        ),

        ident: $ => /[A-Za-z_][A-Za-z0-9_]*/,

        int_lit: $ => choice(
            /0x\d+/,
            /\d+/,
            seq(
                "'",
                choice(
                    /./,
                    /\\n/,
                ),
                "'",
            ),
        ),

    }
});
