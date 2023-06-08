//helpers:
function commaSep1(rule) {
    return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
    return optional(commaSep1(rule))
}


module.exports = grammar({
    name: 'Hazel',
    conflicts: $ => [[$._expression, $._pat], [$.tuple_pat, $.tuple_exp], [$.list, $.list_pat]],

    rules: {
        program: $ => ($._expression),

        ident: $ => /[A-Za-z][A-Za-z0-9_']*/,

        //basic structures:

        _expression: $ => choice(
            $.var,
            $.let,
            $.int_lit,
            $.float_lit,
            $.bool_lit,
            $.string,
            $.tuple_exp,
            $.infix_exp,
            $.fun,
            $.if,
            $.ap,
            $.case,
            $.test,
            $.list,
        ),

        _pat: $ => choice(
            $.var,
            $.typeann,
            $.wildcard,
            $.int_lit,
            $.float_lit,
            $.bool_lit,
            $.string,
            $.tuple_pat,
            $.list_pat,
            $.wildcard,
            $.as_pat
        ),

        _type: $ => choice(
            'Int',
            'String',
            'Float',
            'Bool',
            $.tuple_type,
            $.arrow_type,
        ),

        //types:

        tuple_type: $ => seq(
            '(',
            commaSep($._type),
            ')',
        ),

        arrow_type: $ => prec.left(seq(
            $._type,
            '->',
            $._type,
        )),

        //expressions

        test: $ => seq(
            'test',
            $._expression,
            'end',
        ),

        list: $ => seq(
            '[',
            commaSep($._expression),
            ']',
        ),

        //literals:

        int_lit: $ => /[0-9]+/,

        float_lit: $ => /[0-9]+\.[0-9]+/,

        bool_lit: $ => choice(
            'true',
            'false',
        ),

        string: $ => seq(
            '"',
            repeat(choice(
                /[^"\\]/,
                /\\./
            )),
            '"'
        ),

        //infix expressions:

        infix_exp: $ => choice(
            $.plus,
            $.minus,
            $.times,
            $.divide,
            $.pow,
            $.fpow,
            $.assign,
            $.equals,
            $.string_equals,
            $.less_than,
            $.greater_than,
            $.greater_than_equals,
            $.less_than_equals,
            $.fplus,
            $.fminus,
            $.ftimes,
            $.fdivide,
            $.fequals,
            $.float_greater_than,
            $.float_less_than,
            $.float_greater_than_equals,
            $.float_less_than_equals,
            $.substr1,
            $.bitwise_and,
            $.logical_and,
            $.bitwise_or,
            $.logical_or,
        ),

        var: $ => $.ident,

        plus: $ => prec.left(5, seq(
            $._expression,
            '+',
            $._expression,
        )),

        minus: $ => prec.left(5, seq(
            $._expression,
            '-',
            $._expression,
        )),

        times: $ => prec.left(4, seq(
            $._expression,
            '*',
            $._expression,
        )),

        divide: $ => prec.left(4, seq(
            $._expression,
            '/',
            $._expression,
        )),

        pow: $ => prec.right(3, seq(
            $._expression,
            '**',
            $._expression,
        )),

        fpow: $ => prec.right(3, seq(
            $._expression,
            '**.',
            $._expression,
        )),

        assign: $ => prec.right(8, seq(
            $._expression,
            '=',
            $._expression,
        )),

        equals: $ => prec.right(8, seq(
            $._expression,
            '==',
            $._expression,
        )),

        string_equals: $ => prec.right(8, choice(
            seq(
                $._expression,
                '$==',
                $._expression,
            ),
            seq(
                $._expression,
                '$=',
                $._expression,
            ),
            seq(
                $._expression,
                '$',
                $._expression,
            ),
        )),

        less_than: $ => prec.right(5, seq(
            $._expression,
            '<',
            $._expression,
        )),

        greater_than: $ => prec.right(5, seq(
            $._expression,
            '>',
            $._expression,
        )),

        greater_than_equals: $ => prec.right(8, seq(
            $._expression,
            '>=',
            $._expression,
        )),

        less_than_equals: $ => prec.right(8, seq(
            $._expression,
            '<=',
            $._expression,
        )),

        fplus: $ => prec.left(5, seq(
            $._expression,
            '+.',
            $._expression,
        )),

        fminus: $ => prec.left(5, seq(
            $._expression,
            '-.',
            $._expression,
        )),

        ftimes: $ => prec.left(4, seq(
            $._expression,
            '*.',
            $._expression,
        )),

        fdivide: $ => prec.left(4, seq(
            $._expression,
            '/.',
            $._expression,
        )),

        fequals: $ => prec.right(8, seq(
            $._expression,
            '==.',
            $._expression,
        )),

        float_greater_than: $ => prec.right(5, seq(
            $._expression,
            '>.',
            $._expression,
        )),

        float_less_than: $ => prec.right(5, seq(
            $._expression,
            '<.',
            $._expression,
        )),

        float_greater_than_equals: $ => prec.right(8, seq(
            $._expression,
            '>=.',
            $._expression,
        )),

        float_less_than_equals: $ => prec.right(8, seq(
            $._expression,
            '<=.',
            $._expression,
        )),

        substr1: $ => prec.left(8, seq(
            $._expression,
            '=.',
            $._expression,
        )),

        bitwise_and: $ => prec.left(9, seq(
            $._expression,
            '&',
            $._expression,
        )),

        logical_and: $ => prec.left(9, seq(
            $._expression,
            '&&',
            $._expression,
        )),

        bitwise_or: $ => prec.left(5, seq(
            $._expression,
            '|',
            $._expression,
        )),

        logical_or: $ => prec.left(10, seq(
            $._expression,
            '||',
            $._expression,
        )),

        //tuple expressions:
        tuple_exp: $ => seq(
            '(',
            commaSep($._expression),
            ')',
        ),

        //let expressions & related:

        let: $ => seq(
            'let',
            $._pat,
            '=',
            $._expression,
            'in',
            $._expression,
        ),

        //ifs and realted:
        if: $ => seq(
            'if',
            $._expression,
            'then',
            $._expression,
            'else',
            $._expression,
        ),

        //functions and related
        fun: $ => seq(
            'fun',
            $._pat,
            '->',
            $._expression,
        ),

        //application
        ap: $ => prec.left(1, seq(
            $._expression,
            '(',
            $._expression,
            ')'
        )),

        //cases and rules

        case: $ => seq(
            'case',
            $._expression,
            repeat($.rule),
            'end'
        ),

        rule: $ => seq(
            '|',
            $._pat,
            '=>',
            $._expression,
        ),

        //patterns:

        typeann: $ => prec(11, seq(
            $._pat,
            ':',
            $._type,
        )),

        as_pat: $ => prec.left(6, seq(
            $._pat,
            'as',
            $._pat,
        )),

        tuple_pat: $ => seq(
            '(',
            commaSep($._pat),
            ')',
        ),

        list_pat: $ => seq(
            '[',
            commaSep($._pat),
            ']',
        ),

        wildcard: $ => '_',
    }
});
