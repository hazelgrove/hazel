//helpers:
function commaSep1(rule) {
    return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
    return optional(commaSep1(rule))
}


module.exports = grammar({
    name: 'Hazel',

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
        ),

        _pat: $ => choice(
            $.var,
            $.typeann,
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

        //patterns:

        typeann: $ => seq(
            $._pat,
            ':',
            $._type,
        )


    }
});
