open Bark;
open Bark.Syntax;
open Lang;

/* Parser specialization */

type problem =
  | ExpectingLeftParen
  | ExpectingRightParen
  | ExpectingLeftBracket
  | ExpectingRightBracket
  | ExpectingComma
  | ExpectingRightArrow
  | ExpectingLAngle
  | ExpectingRAngle
  | ExpectingSpace
  | ExpectingPound
  | ExpectingDot
  | ExpectingEquals
  | ExpectingDoubleEquals
  | ExpectingHole
  | ExpectingLambda
  | ExpectingPipe
  | ExpectingColon
  | ExpectingFuncSpec
  | ExpectingWildcard
  | ExpectingLineComment
  | ExpectingMultiCommentStart
  | ExpectingMultiCommentEnd
  | ExpectingExactly(int, int)
  | ExpectingMoreIndent
  | ExpectingLet
  | ExpectingIn
  | ExpectingCase
  | ExpectingOf
  | ExpectingType
  | ExpectingAssert
  | ExpectingNat
  | ExpectingConstructorName
  | ExpectingVariableName
  | ExpectingHoleName
  | ExpectingFunctionArity
  | ExpectingTupleSize
  | ExpectingTupleIndex
  | ExpectingName(string, string)
  | NegativeArity(int)
  | ZeroArity
  | ExpectingEnd;

type context =
  | CType
  | CTTuple
  | CTData
  | CTArr
  | CTForall
  | CTVar
  | CTypeParam
  | CTypeArg
  | CPat
  | CPTuple
  | CPVar
  | CPWildcard
  | CExp
  | CELet
  | CEVar
  | CECtor
  | CETuple
  | CEProj
  | CEApp
  | CEHole
  | CELambda
  | CECase
  | CEList
  | CENat
  | CStatement
  | CSDatatype
  | CSDatatypeCtors
  | CSDefinition
  | CSAssertion
  | CSFuncSpec
  | CSFuncSpecInput
  | CSFuncSpecOutput
  | CProgram;

type parser('a) = Bark.parser(context, problem, 'a);

/* Symbols */

let left_paren = Token("(", ExpectingLeftParen);

let right_paren = Token(")", ExpectingRightParen);

let left_bracket = Token("[", ExpectingLeftBracket);

let right_bracket = Token("]", ExpectingRightBracket);

let comma = Token(",", ExpectingComma);

let right_arrow = Token("->", ExpectingRightArrow);

let langle = Token("<", ExpectingLAngle);

let rangle = Token(">", ExpectingRAngle);

let pound = Token("#", ExpectingPound);

let dot = Token(".", ExpectingDot);

let equals = Token("=", ExpectingEquals);

let double_equals = Token("==", ExpectingDoubleEquals);

let hole = Token("??", ExpectingHole);

let lambda = Token("\\", ExpectingLambda);

let pipe = Token("|", ExpectingPipe);

let colon = Token(":", ExpectingColon);

let wildcard = Token("_", ExpectingWildcard);

let line_comment_start = Token("--", ExpectingLineComment);

let multi_comment_start = Token("{-", ExpectingMultiCommentStart);

let multi_comment_end = Token("-}", ExpectingMultiCommentEnd);

/* Keywords */

let forall_keyword = Token("forall", ExpectingLet);

let let_keyword = Token("let", ExpectingLet);

let in_keyword = Token("in", ExpectingIn);

let case_keyword = Token("case", ExpectingCase);

let of_keyword = Token("of", ExpectingOf);

let type_keyword = Token("type", ExpectingType);

let assert_keyword = Token("assert", ExpectingAssert);

/* No-lookahead keywords */

let specify_function_token = Token("specifyFunction", ExpectingFuncSpec);

/* Parser helpers */

let optional: parser('a) => parser(option('a)) = (
  p => one_of([map(x => Some(x), p), succeed(None)]):
    parser('a) => parser(option('a))
);

type indent_strictness =
  | Strict
  | Lax;

let check_indent: indent_strictness => parser(unit) = (
  indent_strictness => {
    let check_ok = (col, indent) =>
      switch (indent_strictness) {
      | Strict => col > indent

      | Lax => col >= indent
      };

    let* ok = succeed(check_ok) |= get_col |= get_indent;
    if (ok) {
      succeed();
    } else {
      problem(ExpectingMoreIndent);
    };
  }:
    indent_strictness => parser(unit)
);

let with_current_indent: parser('a) => parser('a) = (
  p => {
    let* col = get_col;
    with_indent(col, p);
  }:
    parser('a) => parser('a)
);

/* Spaces */

let if_progress: (parser('a), int) => parser(step(int, unit)) = (
  (p, offset) => {
    let+ new_offset = succeed(n => n)->p |= get_offset;
    if (Int.equal(offset, new_offset)) {
      Done();
    } else {
      Loop(new_offset);
    };
  }:
    (parser('a), int) => parser(step(int, unit))
);

let any_spaces: parser(unit) = (
  loop(
    0,
    if_progress(
      one_of([
        line_comment(line_comment_start),
        multi_comment(multi_comment_start, multi_comment_end, Nestable),
        spaces,
      ]),
    ),
  ):
    parser(unit)
);

let single_line_spaces: parser(unit) = (
  loop(
    0,
    if_progress(
      one_of([
        line_comment(line_comment_start),
        chomp_while(Char.equal(' ')),
      ]),
    ),
  ):
    parser(unit)
);

/* Indented spaces */

let sspaces: parser(unit) = (
  succeed()->any_spaces->(check_indent(Strict)): parser(unit)
);

let lspaces: parser(unit) = (
  succeed()->any_spaces->(check_indent(Lax)): parser(unit)
);

let tuple: ('a => 'b, list('a) => 'b, parser('a)) => parser('b) = (
  (single, multiple, item) =>
    map(
      inners =>
        switch (inners) {
        | [inner] => single(inner)

        | _ => multiple(inners)
        },
      sequence(
        ~start=left_paren,
        ~separator=comma,
        ~endd=right_paren,
        ~spaces=lspaces,
        ~item,
        ~trailing=Forbidden,
      ),
    ):
    ('a => 'b, list('a) => 'b, parser('a)) => parser('b)
);

let listt: parser('a) => parser(list('a)) = (
  item =>
    sequence(
      ~start=left_bracket,
      ~separator=comma,
      ~endd=right_bracket,
      ~spaces=lspaces,
      ~item,
      ~trailing=Forbidden,
    ):
    parser('a) => parser(list('a))
);

let wrapped_poly: parser('a) => parser(list('a)) = (
  item =>
    sequence(
      ~start=langle,
      ~separator=comma,
      ~endd=rangle,
      ~spaces=lspaces,
      ~item,
      ~trailing=Forbidden,
    ):
    parser('a) => parser(list('a))
);

let single_wrapped_poly: parser('a) => parser('a) = (
  item =>
    (succeed(x => x)->(symbol(langle))->sspaces |= item)
    ->sspaces
    ->(symbol(rangle)):
    parser('a) => parser('a)
);

let exactly: (int, parser('a)) => parser(list('a)) = (
  (n, p) =>
    loop((n, []), ((k, rev_xs)) =>
      if (k <= 0) {
        succeed(Done(List.rev(rev_xs)));
      } else {
        one_of([
          map(x => Loop((k - 1, [x, ...rev_xs])), p),
          problem(ExpectingExactly(n, n - k)),
        ]);
      }
    ):
    (int, parser('a)) => parser(list('a))
);

let chainl1:
  (context, parser('a), parser('b), parser(('a, 'b) => 'a)) => parser('a) = (
  (chain_context, p_head, p_arg, op) => {
    let rec next = acc =>
      one_of([
        in_context(
          chain_context,
          {
            let* combiner = op;
            p_arg |> and_then(combiner(acc) >> next);
          },
        ),
        succeed(acc),
      ]);

    p_head |> and_then(next);
  }:
    (context, parser('a), parser('b), parser(('a, 'b) => 'a)) => parser('a)
);

let chainr1: (context, parser('a), parser(('a, 'a) => 'a)) => parser('a) = (
  (chain_context, p, op) => {
    let rec rest = acc =>
      one_of([
        in_context(
          chain_context,
          {
            let* combiner = op;
            map(combiner(acc), p |> and_then(rest));
          },
        ),
        succeed(acc),
      ]);

    p |> and_then(rest);
  }:
    (context, parser('a), parser(('a, 'a) => 'a)) => parser('a)
);

let ignore_with: ('a, parser(unit)) => parser('a) = (
  (x, p) => map(_ => x, p): ('a, parser(unit)) => parser('a)
);

/* Character predicates */

let inner_char: char => bool = (
  c =>
    Char2.lowercase_char(c)
    || Char2.uppercase_char(c)
    || Char2.digit_char(c)
    || Char.equal(c, '_'):
    char => bool
);

/* Names */

let reserved_words =
  String_set.of_list([
    "forall",
    "if",
    "then",
    "else",
    "case",
    "of",
    "let",
    "in",
    "type",
    "module",
    "where",
    "import",
    "exposing",
    "as",
    "port",
    "infix",
    "infixl",
    "infixr",
  ]);

let constructor_name: parser(string) = (
  variable(
    ~start=Char2.uppercase_char,
    ~inner=inner_char,
    ~reserved=String_set.empty,
    ~expecting=ExpectingConstructorName,
  ):
    parser(string)
);

let variable_name: parser(string) = (
  variable(
    ~start=Char2.lowercase_char,
    ~inner=inner_char,
    ~reserved=reserved_words,
    ~expecting=ExpectingVariableName,
  ):
    parser(string)
);

/* Types */

let rec typ': unit => parser(typ) = (
  () => {
    let monotype': unit => parser(typ) = (
      () => {
        let rec ground_typ': unit => parser(typ) = (
          () =>
            (
              succeed(t => t)
              |= one_of([
                   in_context(
                     CTTuple,
                     tuple(t => t, ts => TTuple(ts), lazily(typ')),
                   ),
                   in_context(
                     CTData,
                     (
                       succeed((name, args) => TData(name, args))
                       |= constructor_name
                     )
                     ->single_line_spaces
                     |= loop([], rev_args =>
                          one_of([
                            map(
                              a => Loop([a, ...rev_args]),
                              lazily(ground_typ'),
                            ),
                            succeed(Done(List.rev(rev_args))),
                          ])
                        ),
                   ),
                   in_context(
                     CTVar,
                     map(name => TVar(name), variable_name),
                   ),
                 ])
            )
            ->single_line_spaces:
            unit => parser(typ)
        );

        chainr1(
          CTArr,
          lazily(ground_typ'),
          ignore_with(
            (domain, codomain) => TArr(domain, codomain),
            succeed()->(symbol(right_arrow))->any_spaces,
          ),
        );
      }:
        unit => parser(typ)
    );

    let polytype': unit => parser(typ) = (
      () =>
        in_context(
          CTForall,
          (
            succeed((a, bound_type) => TForall(a, bound_type))
            ->(keyword(forall_keyword))
            ->sspaces
            |= variable_name
          )
          ->sspaces
          ->(symbol(dot))
          ->sspaces
          |= lazily(typ'),
        ):
        unit => parser(typ)
    );

    one_of([lazily(polytype'), lazily(monotype')]);
  }:
    unit => parser(typ)
);

let typ: parser(typ) = (in_context(CType, lazily(typ')): parser(typ));

/* Patterns */

let rec pat': unit => parser(pat) = (
  () =>
    one_of([
      in_context(CPTuple, tuple(p => p, ps => PTuple(ps), lazily(pat'))),
      in_context(CPWildcard, ignore_with(PWildcard, symbol(wildcard))),
      in_context(CPVar, map(name => PVar(name), variable_name)),
    ]):
    unit => parser(pat)
);

let pat: parser(pat) = (in_context(CPat, lazily(pat')): parser(pat));

/* Expressions */

let params: parser(list(param)) = (
  loop([], rev_params =>
    one_of([
      (
        succeed(p => Loop(p @ rev_params))
        |= one_of([
             in_context(
               CTypeParam,
               map(
                 taus => taus |> List.rev |> List.map(tau => TypeParam(tau)),
                 wrapped_poly(variable_name),
               ),
             ),
             map(p => [PatParam(p)], pat),
           ])
      )
      ->sspaces,
      succeed(Done(List.rev(rev_params))),
    ])
  ):
    parser(list(param))
);

let rec binding': unit => parser((string, list(param), exp)) = (
  () =>
    (
      (succeed((name, ps, body) => (name, ps, body)) |= variable_name)
      ->sspaces
      |= params
    )
    ->(symbol(equals))
    ->sspaces
    |= lazily(exp'):
    unit => parser((string, list(param), exp))
)

and definition': unit => parser((typ, string, list(param), exp)) = (
  () => {
    let* (name, the_typ) =
      (
        (succeed(Pair2.pair) |= backtrackable(variable_name))
        ->(backtrackable(any_spaces))
        ->(symbol(colon))
        ->any_spaces
        |= typ
      )
      ->any_spaces;
    let* (name', pats, body) = lazily(binding');
    if (!String.equal(name, name')) {
      problem(ExpectingName(name, name'));
    } else {
      succeed((the_typ, name, pats, body));
    };
  }:
    unit => parser((typ, string, list(param), exp))
)

and ground_exp': unit => parser(exp) = (
  () => {
    let branches =
      loop([], rev_branches =>
        one_of([
          (
            (
              (
                succeed((c, x, body) =>
                  Loop([(c, (x, body)), ...rev_branches])
                )
                ->(check_indent(Strict))
                |= constructor_name
              )
              ->sspaces
              |= pat
            )
            ->sspaces
            ->(symbol(right_arrow))
            ->sspaces
            |= lazily(exp')
          )
          ->any_spaces,
          succeed(Done(List.rev(rev_branches))),
        ])
      );

    one_of([
      in_context(
        CELet,
        (
          succeed(((typ, name, ps, body), rest) =>
            Desugar.lett(typ, name, Desugar.func_params(ps, body), rest)
          )
          ->(keyword(let_keyword))
          ->sspaces
          |= lazily(definition')
        )
        ->lspaces
        ->(keyword(in_keyword))
        ->lspaces
        |= lazily(exp'),
      ),
      in_context(
        CECase,
        (
          succeed((scrutinee, branches) => ECase(scrutinee, branches))
          ->(keyword(case_keyword))
          ->sspaces
          |= lazily(exp')
        )
        ->lspaces
        ->(keyword(of_keyword))
        ->sspaces
        |= branches,
      ),
      map(
        name => EVar(name),
        one_of([
          in_context(CEVar, variable_name),
          in_context(CECtor, constructor_name),
        ]),
      ),
      in_context(CETuple, tuple(e => e, es => ETuple(es), lazily(exp'))),
      in_context(
        CEProj,
        (
          (
            succeed((n, i, arg) => EProj(n, i, arg))->(symbol(pound))
            |= Bark.int(ExpectingTupleSize)
          )
          ->(symbol(dot))
          |= Bark.int(ExpectingTupleIndex)
        )
        ->sspaces
        |= lazily(ground_exp'),
      ),
      in_context(
        CEHole,
        succeed(name => EHole(name))->(symbol(hole))
        |= one_of([
             /* Bark.int ExpectingHoleName
                ; */ succeed(Fresh.unused),
           ]),
      ),
      in_context(
        CELambda,
        (succeed(Desugar.func_params)->(symbol(lambda))->sspaces |= params)
        ->(symbol(right_arrow))
        ->sspaces
        |= lazily(exp'),
      ),
      in_context(
        CEList,
        succeed(Desugar.listt)
        |= listt(lazily(exp'))
        |= one_of([wrapped_poly(typ), succeed([])]),
      ),
      in_context(CENat, map(Desugar.nat, Bark.int(ExpectingNat))),
    ]);
  }:
    unit => parser(exp)
  /* Constructors handled in post-processing */
  /* Don't allow user hole names (so each hole name appears at most
   * once in a program) for type checking.
   */
)

and ground_args': unit => parser(list(exp_arg)) = (
  () =>
    one_of([
      in_context(
        CTypeArg,
        map(List.map(tau => EAType(tau)), wrapped_poly(typ)),
      ),
      map(e => [EAExp(e)], lazily(ground_exp')),
    ]):
    unit => parser(list(exp_arg))
)

and exp': unit => parser(exp) = (
  () =>
    with_current_indent(
      chainl1(
        CEApp,
        with_current_indent(lazily(ground_exp')),
        with_current_indent(lazily(ground_args')),
        ignore_with(Desugar.app, backtrackable(sspaces)),
      ),
    ):
    unit => parser(exp)
);

let ground_exp: parser(exp) = (
  in_context(CExp, lazily(ground_exp')): parser(exp)
);

let exp: parser(exp) = (in_context(CExp, lazily(exp')): parser(exp));

let definition: parser((typ, string, list(param), exp)) = (
  lazily(definition'): parser((typ, string, list(param), exp))
);

let arg: parser(exp_arg) = (
  one_of([
    in_context(
      CTypeArg,
      map(tau => EAType(tau), single_wrapped_poly(typ)),
    ),
    map(e => EAExp(e), exp),
  ]):
    parser(exp_arg)
);

/* Programs */

let specify_function_name: parser(int) = (
  {
    let* arity =
      succeed(n => n)->(token(specify_function_token))
      |= one_of([Bark.int(ExpectingFunctionArity), succeed(1)]);
    if (arity < 0) {
      problem(NegativeArity(arity));
    } else if (arity == 0) {
      problem(ZeroArity);
    } else {
      succeed(arity);
    };
  }:
    parser(int)
  /* Important: must be token (not keyword) so no lookahead! */
);

type statement =
  | Datatype((string, (list(string), list((string, typ)))))
  | Definition((string, (typ, exp)))
  | Assertion((exp, exp));

let statement_group: parser(list(statement)) = (
  in_context(
    CStatement,
    one_of([
      in_context(
        CSDatatype,
        (
          (
            succeed((data_name, type_params, ctors) =>
              [Datatype((data_name, (type_params, ctors)))]
            )
            ->(keyword(type_keyword))
            ->sspaces
            |= constructor_name
          )
          ->sspaces
          |= loop([], rev_params =>
               one_of([
                 (succeed(p => Loop([p, ...rev_params])) |= variable_name)
                 ->sspaces,
                 succeed(Done(List.rev(rev_params))),
               ])
             )
        )
        ->sspaces
        ->(symbol(equals))
        ->sspaces
        |= chainr1(
             CSDatatypeCtors,
             (
               succeed((ctor_name, arg) => [(ctor_name, arg)])
               |= constructor_name
             )
             ->sspaces
             |= one_of([typ, succeed(TTuple([]))]),
             ignore_with(
               List.append,
               succeed()->(backtrackable(sspaces))->(symbol(pipe))->sspaces,
             ),
           ),
      ),
      in_context(
        CSAssertion,
        (
          succeed((e1, e2) => [Assertion((e1, e2))])
          ->(keyword(assert_keyword))
          ->sspaces
          |= exp
        )
        ->sspaces
        ->(symbol(double_equals))
        ->sspaces
        |= ground_exp,
      ),
      in_context(
        CSFuncSpec,
        {
          let* (arity, func) =
            (
              (succeed((n, f) => (n, f)) |= specify_function_name)->sspaces
              |= ground_exp
            )
            ->sspaces;
          let+ io_examples =
            listt(
              (
                (
                  succeed((inputs, output) => (inputs, output))
                  ->(symbol(left_paren))
                  |= exactly(
                       arity,
                       in_context(
                         CSFuncSpecInput,
                         (succeed(e => e)->lspaces |= arg)
                         ->lspaces
                         ->(symbol(comma)),
                       ),
                     )
                )
                ->lspaces
                |= in_context(CSFuncSpecOutput, exp)
              )
              ->lspaces
              ->(symbol(right_paren)),
            );
          List.map(
            ((inputs, output)) =>
              Assertion((Desugar.app(func, inputs), output)),
            io_examples,
          );
        },
      ),
      in_context(
        CSDefinition,
        {
          let+ (typ, name, ps, body) = definition;
          [Definition((name, (typ, Desugar.func_params(ps, body))))];
        },
      ),
    ]),
  ):
    parser(list(statement))
);

let program: parser(Desugar.program) = (
  in_context(
    CProgram,
    loop([], rev_statements =>
      one_of([
        (
          succeed(stmts => Loop(List.rev_append(stmts, rev_statements)))
          |= statement_group
        )
        ->any_spaces,
        (
          succeed(main_opt =>
            Done(
              Desugar.(
                List.fold_left(
                  (prog, stmt) =>
                    switch (stmt) {
                    | Datatype(d) => {
                        ...prog,
                        datatypes: [d, ...prog.datatypes],
                      }

                    | Definition(d) => {
                        ...prog,
                        definitions: [d, ...prog.definitions],
                      }

                    | Assertion(a) => {
                        ...prog,
                        assertions: [a, ...prog.assertions],
                      }
                    },
                  {datatypes: [], definitions: [], assertions: [], main_opt},
                  rev_statements,
                )
              ),
            )
          )
          |= optional(exp)
        )
        ->any_spaces
        ->(endd(ExpectingEnd)),
      ])
    ),
  ):
    parser(Desugar.program)
);
