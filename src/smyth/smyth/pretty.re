open Lang;

/* Parameters */

let indent_size: int = (2: int);

let max_inner: int = (30: int);

/* Pretty printing types */

type state = {
  indent: int,
  app_needs_parens: bool,
  fancy_needs_parens: bool,
};

type printer('a) = (state, 'a) => string;

/* Helpers */

let make_indent: int => string = (
  n => String.make(indent_size * n, ' '): int => string
);

/* Collections */

type paren_type =
  | Round
  | Square
  | Angle;

let left_paren: paren_type => string = (
  fun
  | Round => "("
  | Square => "["
  | Angle => "<":
    paren_type => string
);

let right_paren: paren_type => string = (
  fun
  | Round => ")"
  | Square => "]"
  | Angle => ">":
    paren_type => string
);

let collection: (paren_type, state, printer('a), list('a)) => string = (
  (paren_type, state, print, comps) => {
    let comp_strings =
      List.map(
        print({
          indent: state.indent + 1,
          app_needs_parens: false,
          fancy_needs_parens: false,
        }),
        comps,
      );

    let inside_len = comp_strings |> List.map(String.length) |> List2.sum;

    let contains_newline =
      comp_strings |> List.exists(s => String.contains(s, '\n'));

    let (left, sep, right) =
      if (contains_newline || inside_len > max_inner) {
        let indent = make_indent(state.indent);

        (
          left_paren(paren_type) ++ " ",
          "\n" ++ indent ++ ", ",
          "\n" ++ indent ++ right_paren(paren_type),
        );
      } else {
        (left_paren(paren_type), ", ", right_paren(paren_type));
      };

    left ++ String.concat(sep, comp_strings) ++ right;
  }:
    (paren_type, state, printer('a), list('a)) => string
);

let wrapped_poly: string => string = (s => "<" ++ s ++ ">": string => string);

/* Applications */

let application:
  (state, printer('head), 'head, printer('arg), 'arg) => string = (
  (state, print_head, head, print_arg, arg) => {
    let head_string =
      print_head(
        {
          indent: state.indent,
          app_needs_parens: false,
          fancy_needs_parens: true,
        },
        head,
      );

    let arg_string =
      print_arg(
        {
          indent: state.indent + 1,
          app_needs_parens: true,
          fancy_needs_parens: true,
        },
        arg,
      );

    let sep =
      if (String.contains(arg_string, '\n')) {
        "\n" ++ make_indent(state.indent + 1);
      } else {
        " ";
      };

    let inner = head_string ++ sep ++ arg_string;

    if (state.app_needs_parens) {
      "(" ++ inner ++ ")";
    } else {
      inner;
    };
  }:
    (state, printer('head), 'head, printer('arg), 'arg) => string
);

/* Patterns */

let rec pat': printer(pat) = (
  state =>
    fun
    | PVar(name) => name

    | PTuple(comps) => collection(Round, state, pat', comps)

    | PWildcard => "_":
    printer(pat)
);

let pat: pat => string = (
  pat'({indent: 0, app_needs_parens: false, fancy_needs_parens: false}):
    pat => string
);

/* Types */

let rec typ': printer(typ) = (
  state =>
    fun
    | TArr(input, output) => {
        let inner =
          typ'(
            {
              indent: state.indent,
              app_needs_parens: false,
              fancy_needs_parens: true,
            },
            input,
          )
          ++ " -> "
          ++ typ'(
               {
                 indent: state.indent,
                 app_needs_parens: false,
                 fancy_needs_parens: false,
               },
               output,
             );

        if (state.fancy_needs_parens) {
          "(" ++ inner ++ ")";
        } else {
          inner;
        };
      }

    | TTuple(comps) => collection(Round, state, typ', comps)

    | TData(name, type_args) => {
        let type_args_string =
          if (type_args == []) {
            "";
          } else {
            " " ++ String.concat(" ", List.map(typ'(state), type_args));
          };

        name ++ type_args_string;
      }

    | TForall(x, t) => "forall " ++ x ++ ". " ++ typ'(state, t)

    | TVar(x) => x:
    printer(typ)
);

let typ: typ => string = (
  typ'({indent: 0, app_needs_parens: false, fancy_needs_parens: false}):
    typ => string
);

/* Expressions */

let rec try_sugar: (state, exp) => option(string) = (
  (state, exp) =>
    switch (Sugar.nat(exp)) {
    | Some(n) => Some(string_of_int(n))

    | None =>
      switch (Sugar.listt(exp)) {
      | Some((exp_list, [])) =>
        Some(collection(Square, state, exp', exp_list))

      | Some((exp_list, type_args)) =>
        Some(
          collection(Square, state, exp', exp_list)
          ++ collection(Angle, state, typ', type_args),
        )

      | None => None
      }
    }:
    (state, exp) => option(string)
)

and exp': printer(exp) = (
  (state, exp) =>
    switch (try_sugar(state, exp)) {
    | Some(sugar) => sugar

    | None =>
      switch (exp) {
      | EFix(rec_name_opt, PatParam(param_pat), body) =>
        let lambda =
          "\\"
          ++ pat'(
               {
                 indent: state.indent,
                 app_needs_parens: true,
                 fancy_needs_parens: true,
               },
               param_pat,
             )
          ++ " -> "
          ++ exp'(
               {
                 indent: state.indent,
                 app_needs_parens: false,
                 fancy_needs_parens: false,
               },
               body,
             );

        let inner =
          switch (rec_name_opt) {
          | Some(rec_name) =>
            "let " ++ rec_name ++ " = " ++ lambda ++ " in " ++ rec_name

          | None => lambda
          };

        if (state.fancy_needs_parens) {
          "(" ++ inner ++ ")";
        } else {
          inner;
        };

      | EFix(rec_name_opt, TypeParam(x), body) =>
        exp'(
          state,
          EFix(rec_name_opt, PatParam(PVar(wrapped_poly(x))), body),
        )

      | EApp(_, head, EAExp(arg)) =>
        application(state, exp', head, exp', arg)

      | EApp(_, head, EAType(arg)) =>
        application(
          state,
          exp',
          head,
          (state, s) => wrapped_poly(typ'(state, s)),
          arg,
        )

      | EVar(name) => name

      | ETuple(comps) => collection(Round, state, exp', comps)

      | EProj(n, i, arg) =>
        application(
          state,
          (_, _) => "#" ++ string_of_int(n) ++ "." ++ string_of_int(i),
          (),
          exp',
          arg,
        )

      | ECtor(ctor_name, type_args, arg) =>
        let type_args_string =
          if (type_args == []) {
            "";
          } else {
            collection(Angle, state, typ', type_args);
          };

        let head_string = ctor_name ++ type_args_string;

        if (Exp.syntactically_equal(arg, ETuple([]))) {
          head_string;
        } else {
          application(state, (_, _) => head_string, (), exp', arg);
        };

      | ECase(scrutinee, branches) =>
        let indent1 = make_indent(state.indent + 1);

        let indent2 = make_indent(1) ++ indent1;

        let print_branch = ((ctor_name, (param_pat, body))) =>
          indent1
          ++ ctor_name
          ++ " "
          ++ pat'(
               {
                 indent: state.indent + 1,
                 app_needs_parens: true,
                 fancy_needs_parens: true,
               },
               param_pat,
             )
          ++ " -> \n"
          ++ indent2
          ++ exp'(
               {
                 indent: state.indent + 2,
                 app_needs_parens: false,
                 fancy_needs_parens: false,
               },
               body,
             );

        let inner =
          "case "
          ++ exp'(
               {
                 indent: state.indent,
                 app_needs_parens: false,
                 fancy_needs_parens: false,
               },
               scrutinee,
             )
          ++ " of\n"
          ++ String.concat("\n\n", List.map(print_branch, branches));

        if (state.fancy_needs_parens) {
          "(" ++ inner ++ ")";
        } else {
          inner;
        };

      | EHole(_) => "??"

      | EAssert(_, _) => "{ASSERTION}"

      | ETypeAnnotation(the_exp, the_typ) =>
        let inner =
          exp'(
            {
              indent: state.indent,
              app_needs_parens: true,
              fancy_needs_parens: true,
            },
            the_exp,
          )
          ++ " : "
          ++ typ'(
               {
                 indent: state.indent,
                 app_needs_parens: false,
                 fancy_needs_parens: false,
               },
               the_typ,
             );

        if (state.fancy_needs_parens) {
          "(" ++ inner ++ ")";
        } else {
          inner;
        };
      }
    }:
    printer(exp)
);

let exp: exp => string = (
  exp'({indent: 0, app_needs_parens: false, fancy_needs_parens: false}):
    exp => string
);
