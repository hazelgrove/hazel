open Lang;

let annotate_rec_name: (string, exp) => exp = (
  (rec_name, exp) =>
    switch (exp) {
    | EFix(_, param, body) => EFix(Some(rec_name), param, body)

    | _ => exp
    }:
    (string, exp) => exp
);

let lett: (typ, string, exp, exp) => exp = (
  (the_typ, name, binding, body) =>
    EApp(
      false,
      EFix(None, PatParam(PVar(name)), body),
      EAExp(ETypeAnnotation(annotate_rec_name(name, binding), the_typ)),
    ):
    (typ, string, exp, exp) => exp
);

let func_params: (list(param), exp) => exp = (
  List.fold_right((param, body) => EFix(None, param, body)):
    (list(param), exp) => exp
);

let app: (exp, list(exp_arg)) => exp = (
  List.fold_left((acc, arg) => EApp(false, acc, arg)):
    (exp, list(exp_arg)) => exp
);

/* Precondition: input >= 0 */
let nat: int => exp = (
  {
    let rec helper = (acc, n) =>
      if (n == 0) {
        acc;
      } else {
        helper(ECtor("S", [], acc), n - 1);
      };

    helper(ECtor("Z", [], ETuple([])));
  }:
    int => exp
);

let listt: (list(exp), list(typ)) => exp = (
  (es, ts) =>
    List.fold_right(
      (e, acc) => ECtor("Cons", ts, ETuple([e, acc])),
      es,
      ECtor("Nil", ts, ETuple([])),
    ):
    (list(exp), list(typ)) => exp
);

type program = {
  datatypes: datatype_ctx,
  definitions: list((string, (typ, exp))),
  assertions: list((exp, exp)),
  main_opt: option(exp),
};

let program: program => (exp, datatype_ctx) = (
  ({datatypes, definitions, assertions, main_opt}) => (
    Post_parse.exp(
      List.fold_right(
        ((name, (the_typ, the_exp))) => lett(the_typ, name, the_exp),
        definitions,
        List.fold_right(
          ((e1, e2)) => lett(TTuple([]), "_", EAssert(e1, e2)),
          assertions,
          Option2.with_default(ETuple([]), main_opt),
        ),
      ),
    ),
    List.rev(datatypes),
  ):
    program => (exp, datatype_ctx)
);
