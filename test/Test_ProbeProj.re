open Alcotest;
open Haz3lcore;

/* A probe on a call names the function it calls and labels the arguments
   by the variables passed. Parens and projectors around the function or an
   argument do not change what is called, and a module member is called by
   its qualified name. */

let parse = (s: string): Language.Exp.t =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(e) => e
  | None => fail("failed to parse " ++ s)
  };

/* Statics for the first call in `e` (pre-order). */
let first_call_info = (e: Language.Exp.t): Language.Statics.Info.t => {
  module M = {
    exception Found(Language.Exp.t);
  };
  let ap =
    switch (
      Language.Exp.map_term(
        ~f_exp=
          (continue, e) =>
            switch (e.term) {
            | Ap(_) => raise(M.Found(e))
            | _ => continue(e)
            },
        e,
      )
    ) {
    | exception (M.Found(ap)) => ap
    | _ => fail("no call found")
    };
  let (info_map, _) =
    Language.Statics.mk(
      Language.CoreSettings.on,
      Language.Builtins.ctx_init(Some(Int)),
      e,
    );
  switch (Id.Map.find_opt(Language.Exp.rep_id(ap), info_map)) {
  | Some(info) => info
  | None => fail("no statics for the call")
  };
};

let info_of_text = (s: string) => first_call_info(parse(s));

/* Wraps every reference to variable `name` in a fold projector. */
let fold_var = (name: string, e: Language.Exp.t): Language.Exp.t => {
  let (module F) = ProjectorInit.to_module(Fold);
  Language.Exp.map_term(
    ~f_exp=
      (continue, e) =>
        switch (e.term) {
        | Var(x) when x == name =>
          let model =
            switch (F.init(Exp(e))) {
            | Some(m) => m
            | None => fail("fold should init on a variable")
            };
          {
            ...e,
            term:
              Projector(
                {
                  kind: Fold,
                  model,
                },
                e,
              ),
          };
        | _ => continue(e)
        },
    e,
  );
};

let fn_name = info => ProbeProj.get_fn_name_from_statics(info);

let fn_name_tests = [
  test_case("bare variable", `Quick, () =>
    check(
      option(string),
      "f",
      Some("f"),
      fn_name(info_of_text("let f = fun x -> x in f(1)")),
    )
  ),
  test_case("parenthesized variable", `Quick, () =>
    check(
      option(string),
      "(f)",
      Some("f"),
      fn_name(info_of_text("let f = fun x -> x in (f)(1)")),
    )
  ),
  test_case("projected variable", `Quick, () =>
    check(
      option(string),
      "folded f",
      Some("f"),
      fn_name(
        first_call_info(fold_var("f", parse("let f = fun x -> x in f(1)"))),
      ),
    )
  ),
  test_case("module member", `Quick, () =>
    check(
      option(string),
      "M.f",
      Some("M.f"),
      fn_name(
        info_of_text("module M = {\n  let f = fun x -> x\n} in\nM.f(1)"),
      ),
    )
  ),
  test_case("lambda", `Quick, () =>
    check(
      option(string),
      "lambda",
      Some({js|λ|js}),
      fn_name(info_of_text("(fun x -> x)(1)")),
    )
  ),
];

let arg_var_tests = [
  test_case("projected argument", `Quick, () =>
    check(
      list(option(string)),
      "folded y",
      [Some("y")],
      ProbeProj.get_arg_var_info(
        first_call_info(
          fold_var("y", parse("let f = fun x -> x in let y = 1 in f(y)")),
        ),
      ),
    )
  ),
  test_case("projected tuple element", `Quick, () =>
    check(
      list(option(string)),
      "folded y among args",
      [Some("y"), None],
      ProbeProj.get_arg_var_info(
        first_call_info(
          fold_var(
            "y",
            parse("let f = fun (a, b) -> a in let y = 1 in f(y, 2)"),
          ),
        ),
      ),
    )
  ),
];

let tests = [
  ("ProbeProj: call name", fn_name_tests),
  ("ProbeProj: argument names", arg_var_tests),
];
