open Alcotest;
open Haz3lcore;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);
let testable_info_error_pat =
  testable(Fmt.using(Info.show_error_pat, Fmt.string), Info.equal_error_pat);
let testable_list_uuidm = testable(Fmt.list(Uuidm.pp), (==));

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);
let info_error_of_pat_id = (f: Exp.t, id: Id.t): option(Info.error_pat) => {
  Statics.get_pat_error_at(statics(f), id);
};

let no_errors = (name, exp) => {
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(exp);
      let errors = Statics.Map.error_ids(s);
      Alcotest.check(testable_list_uuidm, "Static Errors", [], errors);
    },
  );
};

let parse_exp = (s: string) => {
  switch (MakeTerm.parse_exp(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let parse_menhir = (s: string) => {
  let (e, _) =
    Haz3lmenhir.Conversion.Exp.of_menhir_ast(
      Haz3lmenhir.Interface.parse_program(s),
    );
  // print_endline("Parsed: " ++ Exp.show(e));
  // print_endline("Original: " ++ (parse_exp(s) |> Exp.show));
  // failwith("X");
  e;
};

let reusable_id = Id.mk();
let reusable_pat: TermBase.pat_term => TermBase.pat_t =
  p => {
    {ids: [reusable_id], term: p, copied: false};
  };

let bare_let =
  test_case("Bare let has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let x = 1 in x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(let_(reusable_pat(Var("x")), int(1), var("x"))),
        reusable_id,
      ),
    )
  );

let bare_fun =
  test_case("Bare fun has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun x -> x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(fun_(reusable_pat(Var("x")), var("x"), None, None)),
        reusable_id,
      ),
    )
  );

let annotated_let =
  test_case("Annotated let has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let x : Int = 1 in x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          let_(
            reusable_pat(Cast(pvar("x"), tint(), tunknown(Internal))),
            int(1),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_fun =
  test_case("Annotated fun has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun x : Int -> x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          fun_(
            reusable_pat(Cast(pvar("x"), tint(), tunknown(Internal))),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

let let_tuple =
  test_case("Let binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let (x, y, z) = 1 in x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          let_(
            reusable_pat(Tuple([pvar("x"), pvar("y"), pvar("z")])),
            tuple([int(1), int(2), int(3)]),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let fun_tuple =
  test_case("Fun binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun (x, y, z) => x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          fun_(
            reusable_pat(Tuple([pvar("x"), pvar("y"), pvar("z")])),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_let_tuple =
  test_case(
    "Annotated let binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "let (x, y, z): (Int, Int, Int) = 1 in x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          let_(
            reusable_pat(
              Cast(
                ptuple([pvar("x"), pvar("y"), pvar("z")]),
                tprod([tint(), tint(), tint()]),
                tunknown(Internal),
              ),
            ),
            tuple([int(1), int(2), int(3)]),
            var("x"),
          )
        ),
        reusable_id,
      ),
    )
  );

let annotated_fun_tuple =
  test_case(
    "Annotated fun binding a tuple has no error on pattern", `Quick, () =>
    Alcotest.check(
      Alcotest.option(testable_info_error_pat),
      "fun (x, y, z) : (Int, Int, Int) -> x",
      None,
      info_error_of_pat_id(
        Term.Fresh.(
          fun_(
            reusable_pat(
              Cast(
                ptuple([pvar("x"), pvar("y"), pvar("z")]),
                tprod([tint(), tint(), tint()]),
                tunknown(Internal),
              ),
            ),
            var("x"),
            None,
            None,
          )
        ),
        reusable_id,
      ),
    )
  );

let peanut_tree =
  no_errors(
    "Peanut Figure 1: Exhaustive + Irredundant Tree",
    parse_menhir(
      {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Node([]) => Empty
    | Node([x]) => Node([f(x), Empty])
    | Node([x, y]) => Node([f(x), f(y)])
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | Leaf(x) => Leaf(x)
    | Empty => Empty
  end}}}
in ?
      |},
    ),
  );
// TODO: list examples from paper
// TODO: first example from paper
// TODO: recursive type
// TODO: integers
// TODO: floats
// TODO: strings
// TODO: Andrew's card example
// TODO: test that exercises the default case
// TODO: unknown scrutinee
// TODO: partially unknown scrutinee - still check exhaustiveness at outer level?

let tests = (
  "Pattern Coverage Checker",
  [
    bare_let,
    bare_fun,
    annotated_let,
    annotated_fun,
    let_tuple,
    fun_tuple,
    annotated_let_tuple,
    annotated_fun_tuple,
    peanut_tree,
  ],
);
