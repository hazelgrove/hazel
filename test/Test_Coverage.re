open Alcotest;
open Haz3lcore;

let testable_error_map =
  testable(
    Fmt.using(Statics.Map.show_error_map, Fmt.string),
    Statics.Map.equal_error_map,
  );

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init);

let parse_menhir = (s: string) => {
  Haz3lmenhir.Conversion.Exp.of_menhir_ast(
    Haz3lmenhir.Interface.parse_program(s),
  );
};

let has_errors = (name: string, exp: string, errors: list(Info.error)) => {
  test_case(
    name,
    `Quick,
    () => {
      let (e, ids) = parse_menhir(exp);
      let s = statics(e);
      let actual_errors = Statics.Map.errors(s);
      let expected_errors = Id.Map.of_list(List.combine(ids, errors));
      Alcotest.check(
        testable_error_map,
        "Static Errors",
        expected_errors,
        actual_errors,
      );
    },
  );
};

let no_errors = (name: string, exp: string) => has_errors(name, exp, []);

let reusable_id = Id.mk();
let reusable_pat: TermBase.pat_term => TermBase.pat_t =
  p => {
    {ids: [reusable_id], term: p, copied: false};
  };

let bare_let =
  no_errors("Bare let has no error on pattern", "let x = 1 in x");

let bare_fun = no_errors("Bare fun has no error on pattern", "fun x -> x");

let annotated_let =
  no_errors("Annotated let has no error on pattern", "let x : Int = 1 in x");

let annotated_fun =
  no_errors("Annotated fun has no error on pattern", "fun (x : Int) -> x");

let let_tuple =
  no_errors(
    "Let binding a tuple has no error on pattern",
    "let (x, y, z) = (1, 2, 3) in x",
  );

let fun_tuple =
  no_errors(
    "Fun binding a tuple has no error on pattern",
    "fun (x, y, z) -> x",
  );

let annotated_let_tuple =
  no_errors(
    "Annotated let binding a tuple has no error on pattern",
    "let (x, y, z): (Int, Int, Int) = (1, 2, 3) in x",
  );

let annotated_fun_tuple =
  no_errors(
    "Annotated fun binding a tuple has no error on pattern",
    "fun ((x, y, z) : (Int, Int, Int)) -> x",
  );

let peanut_1a =
  no_errors(
    "Peanut Figure 1a: Exhaustive + Irredundant Tree",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  case x
    | Node([]) => Empty
    | Node([x]) => Node([f(x), Empty])
    | Node([x, y]) => Node([f(x), f(y)])
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | Leaf(x) => Leaf(x)
    | Empty => Empty
  end
in ?|},
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
    peanut_1a,
  ],
);
