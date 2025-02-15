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

let peanut_1b =
  has_errors(
    "Peanut Figure 1b: Inexhaustive + Redundant (Second Pattern)",
    {|
type Tree = +Empty + Leaf(Int) + Node([Tree]) in
let f = fun (x : Tree) ->
  {{{case x
    | Node(x::y::tl) => Node(f(x)::[f(Node(y::tl))])
    | {{{Node([x, y])}}} => Node([f(x), f(y)])
    | Node([x]) => Node([f(x), Empty])
    | Node([]) => Empty
    | Empty => Empty
  end}}}
in ?|},
    [Info.Exp(InexhaustiveMatch(None)), Info.Pat(Redundant(None))],
  );

let peanut_2a =
  no_errors(
    "Peanut Figure 2a: Indeterminately Exhaustive",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
    | [] => false
    | x::? => true
    end
in ?|},
  );

let peanut_2b =
  has_errors(
    "Peanut Figure 2b: Necessarily Exhaustive",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    {{{case xs
      | [] => false
      | x::?::? => true
    end}}} in ?
|},
    [Info.Exp(InexhaustiveMatch(None))],
  );

let peanut_2c =
  no_errors(
    "Peanut Figure 2c: Necessarily Exhaustive",
    {|
let not : Bool -> Bool = ? in
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::? => true
      | x::tl => not(odd_length(tl))
    end in ?
|},
  );

let peanut_3a =
  no_errors(
    "Peanut Figure 3a: Necessarily Irredundant (first two patterns) + Indeterminately Redundant (third pattern)",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::? => true
      | x::y::tl => odd_length(tl)
    end in ?|},
  );

let peanut_3b =
  has_errors(
    "Peanut Figure 3b: Necessarily Redundant (third pattern)",
    {|
let odd_length : [Int] -> Bool =
  fun xs ->
    case xs
      | [] => false
      | x::tl => odd_length(tl)
      | {{{x::?}}} => true
    end in ?|},
    [Info.Pat(Redundant(None))],
  );
// TODO: list examples from paper
// TODO: recursive type
// TODO: integers
// TODO: floats
// TODO: strings
// TODO: Andrew's card example
// TODO: test that exercises the default case
// TODO: unknown scrutinee
// TODO: partially unknown scrutinee - still check exhaustiveness at outer level?
// TODO: double errors

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
    peanut_1b,
    peanut_2a,
    peanut_2b,
    peanut_2c,
    peanut_3a,
    peanut_3b,
  ],
);
