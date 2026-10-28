open Alcotest;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.TypAp",
  [
    test_case("Explicit polymorphism with type alias", `Quick, () =>
      parse_and_evaluate_test(
        "[5, 3]",
        {|type T=String in
let map = typfun a ->typfun b -> fun f :(a ->b), as : [a] -> let bs : [b] =
  case as
    | [] => []
    | (a :: as) => f(a) :: map@<a>@<b>(f, as)
  end
in bs in

map@<T>@<Int>(fun e -> string_length(e), ["hello","bar"])|},
      )
    ),
    /* Test_Statics_Polymorphism pins the type this result has to agree with. */
    test_case(
      "typfun binder shadows a type alias, so the body keeps `y` (#1717)",
      `Quick,
      () =>
      parse_and_evaluate_test(
        {|typfun y -> (?:y)|},
        {|type y = Int in typfun y -> (? : y)|},
      )
    ),
    /* Built by hand: the parser does not put an annotation inside a broken
       pattern. */
    test_case(
      "A broken binder's written annotation does not reach a type application",
      `Quick,
      () => {
        open Language.IdTagged.FreshGrammar;
        let annotated =
          Pat.asc(
            Pat.var("x"),
            Typ.sig_([Sig.sig_let(Pat.asc(Pat.var("y"), Typ.var("T")))]),
          );
        let body =
          Exp.let_(
            Pat.multi_hole([Language.Grammar.Pat(annotated)]),
            Exp.int(2),
            Exp.int(1),
          );
        evaluation_test(
          "the broken binder matches anything",
          parse_exp("1"),
          elaborate(
            Exp.typ_ap(Exp.typ_fun(TPat.var("T"), body, None), Typ.int()),
          ),
        );
      },
    ),
  ],
);
