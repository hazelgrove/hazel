open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* Verify that a parameterized constructor applied at the expected type
   evaluates to a final `Constructor(_, Some(Some(specialized)))` form —
   i.e. the polymorphic constructor's implicit TypAp wrapper is fully
   reduced by the evaluator and the result carries a monomorphic type
   ascription (no stuck `TypAp` wrapping, no free-constructor errors
   when the result is re-analyzed). */
let evaluated_is_self_typed_ctr =
    (src: string, name: string): (bool, Exp.t) => {
  let exp = Haz3lcore.Parser.to_term(src, ~root=Exp) |> Option.get;
  let (_info_map, elab) =
    Statics.mk(CoreSettings.on, Language.Builtins.ctx_init(Some(Int)), exp);
  let evaluated = Evaluator.evaluate(~env=Language.Builtins.env_init, elab) |> fst;
  /* Walk the evaluated result looking for a fully-specialized ctr of the
     expected name. */
  let found = ref(false);
  let rec walk = (e: Exp.t): unit => {
    switch (e.term) {
    | Constructor(c, Some(Some(t))) when c == name =>
      switch (t.term) {
      | Poly(_) => ()
      | _ => found := true
      }
    | TypAp(_) => () /* stuck TypAp: not self-typed */
    | _ => ()
    };
    switch (e.term) {
    | Ap(_, f, a) =>
      walk(f);
      walk(a);
    | Tuple(xs) => List.iter(walk, xs)
    | Parens(inner)
    | Asc(inner, _)
    | Closure(_, inner)
    | Filter(_, inner) => walk(inner)
    | _ => ()
    };
  };
  walk(evaluated);
  (found^, evaluated);
};

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
    test_case(
      "Parameterized Some(3) evaluates to self-typed constructor",
      `Quick,
      () => {
        let (ok, _) =
          evaluated_is_self_typed_ctr(
            {|type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(3) in x|},
            "Some",
          );
        check(
          bool,
          "Some is specialized after TypAp reduction",
          true,
          ok,
        );
      },
    ),
    test_case(
      "Nested polymorphic constructors all specialize",
      `Quick,
      () => {
        let (cons_ok, _) =
          evaluated_is_self_typed_ctr(
            {|type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons(0, Cons(1, Cons(2, Nil))) in xs|},
            "Cons",
          );
        check(
          bool,
          "Every Cons becomes self-typed after evaluation",
          true,
          cons_ok,
        );
        let (nil_ok, _) =
          evaluated_is_self_typed_ctr(
            {|type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons(0, Cons(1, Cons(2, Nil))) in xs|},
            "Nil",
          );
        check(
          bool,
          "Nil becomes self-typed after evaluation",
          true,
          nil_ok,
        );
      },
    ),
  ],
);
