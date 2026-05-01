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
    test_case("Multi-binder typfun with multi-arg @<>", `Quick, () =>
      /* `typfun a, b -> …` declares a single value-level type-
         abstraction value with two binders (curried internally), and
         `f@<Int, Bool>` applies both type arguments in one step via a
         `TypTuple`. The example uses `pair = typfun a, b -> fun x, y
         -> (x, y)` and then `pair@<Int, Bool>(3, true)` to build a
         pair value. */
      parse_and_evaluate_test(
        "(3, true)",
        {|let pair : poly a, b -> a -> b -> (a, b) =
  typfun a, b -> fun x : a -> fun y : b -> (x, y) in
pair@<Int, Bool>(3)(true)|},
      )
    ),
    /* Regression: a recursive polymorphic `map` with parenthesized
       multi-binder polymorphism (`poly (a, b) -> …`, internal
       `TPat.Tuple`) and `typfun (a, b) -> …` must fully evaluate the
       resulting list — including any arithmetic inside the mapped
       function's body. The bug had `[1 + 1, 2 + 1, 3 + 1]` left
       unevaluated for the multi-binder form even though the curried
       form (`poly a -> poly b -> …`, two single-binder Polys)
       reduced to `[2, 3, 4]` correctly. */
    test_case(
      "Polymorphic map with multi-binder poly evaluates list arithmetic",
      `Quick,
      () =>
      parse_and_evaluate_test(
        "[2, 3, 4]",
        {|let emptylist : poly a -> [a] = typfun a -> [] in
let map : poly (a, b) -> (a -> b, [a]) -> [b] =
  typfun (a, b) -> fun (f : (a -> b), l : [a]) ->
    case l
    | h :: t => f(h)::map@<a, b>(f, t)
    | _ => emptylist@<b>
    end in
map@<Int, Int>((fun x -> x + 1), [1, 2, 3])|},
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
    test_case(
      "Non-uniform recursive parameterized list evaluates and re-statics",
      `Quick,
      () => {
        /* `List(a) = + Nil + Cons(a, List((Int, a)))` is non-uniform:
           each recursive Cons sits at a different type instantiation
           (List(Int) → List((Int, Int)) → List((Int, (Int, Int))) → …).
           The evaluator must specialize each Cons's TypAp wrapper with
           the right argument, and re-statics on the result must not
           produce any marks even though the original alias is gone:
           constructor annotations carry the canonical higher-kinded
           form `TypParamAp(Rec(List, TypLam(a, …)), arg)` so re-statics
           can unfold one step on demand. */
        let src = {|type List(a) =
  + Nil
  + Cons(a, List((Int, a))) in
let x : List(Int) = Cons(3, Cons((4, 4), Cons((5, (6, 7)), Nil))) in x|};
        let exp = Haz3lcore.Parser.to_term(src, ~root=Exp) |> Option.get;
        let (_info_map, elab) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            exp,
          );
        let evaluated =
          Evaluator.evaluate(~env=Language.Builtins.env_init, elab) |> fst;
        /* No stuck TypAps in the evaluated result: every TypAp wrapping
           a Constructor should have been reduced. */
        let stuck_typ_aps = ref(0);
        let rec walk = (e: Exp.t): unit => {
          switch (e.term) {
          | TypAp({term: Constructor(_), _}, _) => incr(stuck_typ_aps)
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
        check(
          int,
          "no stuck TypAp(Constructor, _) in evaluated result",
          0,
          stuck_typ_aps^,
        );
        let (restatics_map, _) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            evaluated,
          );
        let all_marks =
          Id.Map.fold(
            (_, info, acc) =>
              switch (info) {
              | Info.InfoExp({marks, _}) => marks @ acc
              | _ => acc
              },
            restatics_map,
            [],
          );
        check(
          int,
          "no marks on non-uniform recursive list result",
          0,
          List.length(all_marks),
        );
      },
    ),
    test_case(
      "Result of nested parameterized ctors re-statics without error marks",
      `Quick,
      () => {
        let src = {|type List(a) = + Nil + Cons(a, List(a)) in
let x : List(Int) = Cons(0, Cons(10, Nil)) in x|};
        let exp = Haz3lcore.Parser.to_term(src, ~root=Exp) |> Option.get;
        let (_info_map, elab) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            exp,
          );
        let evaluated =
          Evaluator.evaluate(~env=Language.Builtins.env_init, elab) |> fst;
        let (restatics_map, _) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            evaluated,
          );
        let all_marks =
          Id.Map.fold(
            (_, info, acc) =>
              switch (info) {
              | Info.InfoExp({marks, _}) => marks @ acc
              | _ => acc
              },
            restatics_map,
            [],
          );
        check(
          int,
          "re-statics of the evaluated result produces no marks",
          0,
          List.length(all_marks),
        );
      },
    ),
  ],
);
