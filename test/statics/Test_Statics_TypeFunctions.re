/* Type functions (`typfun`, parameterized aliases) against the kinds the
   surrounding position expects, and polymorphic constructors used where
   the expected type does not determine their type arguments. */
open Alcotest;
open Language;
open Test_Statics_Prelude;

let static_errors = (src: string): list(Mark.t) =>
  Test_Statics_ParameterizedTypes.static_errors(src);

let kind_mismatch = (actual: TypKind.t): Mark.t =>
  TypKindMismatch({
    expected: TypKind.Type,
    actual,
  });

let type_to_type = TypKind.Arrow([TypKind.Type], TypKind.Type);

let check_marks = (name, expected: list(Mark.t), src) =>
  check(
    list(testable_issue),
    name,
    List.map(~f=m => Marks([m]), expected),
    List.map(~f=m => Marks([m]), static_errors(src)),
  );

let no_errors = (name, src) => check_marks(name, [], src);

let tests = (
  "Statics.TypeFunctions",
  [
    /* Max's examples on #2254: a type function is not a type, so it
       cannot annotate a value, whether bare or under another type. */
    test_case("typfun annotating a value is a kind error", `Quick, () =>
      check_marks(
        "kind mismatch",
        [kind_mismatch(type_to_type)],
        {|let x : typfun A -> A = ? in x|},
      )
    ),
    test_case(
      "typfun inside a product in an alias body is a kind error", `Quick, () =>
      check_marks(
        "kind mismatch on the typfun only",
        [kind_mismatch(type_to_type)],
        {|type T = (Int, typfun B -> B) in ?|},
      )
    ),
    test_case("typfun as a poly body is a kind error", `Quick, () =>
      check_marks(
        "kind mismatch",
        [kind_mismatch(type_to_type)],
        {|let w : poly a -> typfun b -> b = ? in w|},
      )
    ),
    test_case("an unapplied type function alias is a kind error", `Quick, () =>
      check_marks(
        "kind mismatch",
        [kind_mismatch(type_to_type)],
        {|type F = typfun a -> [a] in let y : F = ? in y|},
      )
    ),
    test_case("a poly type cannot be applied to a type", `Quick, () =>
      check_marks(
        "non-arrow kind",
        [TypParamApplyNonArrowKind(TypKind.Type)],
        {|type P = poly a -> a -> a in let z : P(Int) = ? in z|},
      )
    ),
    test_case(
      "curried typfun and its partial application are fine", `Quick, () =>
      no_errors(
        "no errors",
        {|type C = typfun a -> typfun b -> (a, b) in
type C1 = C(Int) in
let c : C1(Bool) = (1, true) in c|},
      )
    ),
    /* Max's "D is not found": constructors of a curried typfun alias. */
    test_case("constructors of a curried typfun alias", `Quick, () =>
      no_errors(
        "no errors",
        {|type PResult = typfun err -> typfun ok -> + E(err) + D(ok) in
let x : PResult(String)(Bool) = D(true) in
let y : PResult(String)(Bool) = E("no") in
(x, y)|},
      )
    ),
    test_case("curried typfun constructor schema", `Quick, () =>
      check(
        string,
        "D's type",
        "poly err, ok -> ok -> PResult(err)(ok)",
        {
          let exp =
            Test_Statics_ParameterizedTypes.parse_menhir_exp(
              {|type PResult = typfun err -> typfun ok -> + E(err) + D(ok) in D@<String, Bool>|},
            );
          let (info_map, _) =
            Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
          let found = ref("not found");
          Id.Map.iter(
            (_, info) =>
              switch (info) {
              | Info.InfoExp({ctx, _}) =>
                switch (Ctx.lookup_ctr(ctx, "D")) {
                | Some({typ, _}) when String.equal(found^, "not found") =>
                  found := Typ.pretty_print(typ)
                | _ => ()
                }
              | _ => ()
              },
            info_map,
          );
          found^;
        },
      )
    ),
    /* Alexander's B2T2 slides: a polymorphic constructor whose expected
       type says nothing about its type arguments is used at unknown ones,
       as before `Option` was parameterized, rather than rejected. */
    test_case("constructors in synthetic position", `Quick, () =>
      no_errors(
        "no errors",
        {|let a = Some(3) in
let b = [Some(1), None] in
let f = Some in
type PResult = typfun err -> typfun ok -> + E(err) + D(ok) in
let d = D(true) in
(a, b, f(2), d)|},
      )
    ),
    test_case("an analyzed constructor is still checked", `Quick, () =>
      check_marks(
        "payload mismatch",
        [
          ExpectationMismatch({
            ana: Atom(Int) |> Typ.temp,
            syn: Atom(Bool) |> Typ.temp,
          }),
        ],
        {|let c : Option(Int) = Some(true) in c|},
      )
    ),
    test_case("a type application's arity is checked", `Quick, () =>
      check_marks(
        "arity mismatch",
        [
          TypAbsApplyArityMismatch({
            expected: 1,
            actual: 2,
          }),
        ],
        {|let id : poly a -> a -> a = abs a -> fun x -> x in id@<Int, Bool>|},
      )
    ),
  ],
);
