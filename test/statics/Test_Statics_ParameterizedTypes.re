open Test_Statics_Prelude;
open Alcotest;
open Language;

let parse_menhir_exp = (src: string): TermBase.Exp.t =>
  MenhirParser.Interface.parse_program(src)
  |> MenhirParser.Conversion.Exp.of_menhir_ast
  |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.mk_internal([Id.mk()]));

let static_errors = src => {
  let exp = parse_menhir_exp(src);
  statics(exp) |> errors |> List.map(snd) |> List.flatten;
};

/* Run the elaborator and walk the entire result. */
let walk_elaboration = (src, visit) => {
  let exp = parse_menhir_exp(src);
  let (_info_map, elab) =
    Statics.mk(CoreSettings.on, Language.Builtins.ctx_init(Some(Int)), exp);
  let rec walk = (e: TermBase.Exp.t): unit => {
    visit(e);
    switch (e.term) {
    | TypAp(inner, _) => walk(inner)
    | Ap(_, f, a) =>
      walk(f);
      walk(a);
    | Let(_, def, body) =>
      walk(def);
      walk(body);
    | TyAlias(_, _, body) => walk(body)
    | Use(_, body) => walk(body)
    | Tuple(xs) => List.iter(walk, xs)
    | Fun(_, b, _, _)
    | TypFun(_, b, _) => walk(b)
    | Asc(inner, _)
    | Parens(inner) => walk(inner)
    | Match(scrut, rules) =>
      walk(scrut);
      List.iter(((_, r)) => walk(r), rules);
    | _ => ()
    };
  };
  walk(elab);
};

let elaboration_contains = (src, predicate) => {
  let found = ref(false);
  walk_elaboration(src, e =>
    if (predicate(e)) {
      found := true;
    }
  );
  found^;
};

/* A "fallback" constructor is a `Constructor` node that appears in the
   elaboration *without* a surrounding `TypAp` wrapper. The TypAp spine
   is what records the constructor's implicit type instantiation, so a
   constructor that should be polymorphic but lacks a TypAp wrapper is
   considered a fallback. We count by walking the elaboration but
   skipping any `Constructor` directly inside a `TypAp`. */
let count_fallback_constructors = (src, name): int => {
  let count = ref(0);
  let exp = parse_menhir_exp(src);
  let (_info_map, elab) =
    Statics.mk(CoreSettings.on, Language.Builtins.ctx_init(Some(Int)), exp);
  let rec walk = (~inside_typ_ap=false, e: TermBase.Exp.t): unit => {
    switch (e.term) {
    | Constructor(c, Some(Some(_))) when c == name && !inside_typ_ap =>
      incr(count)
    | _ => ()
    };
    switch (e.term) {
    | TypAp(inner, _) => walk(~inside_typ_ap=true, inner)
    | Ap(_, f, a) =>
      walk(f);
      walk(a);
    | Let(_, def, body) =>
      walk(def);
      walk(body);
    | TyAlias(_, _, body) => walk(body)
    | Use(_, body) => walk(body)
    | Tuple(xs) => List.iter(walk, xs)
    | Fun(_, b, _, _)
    | TypFun(_, b, _) => walk(b)
    | Asc(inner, _)
    | Parens(inner) => walk(inner)
    | Match(scrut, rules) =>
      walk(scrut);
      List.iter(((_, r)) => walk(r), rules);
    | _ => ()
    };
  };
  walk(elab);
  count^;
};

let is_typ_ap_of_constructor = (name, e: TermBase.Exp.t): bool =>
  switch (e.term) {
  | TypAp({term: Constructor(c, _), _}, _) => c == name
  | _ => false
  };

let has_mark = (expected: Mark.t, marks: list(Mark.t)): bool =>
  List.exists(mark => equal_mark(mark, expected), marks);

let tests = (
  "Statics.ParameterizedTypes",
  [
    test_case(
      "applied type constructor has kind Type",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = ? in x
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "expected Option(Int) drives Some(Int)",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(3) in x
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "wrong parameterized constructor payload is rejected",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(true) in x
|},
          );

        check(bool, "payload mismatch", true, !List.is_empty(marks));
      },
    ),
    test_case(
      "recursive List(Int) constructor payload checks",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons((1, Nil)) in xs
|},
          );

        Alcotest.check(
          list(testable_issue),
          "Static Errors",
          [],
          List.map(ms => Marks([ms]), marks),
        );
      },
    ),
    test_case(
      "bare type constructor rejected in Type position",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Option(a) = + None + Some(a) in
let x : Option = ? in x
|},
          );

        check(
          bool,
          "kind mismatch",
          true,
          has_mark(
            Mark.TypKindMismatch({
              expected: TypKind.Type,
              actual: TypKind.Arrow(TypKind.Type, TypKind.Type),
            }),
            marks,
          ),
        );
      },
    ),
    test_case("elaboration wraps Some in TypAp for Option(Int)", `Quick, () => {
      check(
        bool,
        "TypAp(Some, ...) appears in elab",
        true,
        elaboration_contains(
          {|
type Option(a) = + None + Some(a) in
let x : Option(Int) = Some(3) in x
|},
          is_typ_ap_of_constructor("Some"),
        ),
      )
    }),
    test_case("elaboration wraps recursive Cons/Nil in TypAp", `Quick, () => {
      check(
        bool,
        "TypAp(Cons, ...) appears in elab",
        true,
        elaboration_contains(
          {|
type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons((1, Nil)) in xs
|},
          is_typ_ap_of_constructor("Cons"),
        ),
      )
    }),
    test_case(
      "elaboration wraps every nested Cons/Nil, not just the outermost",
      `Quick,
      () => {
        let src = {|
type List(a) = + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons(0, Cons(1, Cons(2, Nil))) in xs
|};
        check(
          int,
          "no Cons should fall back to the type-ascribed form",
          0,
          count_fallback_constructors(src, "Cons"),
        );
        check(
          int,
          "no Nil should fall back to the type-ascribed form",
          0,
          count_fallback_constructors(src, "Nil"),
        );
        /* Verify there are actually multiple TypAp(Cons, Int) wrappers in
           the elaboration — the fallback count being 0 could be vacuous
           if no constructors existed at all. */
        let count_typ_ap_cons = ref(0);
        walk_elaboration(src, e =>
          switch (e.term) {
          | TypAp({term: Constructor("Cons", _), _}, _) =>
            incr(count_typ_ap_cons)
          | _ => ()
          }
        );
        check(
          int,
          "three Cons constructors are each wrapped in TypAp",
          3,
          count_typ_ap_cons^,
        );
      },
    ),
    test_case(
      "elaboration wraps constructor via Type-kinded alias", `Quick, () => {
      check(
        bool,
        "TypAp(Some, ...) appears via IntOption alias",
        true,
        elaboration_contains(
          {|
type Option(a) = + None + Some(a) in
type IntOption = Option(Int) in
let x : IntOption = Some(3) in x
|},
          is_typ_ap_of_constructor("Some"),
        ),
      )
    }),
    test_case(
      "non-constructor type application rejected",
      `Quick,
      () => {
        let marks = static_errors({|let x : Int(Bool) = ? in x|});

        check(
          bool,
          "apply non-arrow kind",
          true,
          has_mark(Mark.TypApplyNonArrowKind(TypKind.Type), marks),
        );
      },
    ),
  ],
);
