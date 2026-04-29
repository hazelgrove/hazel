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
              actual: TypKind.Arrow([TypKind.Type], TypKind.Type),
            }),
            marks,
          ),
        );
      },
    ),
    test_case(
      "kind mismatch is reported only at the offending node",
      `Quick,
      () => {
        /* `List` appears unapplied inside `Cons(Int, List)`. The
           `Type -> Type` kind mismatch should mark only the `List`
           reference; ancestor nodes (the surrounding `Sum`, `Prod`,
           etc.) shouldn't accumulate the same mark. */
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(Int, List) in ?
|},
          );
        let kind_mismatch_count =
          List.length(
            List.filter(
              fun
              | Mark.TypKindMismatch(_) => true
              | _ => false,
              marks,
            ),
          );
        check(int, "exactly one TypKindMismatch", 1, kind_mismatch_count);
      },
    ),
    test_case(
      "non-uniform recursive parameterized type checks",
      `Quick,
      () => {
        /* `List(a) = + Nil + Cons(a, List((Int, a)))` is non-uniform:
           the recursive occurrence of `List` is applied to a *different*
           type (`(Int, a)`) than the outer parameter `a`. Each `Cons` in
           the example below sits at a different type instantiation, so
           every nested constructor needs its own `TypAp(Cons, …)` wrap
           with the correct argument:
             - outermost Cons :: `Int -> List((Int, Int)) -> List(Int)`
             - middle Cons    :: `(Int, Int) -> List((Int, (Int, Int))) -> List((Int, Int))`
             - inner Cons     :: `(Int, (Int, Int)) -> List((Int, (Int, (Int, Int)))) -> List((Int, (Int, Int)))`
             - innermost Nil  :: at `List((Int, (Int, (Int, Int))))` */
        let src = {|
type List(a) =
  + Nil
  + Cons(a, List((Int, a))) in
let x : List(Int) = Cons(3, Cons((4, 4), Cons((5, (6, 7)), Nil))) in x
|};
        Alcotest.check(
          list(testable_issue),
          "no static errors",
          [],
          List.map(ms => Marks([ms]), static_errors(src)),
        );
      },
    ),
    test_case(
      "non-uniform recursive type wraps every constructor in TypAp",
      `Quick,
      () => {
        let src = {|
type List(a) =
  + Nil
  + Cons(a, List((Int, a))) in
let x : List(Int) = Cons(3, Cons((4, 4), Cons((5, (6, 7)), Nil))) in x
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
        let typ_ap_cons_count = ref(0);
        let typ_ap_nil_count = ref(0);
        walk_elaboration(src, e =>
          switch (e.term) {
          | TypAp({term: Constructor("Cons", _), _}, _) =>
            incr(typ_ap_cons_count)
          | TypAp({term: Constructor("Nil", _), _}, _) =>
            incr(typ_ap_nil_count)
          | _ => ()
          }
        );
        check(
          int,
          "all three Cons constructors are wrapped in TypAp",
          3,
          typ_ap_cons_count^,
        );
        check(int, "Nil is wrapped in TypAp", 1, typ_ap_nil_count^);
      },
    ),
    test_case(
      "non-uniform List.Cons reports Type -> Type kind error when unapplied",
      `Quick,
      () => {
        /* Sanity: the kind of `List` is `Type -> Type` even when the
           payload nests it non-uniformly. */
        let marks =
          static_errors(
            {|
type List(a) =
  + Nil
  + Cons(a, List((Int, a))) in
let x : List = ? in x
|},
          );
        check(
          bool,
          "kind mismatch on unapplied List",
          true,
          has_mark(
            Mark.TypKindMismatch({
              expected: TypKind.Type,
              actual: TypKind.Arrow([TypKind.Type], TypKind.Type),
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
          has_mark(Mark.TypParamApplyNonArrowKind(TypKind.Type), marks),
        );
      },
    ),
    test_case(
      "multi-parameter Either(Int, Bool) is well-kinded",
      `Quick,
      () => {
        Alcotest.check(
          list(testable_issue),
          "no static errors",
          [],
          static_errors(
            {|
type Either(a, b) = + A(a) + B(b) in
let x : Either(Int, Bool) = A(3) in x
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    test_case(
      "Either(Int) reports arity mismatch (expected 2, got 1)",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type Either(a, b) = + A(a) + B(b) in
let x : Either(Int) = A(3) in x
|},
          );
        check(
          bool,
          "Either applied to 1 argument is rejected with arity mismatch",
          true,
          List.exists(
            fun
            | Mark.TypParamApplyArityMismatch({expected: 2, actual: 1, _}) => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "Either((Int, Bool)) reports arity mismatch, not partial application",
      `Quick,
      () => {
        /* The user's parens make `(Int, Bool)` a *single* tuple argument,
           but Either expects two type arguments. The error should clearly
           say "expected 2, got 1" — not the curried-application kind
           mismatch "Expected Type, found Type -> Type". */
        let marks =
          static_errors(
            {|
type Either(a, b) = + A(a) + B(b) in
let x : Either((Int, Bool)) = A(0) in x
|},
          );
        check(
          bool,
          "single-tuple-arg application is an arity mismatch",
          true,
          List.exists(
            fun
            | Mark.TypParamApplyArityMismatch({expected: 2, actual: 1, _}) => true
            | _ => false,
            marks,
          ),
        );
        /* And specifically, no `TypKindMismatch` should be emitted on
           this node — the arity error fully explains the problem. */
        check(
          bool,
          "no spurious kind mismatch from currying",
          false,
          List.exists(
            fun
            | Mark.TypKindMismatch(_) => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "List(Int, Bool) is rejected as wrong arity, not as a list of pairs",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(a, List(a)) in
let x : List(Int, Bool) = ? in x
|},
          );
        let arity_marks =
          List.filter(
            fun
            | Mark.TypParamApplyArityMismatch(_) => true
            | _ => false,
            marks,
          );
        check(
          int,
          "List(Int, Bool) reports an arity mismatch (List takes 1 arg)",
          1,
          List.length(arity_marks),
        );
      },
    ),
    test_case(
      "List((Int, Bool)) — extra parens — is a list of pairs",
      `Quick,
      () => {
        /* The user can disambiguate: `List((Int, Bool))` is a single-arg
           type application whose argument is the `Prod[Int, Bool]`
           tuple, i.e. a list of pairs. This should type-check cleanly. */
        Alcotest.check(
          list(testable_issue),
          "no static errors",
          [],
          static_errors(
            {|
type List(a) = + Nil + Cons(a, List(a)) in
let x : List((Int, Bool)) = Nil in x
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    test_case(
      "Free variable in TypParamAp callee position is reported",
      `Quick,
      () => {
        /* Writing `L(a)` where `L` is not bound should mark `L` as a
           free type variable, not silently give it kind `Type`. The
           callee position of a type parameter application bypasses
           the ordinary `(TypeExpected, Var(_))` status check, so we
           need to report unbound names there explicitly. The callee's
           kind is `Unknown` (not assumed `Type`), so the surrounding
           `TypParamAp` doesn't pile a spurious "cannot apply" or
           arity-mismatch mark on top of the free-variable error. */
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(a, L(a)) in ?
|},
          );
        check(
          bool,
          "L is reported as a free type variable",
          true,
          List.exists(
            fun
            | Mark.TypFreeTypeVariable("L") => true
            | _ => false,
            marks,
          ),
        );
        check(
          bool,
          "no spurious cannot-apply mark on the surrounding TypParamAp",
          false,
          List.exists(
            fun
            | Mark.TypParamApplyNonArrowKind(_)
            | Mark.TypParamApplyArityMismatch(_) => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "Multi-param Either evaluates to a self-typed constructor",
      `Quick,
      () => {
        /* End-to-end: declare `Either(a, b)`, build a value at
           `Either(Int, Bool)`, and check that the elaboration wraps the
           constructor in a `TypAp` that consumes the entire argument
           tuple at once via a `TypTuple` payload. */
        let src = {|
type Either(a, b) = + A(a) + B(b) in
let x : Either(Int, Bool) = A(3) in x
|};
        let typ_ap_with_tuple = ref(0);
        walk_elaboration(src, e =>
          switch (e.term) {
          | TypAp(
              {term: Constructor("A", _), _},
              {term: TypTuple(_), _},
            ) =>
            incr(typ_ap_with_tuple)
          | _ => ()
          }
        );
        check(
          int,
          "A is wrapped in TypAp(_, TypTuple([Int, Bool]))",
          1,
          typ_ap_with_tuple^,
        );
      },
    ),
  ],
);
