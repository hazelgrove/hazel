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
    | TypAbs(_, b, _) => walk(b)
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
    | TypAbs(_, b, _) => walk(b)
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
      "Free variables nested inside a TypParamAp callee are all reported",
      `Quick,
      () => {
        /* The callee position can hold a compound form like
           `A(B)(a)` (parsed as `TypParamAp(TypParamAp(A, B), a)`). All
           unbound names — A and B in this example — should be marked
           as free type variables, and every nested node should have
           its own info entry (so cursor lookups don't fall through to
           "Whitespace or comment"). */
        let marks =
          static_errors(
            {|
type List(a) = + Nil + Cons(a, A(B)(a)) in ?
|},
          );
        let unbound_names =
          List.filter_map(
            fun
            | Mark.TypFreeTypeVariable(n) => Some(n)
            | _ => None,
            marks,
          );
        check(
          bool,
          "A is reported as free",
          true,
          List.mem("A", unbound_names),
        );
        check(
          bool,
          "B is reported as free",
          true,
          List.mem("B", unbound_names),
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
    test_case(
      "Multi-binder poly parses as a single Poly with TPat.Tuple binder",
      `Quick,
      () => {
        /* `poly a, b -> ...` should be a SINGLE `Poly` whose binder is
           a `TPat.Tuple([a, b])` — not a curried chain `Poly(a, Poly(b,
           …))`. Explicit nesting (`poly a -> poly b -> …`) remains
           structurally distinct as a chain. */
        let src = {|
let pair : poly a, b -> a -> b -> (a, b) =
  abs a, b -> fun x : a -> fun y : b -> (x, y) in pair
|};
        let exp = Haz3lcore.Parser.to_term(src, ~root=Exp) |> Option.get;
        /* Walk the AST looking for the user's `poly a, b -> …` annotation
           and the `abs a, b -> …` value. Both should have a single
           `Poly`/`TypAbs` node whose binder is a `TPat.Tuple([_, _])`,
           not a chain of two `Poly`/`TypAbs`s. */
        let poly_tuple_binders = ref(0);
        let abs_tuple_binders = ref(0);
        let nested_poly = ref(0);
        let nested_abs = ref(0);
        let rec walk_typ = (t: Typ.t): unit => {
          switch (t.term) {
          | Poly({term: Tuple([_, _]), _}, body) =>
            incr(poly_tuple_binders);
            walk_typ(body);
          | Poly(_, {term: Poly(_), _}) =>
            incr(nested_poly);
          | Poly(_, body) => walk_typ(body)
          | _ => ()
          };
        };
        let rec walk_pat = (p: Pat.t): unit =>
          switch (p.term) {
          | Asc(inner, t) =>
            walk_pat(inner);
            walk_typ(t);
          | Parens(inner) => walk_pat(inner)
          | _ => ()
          };
        let rec walk_exp = (e: Exp.t): unit => {
          switch (e.term) {
          | TypAbs({term: Tuple([_, _]), _}, body, _) =>
            incr(abs_tuple_binders);
            walk_exp(body);
          | TypAbs(_, {term: TypAbs(_), _}, _) =>
            incr(nested_abs);
          | TypAbs(_, body, _) => walk_exp(body)
          | Let(p, def, body) =>
            walk_pat(p);
            walk_exp(def);
            walk_exp(body);
          | Asc(inner, t) =>
            walk_exp(inner);
            walk_typ(t);
          | Fun(_, body, _, _) => walk_exp(body)
          | Parens(inner) => walk_exp(inner)
          | _ => ()
          };
        };
        walk_exp(exp);
        check(int, "exactly one Poly with a TPat.Tuple binder", 1, poly_tuple_binders^);
        check(int, "no curried Poly chains for the multi-binder form", 0, nested_poly^);
        check(int, "exactly one TypAbs with a TPat.Tuple binder", 1, abs_tuple_binders^);
        check(int, "no curried TypAbs chains for the multi-binder form", 0, nested_abs^);
      },
    ),
    test_case(
      "T(a) form is rejected as a poly binder",
      `Quick,
      () => {
        /* `poly A(a) -> ...` writes a `Param` tpat (`A(a)`) in a
           binder position. Only type-alias *heads* (`type A(a) = ...`)
           accept the parameter-list form; binders should reject it
           with `TPatParamNotAtAliasHead`. */
        let marks =
          static_errors(
            {|
let f : poly A(a) -> Int = ? in f
|},
          );
        check(
          bool,
          "poly A(a) -> reports TPatParamNotAtAliasHead on the binder",
          true,
          List.exists(
            fun
            | Mark.TPatParamNotAtAliasHead("A") => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "T(a) form is rejected as a abs binder",
      `Quick,
      () => {
        let marks =
          static_errors(
            {|
let f = abs A(a) -> ? in f
|},
          );
        check(
          bool,
          "abs A(a) -> reports TPatParamNotAtAliasHead on the binder",
          true,
          List.exists(
            fun
            | Mark.TPatParamNotAtAliasHead("A") => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "T(a) form is rejected nested inside a type-alias head",
      `Quick,
      () => {
        /* The outermost tpat of a type alias may be `T(a, b)`, but
           `T(B(a)) = …` nests another `Param` *inside* the head — the
           inner `B(a)` is not at an alias head and should be flagged
           as `TPatParamNotAtAliasHead`. The outer `T(_)` itself
           remains valid. */
        let marks =
          static_errors(
            {|
type T(B(a)) = a in ?
|},
          );
        check(
          bool,
          "inner B(a) reports TPatParamNotAtAliasHead",
          true,
          List.exists(
            fun
            | Mark.TPatParamNotAtAliasHead("B") => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "Parenthesized binder list `poly (a, b) -> ...` is accepted",
      `Quick,
      () => {
        /* Optional parens around the comma-separated binder list are
           supported and parse to the same `Poly(TPat.Tuple([a, b]),
           …)` shape as the bare comma form. */
        Alcotest.check(
          list(testable_issue),
          "no static errors on poly (a, b) -> ...",
          [],
          static_errors(
            {|
let f : poly (a, b) -> a -> b -> Int = ? in f
|},
          )
          |> List.map(ms => Marks([ms])),
        );
        Alcotest.check(
          list(testable_issue),
          "no static errors on abs (a, b) -> ...",
          [],
          static_errors(
            {|
let g = abs (a, b) -> ? in g
|},
          )
          |> List.map(ms => Marks([ms])),
        );
        /* And the recursive map example with parenthesized binders
           round-trips correctly through the dynamics too: a single-
           binder parens form `(a)` collapses to a bare tpat. */
        Alcotest.check(
          list(testable_issue),
          "no static errors on parenthesized single-binder",
          [],
          static_errors(
            {|
let h = abs (a) -> ? in h
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    test_case(
      "Top-level type alias T(a, b) is still accepted",
      `Quick,
      () => {
        /* Sanity check: a well-formed parameterized type alias
           (`type T(a, b) = …`) has no `TPatParamNotAtAliasHead`
           mark on its head. */
        let marks =
          static_errors(
            {|
type T(a, b) = (a, b) in ?
|},
          );
        check(
          bool,
          "no TPatParamNotAtAliasHead on the alias head",
          false,
          List.exists(
            fun
            | Mark.TPatParamNotAtAliasHead(_) => true
            | _ => false,
            marks,
          ),
        );
      },
    ),
    test_case(
      "Recursive map@<a, b> specializes both binders in one statics step",
      `Quick,
      () => {
        /* The user's example: a recursive `map` declared with the
           multi-binder `poly a, b -> ...` annotation. The recursive
           call `map@<a, b>(tl, f)` must specialize BOTH binders in a
           single `TypAp` step; otherwise a residual `poly b -> …`
           leaks out and the subsequent ordinary application
           `(tl, f)` would fail with an arrow-vs-poly mismatch. */
        Alcotest.check(
          list(testable_issue),
          "no static errors on the recursive multi-binder map",
          [],
          static_errors(
            {|
let map : poly a, b -> ([a], a -> b) -> [b] =
  abs a, b -> fun (xs, f) ->
    case xs
    | [] => []
    | hd::tl => f(hd)::map@<a, b>(tl, f)
    end in
map@<Int, Int>([1, 2, 3], fun x -> x * 2)
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    /* Type-level type function as the body of a type alias.
       `type T = typfun a -> body` is the prefix-binder form of
       `type T(a) = body` and should produce the same kind /
       elaboration shape: `T` has kind `Type -> Type` and
       `T(Int)` normalizes through the existing higher-kinded
       reduction. */
    test_case(
      "type T = typfun a -> body parses + checks like type T(a) = body",
      `Quick,
      () => {
        Alcotest.check(
          list(testable_issue),
          "no static errors on type-level typfun alias body",
          [],
          static_errors(
            {|
type Option = typfun a -> + None + Some(a) in
let x : Option(Int) = Some(3) in x
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    test_case(
      "type-level multi-binder typfun: type Either = typfun a, b -> ...",
      `Quick,
      () => {
        Alcotest.check(
          list(testable_issue),
          "no static errors on type-level multi-binder typfun",
          [],
          static_errors(
            {|
type Either = typfun a, b -> + Left(a) + Right(b) in
let x : Either(Int, Bool) = Right(true) in x
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
    /* Regression: the result type stored on a parameterized
       constructor's polymorphic schema is the alias name applied to
       its parameters in *one* `TypParamAp(name, TypTuple([a, b]))`
       step — not a curried chain `TypParamAp(TypParamAp(name, a),
       b)`. The context inspector renders the curried shape as
       `Either(a)(b)` and the uncurried shape as `Either(a, b)`. */
    test_case(
      "parameterized constructor result type is uncurried",
      `Quick,
      () => {
        let src = {|
type MyEither(a, b) = + A(a) + B(b) in
A
|};
        let exp = parse_menhir_exp(src);
        let (info_map, _elab) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            exp,
          );
        let ctr_schema = ref(None);
        Id.Map.iter(
          (_, info) =>
            if (ctr_schema^ == None) {
              switch (info) {
              | Info.InfoExp({ctx, _}) =>
                switch (Ctx.lookup_ctr(ctx, "A")) {
                | Some({typ, _}) => ctr_schema := Some(typ)
                | None => ()
                }
              | _ => ()
              };
            },
          info_map,
        );
        switch (ctr_schema^) {
        | None => Alcotest.fail("constructor A not found in ctx")
        | Some(t) =>
          /* Schema shape: `Poly(Tuple([a, b]), Arrow(a, result_type))`.
             Result type = the rightmost component of the arrow. */
          let result =
            switch (t.term) {
            | Poly(_, body) =>
              switch (body.term) {
              | Arrow(_, out) => out
              | _ => body
              }
            | _ => t
            };
          switch (result.term) {
          | TypParamAp(callee, arg) =>
            switch (callee.term, arg.term) {
            | (Var(_), TypTuple(args)) =>
              Alcotest.check(
                int,
                "result type's TypTuple holds both args",
                2,
                List.length(args),
              )
            | (TypParamAp(_, _), _) =>
              Alcotest.fail(
                "result type is curried (TypParamAp(TypParamAp(_, _), _)) "
                ++ "instead of uncurried (TypParamAp(_, TypTuple([_, _])))",
              )
            | _ =>
              Alcotest.fail(
                "result type's TypParamAp arg is not a TypTuple",
              )
            }
          | _ =>
            Alcotest.fail("result type is not a TypParamAp")
          };
        };
      },
    ),
    /* Regression: a multi-parameter type alias is stored as a single
       `TypFun(TPat.Tuple([a, b, …]), body)` — *not* a curried
       `TypFun(a, TypFun(b, body))` chain. The context inspector
       displays the stored alias type, so a curried form would render
       as `typfun a -> (typfun b -> body)` instead of the expected
       `typfun a, b -> body`.

       Walks the entire elaboration looking for the `Either` alias
       in any captured ctx and asserts the stored type's outermost
       `TypFun` carries both binders directly. */
    test_case(
      "multi-binder type alias is stored uncurried (single Tuple binder)",
      `Quick,
      () => {
        /* Use `MyEither` (not `Either`) — the latter is a builtin ADT
           shadowing the user's alias, so `lookup_alias` would find
           the builtin's type instead of ours. */
        let src = {|
type MyEither(a, b) = + A(a) + B(b) in
A
|};
        let exp = parse_menhir_exp(src);
        let (info_map, _elab) =
          Statics.mk(
            CoreSettings.on,
            Language.Builtins.ctx_init(Some(Int)),
            exp,
          );
        let alias_ty = ref(None);
        Id.Map.iter(
          (_, info) =>
            if (alias_ty^ == None) {
              switch (info) {
              | Info.InfoExp({ctx, _}) =>
                switch (Ctx.lookup_alias(ctx, "MyEither")) {
                | Some(t) => alias_ty := Some(t)
                | None => ()
                }
              | _ => ()
              };
            },
          info_map,
        );
        switch (alias_ty^) {
        | None => Alcotest.fail("MyEither alias not found in ctx")
        | Some(t) =>
          /* The stored alias type may be wrapped in `Rec` (when the
             alias body references the alias name). For `Either`
             there's no self-reference, so we expect a bare
             `TypFun(Tuple([a, b]), body)`. */
          let inner =
            switch (t.term) {
            | Rec(_, body) => body
            | _ => t
            };
          let n_top_binders =
            switch (inner.term) {
            | TypFun(p, body) =>
              switch (body.term) {
              | TypFun(_, _) =>
                Alcotest.fail(
                  "alias body is curried `TypFun(a, TypFun(b, _))` "
                  ++ "instead of uncurried `TypFun(Tuple([a, b]), _)`",
                )
              | _ => List.length(TPat.binders_of(p))
              }
            | _ =>
              Alcotest.fail("alias body is not a TypFun: " ++ Typ.show(t))
            };
          Alcotest.check(
            int,
            "TypFun's binder lists both `a` and `b`",
            2,
            n_top_binders,
          );
        };
      },
    ),
    test_case(
      "type-level recursive typfun: type List = typfun a -> + Nil + Cons(a, List(a))",
      `Quick,
      () => {
        /* Self-reference inside a type-level typfun body: the alias
           name `List` is captured as a recursive reference (the
           `Var` branch of TyAlias detects it via `free_vars` and
           wraps in `Rec`), and `List(Int)` normalizes via the
           higher-kinded reduction.

           `+ Cons(a, List(a))` declares Cons with a single tuple
           payload `(a, List(a))`, so applications are
           `Cons((1, Nil))`. Same shape as the prefix-binder form
           `type List(a) = …` test elsewhere in this file. */
        Alcotest.check(
          list(testable_issue),
          "no static errors on recursive type-level typfun",
          [],
          static_errors(
            {|
type List = typfun a -> + Nil + Cons(a, List(a)) in
let xs : List(Int) = Cons((1, Nil)) in xs
|},
          )
          |> List.map(ms => Marks([ms])),
        );
      },
    ),
  ],
);
