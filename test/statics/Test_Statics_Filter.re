/* Tests for statics on `debug action(pat) in body` filter expressions.

   The Filter form parses to `Filter(Ap(action, pat), body)` with `action`
   resolving to a `FilterAction`. The Statics handler lifts this into
   `Filter(Filter({act, pat, ids}), body)` for elaboration. While doing so
   it must still write an info entry for the `Ap` wrapper itself —
   otherwise the cursor between `action` and `(pat)` has no info to
   resolve, and ExplainThis falls back to "Whitespace or Comment". */

open Alcotest;
open Test_Statics_Prelude;
open Language;

/* A filter is transparent to typing: `debug act(pat) in body` must have
   exactly the type of `body`. */
let filter_preserves_type = (name, src) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(src);
      let m = statics(exp);
      let body =
        switch (Exp.term_of(exp)) {
        | Filter(_, body) => body
        | _ => Alcotest.fail("expected a filter expression")
        };
      switch (
        Statics.Map.ty_of(Exp.rep_id(exp), m),
        Statics.Map.ty_of(Exp.rep_id(body), m),
      ) {
      | (Some(filter_ty), Some(body_ty)) =>
        check(
          testable_typ,
          src ++ " — filter type equals body type",
          body_ty,
          filter_ty,
        )
      | _ => Alcotest.fail("missing type info for filter or body")
      };
    },
  );

let elab = src =>
  snd(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      parse_exp(src),
    ),
  );

/* Every filter node in an elaborated term, outermost first. */
let filters_of = (e: Exp.t): list(TermBase.stepper_filter_kind_t) => {
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          switch (Exp.term_of(e)) {
          | Filter(kind, _) => found := [kind, ...found^]
          | _ => ()
          };
          cont(e);
        },
      e,
    );
  List.rev(found^);
};

let show_kind: TermBase.stepper_filter_kind_t => string =
  fun
  | Unresolved(_) => "unresolved"
  | Filter({act, _}) => "resolved " ++ FilterAction.string_of_t(act)
  | Residue(_) => "residue";

/* Statics resolves `debug act(pat) in body` into a resolved filter carrying
   the action; anything else stays unresolved. Checks every filter in the
   elaborated term, outermost first. */
let filters_resolve_to = (name, src, expected) =>
  test_case(name, `Quick, () =>
    check(
      list(string),
      src,
      expected,
      elab(src) |> filters_of |> List.map(show_kind),
    )
  );

let no_static_errors = (name, src) =>
  test_case(name, `Quick, () =>
    check(
      int,
      src ++ " — no static errors",
      0,
      parse_exp(src) |> statics |> errors |> List.length,
    )
  );

let tests = (
  "Statics.Filter",
  [
    info_map_preserves_ids("hide", "debug hide(1) in 2"),
    info_map_preserves_ids("eval", "debug eval(1) in 2"),
    info_map_preserves_ids("step", "debug step(1) in 2"),
    info_map_preserves_ids("stop", "debug stop(1) in 2"),
    info_map_preserves_ids(
      "hide with filter-selector pattern",
      "debug hide($e) in 1 + 2",
    ),
    filters_resolve_to(
      "stop resolves",
      "debug stop($e) in 1",
      ["resolved stop"],
    ),
    filters_resolve_to(
      "step resolves",
      "debug step($e) in 1",
      ["resolved step"],
    ),
    filters_resolve_to(
      "hide resolves",
      "debug hide($e) in 1",
      ["resolved hide"],
    ),
    filters_resolve_to(
      "eval resolves",
      "debug eval($e) in 1",
      ["resolved eval"],
    ),
    filters_resolve_to(
      "nested filters all resolve, outermost first",
      "debug eval($e) in debug step($e * $e) in debug stop($e + $e) in 1",
      ["resolved eval", "resolved step", "resolved stop"],
    ),
    filters_resolve_to(
      "a filter in a subexpression resolves",
      "(2 + 3) + (debug hide($e) in 2 + 3)",
      ["resolved hide"],
    ),
    filters_resolve_to(
      "a filter under a binding resolves",
      "let f = fun n -> n + 1 in debug stop(f($v)) in f(1)",
      ["resolved stop"],
    ),
    filters_resolve_to(
      "a non-action head stays unresolved",
      "debug 1 in 2",
      ["unresolved"],
    ),
    filters_resolve_to(
      "an ordinary function head stays unresolved",
      "let f = fun x -> x in debug f(1) in 2",
      ["unresolved"],
    ),
    test_case("resolved filter keeps the elaborated pattern", `Quick, () =>
      switch (elab("debug stop($e + 1) in 1 + 1") |> filters_of) {
      | [Filter({pat, _})] =>
        switch (Exp.term_of(pat)) {
        | BinOp(Int(Plus), lhs, rhs) =>
          check(
            bool,
            "pattern is $e + 1",
            true,
            Exp.term_of(lhs) == FilterSelector(FilterSelector.Exp)
            && Exp.term_of(rhs) == Atom(Int(Bigint.of_int(1))),
          )
        | _ => Alcotest.fail("expected the pattern $e + 1")
        }
      | _ => Alcotest.fail("expected one resolved filter")
      }
    ),
    no_static_errors(
      "the #2331 program has no static errors",
      "debug eval($e) in let fac : Int -> Int = fun n -> if n < 2 then 2 else n * fac(n - 1) in debug stop(fac($v)) in fac(3)",
    ),
    no_static_errors(
      "selectors in arithmetic patterns have no static errors",
      "debug stop($e + $v) in debug hide($v * $e) in (1 + 2) * 3",
    ),
    filter_preserves_type(
      "unresolved filter preserves the body type",
      "debug 1 in true",
    ),
    filter_preserves_type("filter preserves Int", "debug hide(1) in 1 + 1"),
    filter_preserves_type(
      "filter preserves Bool",
      "debug eval($e) in true && false",
    ),
    filter_preserves_type(
      "nested filters preserve the body type",
      "debug hide(1) in debug stop(2) in 3",
    ),
  ],
);
