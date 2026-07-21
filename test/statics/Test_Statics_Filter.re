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
