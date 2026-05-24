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

let collect_ids = (exp: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let collect = (a: IdTagged.IdTag.t) => {
    acc := a.ids @ acc^;
    a;
  };
  let _ = Grammar.map_exp_annotation(collect, exp);
  acc^;
};

let info_map_preserves_ids = (name, src) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(src);
      let m = statics(exp);
      let missing =
        collect_ids(exp)
        |> List.filter(id =>
             !Id.equal(id, Id.invalid)
             && Option.is_none(Statics.Map.lookup(id, m))
           );
      Alcotest.(check(list(string)))(
        src ++ " — every surface id appears in the info map",
        [],
        List.map(Id.show, missing),
      );
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
  ],
);
