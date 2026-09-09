/* W2b main-thread mode (DefStatics ~propagate=false): a rename edit
   re-analyzes ONLY the edited item; downstream keeps stale results
   (the worker supplies fresh ones async), but downstream ctx chains
   re-thread the new exports so a subsequent analysis of any
   downstream item sees fresh upstream bindings. */

open Alcotest;
open Haz3lcore;
open Language;

let settings = CoreSettings.on;

let src = "let aaa = 1 in
let bbb = aaa + 1 in
let ccc = bbb + aaa in
ccc";

/* rename aaa -> zzz: downstream uses of aaa become unbound */
let src' = "let zzz = 1 in
let bbb = aaa + 1 in
let ccc = bbb + aaa in
ccc";

let parse = src =>
  switch (CorpusUtil.parse(~root=Sort.Exp, src)) {
  | Some(seg) => seg
  | None => Alcotest.fail("parse failed")
  };

let errs = (ds: DefStatics.t) =>
  ds.items |> List.map((it: DefStatics.item) => List.length(it.d_error_ids));

let tests = [
  (
    "PropagateClamp",
    [
      test_case(
        "rename: clamped analyzes 1 item, keeps stale",
        `Quick,
        () => {
          let seg = parse(src);
          let t0 = DefStatics.calc(~settings, MakeTerm.go(seg).term);
          check(int, "clean baseline", 0, List.fold_left((+), 0, errs(t0)));
          /* production-shaped edit: splice ONLY the renamed item's fresh
             slice; downstream items keep their piece identities */
          let spliced =
            switch (
              MakeTerm.Incr.slices(seg),
              MakeTerm.Incr.slices(parse(src')),
            ) {
            | ([_, ...old_rest], [fresh_first, ..._]) =>
              List.concat([fresh_first, ...old_rest])
            | _ => Alcotest.fail("slice shapes")
            };
          let term' = MakeTerm.go(spliced).term;
          let clamped =
            DefStatics.calc(~settings, ~propagate=false, ~prev=t0, term');
          check(
            int,
            "only the edited item analyzed",
            1,
            DefStatics.last_analyzed^,
          );
          check(
            list(int),
            "downstream errors stay stale (none)",
            [0, 0, 0, 0],
            errs(clamped),
          );
          let full = DefStatics.calc(~settings, ~prev=t0, term');
          check(
            bool,
            "full propagation sees the new unbound errors",
            true,
            List.fold_left((+), 0, errs(full)) > 0,
          );
          /* a clamped chain CANNOT be caught up by a later incremental
             calc — its recorded exports already match, so nothing seeds
             dirt. That is by design (the worker's summaries are the
             reconciliation path), and it means TOGGLE-OFF MUST BUST THE
             SLOT: a cold calc from the clamped state's term recovers
             full correctness. */
          let incremental_misses =
            DefStatics.calc(~settings, ~prev=clamped, term');
          check(
            list(int),
            "incremental-from-clamped stays stale (by design)",
            [0, 0, 0, 0],
            errs(incremental_misses),
          );
          let cold = DefStatics.calc(~settings, term');
          check(
            list(int),
            "cold calc from the same term recovers full errors",
            errs(full),
            errs(cold),
          );
        },
      ),
    ],
  ),
];
