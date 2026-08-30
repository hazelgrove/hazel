/* Term derivation must be DETERMINISTIC given the segment: W2 ships
   segments and derives statics on BOTH sides, so any Id.mk() minted
   during derivation (MultiHole wraps, singleton-tuple wraps, ...)
   forks the two sides' summaries. Derive each shipped slide twice
   (physically distinct but equal segments, forcing memo misses) and
   demand identical per-item error/warning id sets. */

open Alcotest;
open Haz3lcore;
open Language;

let settings = CoreSettings.on;

/* physical deep copy: forces every derivation memo to miss */
let copy_segment = (seg: Segment.t): Segment.t =>
  Marshal.from_bytes(Marshal.to_bytes(seg, []), 0);

let derive = (~root, seg) => {
  let term =
    switch (root) {
    | Sort.Exp => MakeTerm.go(seg).term
    | _ => MakeTerm.go_mod_root(seg).term
    };
  DefStatics.calc(~settings, term);
};
let summary = (~seg, ds) =>
  ResidentProgram.Summary.of_def_statics(
    ~generation=0,
    ~piece_ids=ResidentProgram.piece_ids(seg),
    ds,
  );

let slide_case = ((name, root, p: PersistentZipper.t)) =>
  test_case(
    name,
    `Slow,
    () => {
      let text = p.backup_text;
      switch (CorpusUtil.parse(~root, ParsedCorpus.normalize(text))) {
      | None => () /* fast-parse gap: nothing shipped, nothing derived */
      | Some(seg) =>
        let seg' = copy_segment(seg);
        let ds_a = derive(~root, seg);
        let ds_b = derive(~root, seg');
        let a = summary(~seg, ds_a);
        let b = summary(~seg=seg', ds_b);
        /* the copy's PIECE ids are identical (marshal preserves them);
           only derivation-minted ids can differ */
        if (!ResidentProgram.Summary.equal(a, b)) {
          /* diagnostic: for each differing item, print the ids present
             on only one side, with their Info classes */
          let imap_a = ds_a.DefStatics.merged;
          let imap_b = ds_b.DefStatics.merged;
          List.combine(a.s_items, b.s_items)
          |> List.iteri((i, (x: ResidentProgram.Summary.item_summary, y: ResidentProgram.Summary.item_summary)) =>
               if (x != y) {
                 let only = (l1, l2) =>
                   List.filter(id => !List.mem(id, l2), l1);
                 let show = (imap, id) =>
                   Id.to_string(id)
                   ++ "("
                   ++ (
                     switch (Id.Map.find_opt(id, imap)) {
                     | Some(info) => Info.show(info) |> String.sub(_, 0, 120)
                     | None => "?"
                     }
                   )
                   ++ ")";
                 Printf.printf(
                   "ITEM %d errs-only-a: %s | errs-only-b: %s\n",
                   i,
                   String.concat(
                     ", ",
                     List.map(show(imap_a), only(x.s_errors, y.s_errors)),
                   ),
                   String.concat(
                     ", ",
                     List.map(show(imap_b), only(y.s_errors, x.s_errors)),
                   ),
                 );
               }
             );
        };
        check(
          bool,
          name ++ ": derivation is deterministic",
          true,
          ResidentProgram.Summary.equal(a, b),
        );
      };
    },
  );

let tests = [
  (
    "DeriveDeterminism",
    Web.Init.documentation_slides
    |> List.filter(((name, _, _)) => !CorpusUtil.mega_scale(name))
    |> List.map(slide_case),
  ),
];
