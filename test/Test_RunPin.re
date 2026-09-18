open Alcotest;
open Haz3lcore;
open Language;
module OutlineTree = Web.OutlineTree;
module Focus = Web.ScratchFocus;

/* Tests-container pinning opens ONE run cell at every block depth
   (andrew's report: module containers pinned each test individually
   while top-level containers opened one cell). Gates: the run entry
   spans all container members, top-level and module alike, and an
   unedited run cell splices home exactly. */

let settings = CoreSettings.on;

let seg_of_text = (text: string): Segment.t =>
  switch (MarkerParse.of_text(~root=Sort.Exp, text)) {
  | Some(z) => Zipper.unselect_and_zip(~erase_buffer=true, z)
  | None => failwith("parse failed: " ++ text)
  };

let seg_equal = (a: Segment.t, b: Segment.t): bool =>
  Sexplib.Sexp.compare(Segment.sexp_of_t(a), Segment.sexp_of_t(b)) == 0;

let src = "let m = {\ntest 1 == 1 end;\ntest 2 == 2 end\n} in\nlet a = 1 in\ntest 3 == 3 end;\ntest 4 == 4 end;\na";

/* the KTests containers of a node list, shallow */
let containers = (ns: list(OutlineTree.node)): list(OutlineTree.node) =>
  List.filter((n: OutlineTree.node) => n.o_kind == KTests, ns);

let kid_ids = (n: OutlineTree.node): list(Id.t) =>
  List.filter_map((c: OutlineTree.node) => c.o_id, n.o_children);

let check_run_pin =
    (label: string, seg: Segment.t, container: OutlineTree.node): unit => {
  let term = MakeTerm.go(seg).term;
  let info_map = DefStatics.calc(~settings, term).merged;
  let ids = kid_ids(container);
  check(bool, label ++ ": container has kids", true, List.length(ids) >= 2);
  let first =
    switch (ids) {
    | [first, ..._] => first
    | [] => failwith("no kids")
    };
  switch (Focus.mk_run_entry(~info_map, first, seg)) {
  | None => fail(label ++ ": mk_run_entry returned None")
  | Some(e) =>
    check(bool, label ++ ": entry is a run cell", true, e.e_run);
    /* members carry BOTH id domains (statement `;` reps and test tile
       reps) — the outline's kid ids must all be covered */
    check(
      bool,
      label ++ ": members cover the container's kids",
      true,
      List.for_all(id => List.mem(id, e.e_members), ids),
    );
    /* unedited round trip: splicing the untouched cell home must
       reproduce the master exactly */
    check(
      bool,
      label ++ ": unedited splice is exact",
      true,
      seg_equal(seg, Focus.splice_entry(e, seg)),
    );
    /* ctx recapture path used on Force frames */
    check(
      bool,
      label ++ ": cell_content resolves",
      true,
      Focus.cell_content(e, seg) != None,
    );
  };
};

let cases = [
  test_case(
    "top-level tests container pins one run cell",
    `Quick,
    () => {
      let seg = seg_of_text(src);
      let term = MakeTerm.go(seg).term;
      switch (containers(OutlineTree.of_term(term))) {
      | [c] => check_run_pin("top", seg, c)
      | cs =>
        fail(
          Printf.sprintf(
            "expected 1 top container, got %d",
            List.length(cs),
          ),
        )
      };
    },
  ),
  test_case(
    "module tests container pins one run cell (parity with top level)",
    `Quick,
    () => {
      let seg = seg_of_text(src);
      let term = MakeTerm.go(seg).term;
      let m =
        switch (
          List.find_opt(
            (n: OutlineTree.node) => n.o_label == "m",
            OutlineTree.of_term(term),
          )
        ) {
        | Some(m) => m
        | None => fail("no module node")
        };
      switch (containers(m.o_children)) {
      | [c] => check_run_pin("module", seg, c)
      | cs =>
        fail(
          Printf.sprintf(
            "expected 1 module container, got %d",
            List.length(cs),
          ),
        )
      };
    },
  ),
];

let tests = [("RunPin", cases)];
