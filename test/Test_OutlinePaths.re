open Alcotest;
open Haz3lcore;
open Language;
module OutlineTree = Web.OutlineTree;
module ScratchPersist = Web.ScratchPersist;

/* Occurrence-qualified outline paths (codex review, PR #2469): label
   paths alone are not unique — duplicate definition names and
   separated `tests` groups both produce same-labeled rows — and the
   old first-match resolution crossed wires between them. The gates:
   every outline row's label_path resolves back to ITS OWN id, and
   the pins/collapse sexp codecs round-trip labels containing the old
   encoding's delimiter characters. */

let term_of_text = (text: string): Exp.t => {
  let z =
    switch (MarkerParse.of_text(~root=Sort.Exp, text)) {
    | Some(z) => z
    | None => failwith("parse failed: " ++ text)
    };
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
};

/* all (id, path) pairs of the outline, depth-first */
let rec walk =
        (
          path: OutlineTree.path,
          ns: list((OutlineTree.node, OutlineTree.path_seg)),
        )
        : list((Id.t, OutlineTree.path)) =>
  List.concat_map(
    ((n: OutlineTree.node, seg)) => {
      let here =
        switch (n.o_id) {
        | Some(id) => [(id, path @ [seg])]
        | None => []
        };
      here @ walk(path @ [seg], OutlineTree.segs(n.o_children));
    },
    ns,
  );

let all_rows = (e: Exp.t): list((Id.t, OutlineTree.path)) =>
  walk([], OutlineTree.segs(OutlineTree.of_term(e)));

let check_self_resolution = (label: string, text: string): unit => {
  let e = term_of_text(text);
  let rows = all_rows(e);
  check(bool, label ++ ": outline nonempty", true, rows != []);
  List.iter(
    ((id, _)) =>
      switch (OutlineTree.label_path(id, e)) {
      | None => fail(label ++ ": no label_path for a row id")
      | Some(path) =>
        switch (OutlineTree.resolve_path(path, e)) {
        | Some(id') when id' == id => ()
        | Some(_) => fail(label ++ ": path resolved to a DIFFERENT row")
        | None => fail(label ++ ": path failed to resolve")
        }
      },
    rows,
  );
};

let dup_defs = "let f = 1 in\nlet g = 2 in\nlet f = 3 in\nf + g";

let two_test_groups = "let a = 1 in\ntest 1 == 1 end;\ntest 2 == 2 end;\nlet b = 2 in\ntest 3 == 3 end;\ntest 4 == 4 end;\na + b";

let nested_dups = "let m = module\nlet x = 1 in\nlet x = 2 in\nin\n1";

/* a test row's path is its place among its block's tests: a test added
   in another block leaves it alone */
let test_elsewhere = () => {
  let before = "let a = 1 in\ntest 1 == 1 end;\nlet b = 2 in\ntest 2 == 2 end;\ntest 3 == 3 end;\na + b";
  let after = "let a = 1 in\ntest 1 == 1 end;\ntest 0 == 0 end;\nlet b = 2 in\ntest 2 == 2 end;\ntest 3 == 3 end;\na + b";
  let e0 = term_of_text(before);
  let e1 = term_of_text(after);
  /* the last test of the program: its displayed number moves from 3 to
     4, its path does not */
  let last = (e: Exp.t) => {
    let rec tests = (ns: list(OutlineTree.node)) =>
      List.concat_map(
        (n: OutlineTree.node) =>
          n.o_kind == KTest ? [n] : tests(n.o_children),
        ns,
      );
    switch (List.rev(tests(OutlineTree.of_term(e)))) {
    | [n, ..._] => n.o_id
    | [] => None
    };
  };
  switch (Option.bind(last(e0), id => OutlineTree.label_path(id, e0))) {
  | None => fail("no path")
  | Some(path) =>
    check(
      bool,
      "resolves to the same test",
      true,
      OutlineTree.resolve_path(path, e1) == last(e1),
    )
  };
};

let cases = [
  test_case(
    "a test added elsewhere leaves a test's path",
    `Quick,
    test_elsewhere,
  ),
  test_case(
    "rename moves a slide's keys; delete removes them",
    `Quick,
    () => {
      let db = Web.HazelDB.kv_save;
      db("scratch:Old", "blob");
      db("scratch:Old:pins", "p");
      db("scratch:Older", "other");
      ScratchPersist.rename_slide("scratch", "Old", "New");
      let get = Web.HazelDB.kv_get;
      check(bool, "blob moved", true, get("scratch:New") == Some("blob"));
      check(bool, "pins moved", true, get("scratch:New:pins") == Some("p"));
      check(bool, "old gone", true, get("scratch:Old") == None);
      check(
        bool,
        "neighbour kept",
        true,
        get("scratch:Older") == Some("other"),
      );
      ScratchPersist.forget_slide("scratch", "New");
      check(
        bool,
        "deleted",
        true,
        get("scratch:New") == None && get("scratch:New:pins") == None,
      );
      check(
        bool,
        "neighbour still kept",
        true,
        get("scratch:Older") == Some("other"),
      );
    },
  ),
  test_case(
    "duplicate top-level names: each row round-trips to itself", `Quick, () =>
    check_self_resolution("dup defs", dup_defs)
  ),
  test_case(
    "two separated tests groups: each round-trips to itself", `Quick, () =>
    check_self_resolution("two groups", two_test_groups)
  ),
  test_case("duplicate names inside a module", `Quick, () =>
    check_self_resolution("nested dups", nested_dups)
  ),
  test_case(
    "distinct same-labeled rows get distinct paths",
    `Quick,
    () => {
      let e = term_of_text(two_test_groups);
      let rows = all_rows(e);
      let paths = List.map(snd, rows);
      check(
        int,
        "no two rows share a path",
        List.length(paths),
        List.length(List.sort_uniq(compare, paths)),
      );
    },
  ),
  test_case(
    "pins codec round-trips delimiter-laden labels",
    `Quick,
    () => {
      /* labels are arbitrary program text: the old line format used
         space, '/', and '\n' as unescaped delimiters */
      let gnarly =
        OutlineTree.[
          {
            s_label: "a name with spaces",
            s_occ: 1,
          },
          {
            s_label: "with/slash\nand newline",
            s_occ: 0,
          },
        ];
      let pins = [(gnarly, true), ([], false)];
      let encoded =
        pins
        |> List.map(((pin_path, pin_run)) =>
             ScratchPersist.{
               pin_path,
               pin_run,
             }
           )
        |> ScratchPersist.sexp_of_pins_file
        |> Sexplib.Sexp.to_string;
      let decoded =
        ScratchPersist.pins_file_of_sexp(Sexplib.Sexp.of_string(encoded))
        |> List.map((p: ScratchPersist.pin_rec) => (p.pin_path, p.pin_run));
      check(bool, "round-trip", true, decoded == pins);
    },
  ),
  test_case(
    "collapse codec round-trips",
    `Quick,
    () => {
      let paths: ScratchPersist.collapse_file =
        OutlineTree.[
          [
            {
              s_label: "tests",
              s_occ: 1,
            },
          ],
          [
            {
              s_label: "m",
              s_occ: 0,
            },
            {
              s_label: "x y",
              s_occ: 3,
            },
          ],
        ];
      let encoded =
        paths |> ScratchPersist.sexp_of_collapse_file |> Sexplib.Sexp.to_string;
      let decoded =
        ScratchPersist.collapse_file_of_sexp(
          Sexplib.Sexp.of_string(encoded),
        );
      check(bool, "round-trip", true, decoded == paths);
    },
  ),
];

let tests = [("OutlinePaths", cases)];
