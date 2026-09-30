open Alcotest;
open Haz3lcore;
open Language;

/* A jump to a tile outside every open cell opens the item holding it
   (problems panel, inspector, agent results).
   Run: bash test/run_node.sh test 'ClosedJump' */

let src = "module T = {\n  let a = 10q;\n  let b = 2\n};\n0";

let jump = () => {
  let seg =
    switch (FastParse.of_text(~root=Mod, src)) {
    | Some(seg) => seg
    | None =>
      switch (MarkerParse.of_text(~root=Mod, src)) {
      | Some(z) => Zipper.unselect_and_zip(z)
      | None => fail("parse")
      }
    };
  let term = MakeTerm.Incr.term_of_mod(seg);
  let ds = DefStatics.calc(~settings=CoreSettings.on, term);
  let row = label => {
    let rec find = (ns: list(Web.OutlineTree.node)) =>
      List.fold_left(
        (acc, n: Web.OutlineTree.node) =>
          switch (acc) {
          | Some(_) => acc
          | None => n.o_label == label ? n.o_id : find(n.o_children)
          },
        None,
        ns,
      );
    Option.get(find(Web.OutlineTree.of_term(term)));
  };
  let bad =
    switch (
      List.find_opt(
        id =>
          switch (Id.Map.find_opt(id, ds.merged)) {
          | Some(info) => Info.is_error(info)
          | None => false
          },
        DefStatics.all_error_ids(ds),
      )
    ) {
    | Some(id) => id
    | None => fail("no error")
    };
  let editor = Web.ScratchFocus.cell_of_seg(~root=Mod, seg);
  let editor = {
    ...editor,
    editor: {
      ...editor.editor,
      statics: {
        ...editor.editor.statics,
        term,
        info_map: ds.merged,
      },
    },
  };
  let d =
    switch (Web.Divided.split(~info_map=ds.merged, editor, row("b"))) {
    | Some(d) => d
    | None => fail("split")
    };
  switch (Web.ScratchMode.Selection.cross_cell_target(~target_id=bad, ~d)) {
  | Some((FocusEnsure(fid), _, _)) =>
    check(bool, "opens a", true, fid == row("a"));
    check(
      bool,
      "a opens",
      true,
      Web.Divided.open_(~info_map=ds.merged, ~term, row("a"), d) != None,
    );
  | Some(_) => fail("unexpected action")
  | None => fail("no target")
  };
};

let tests = (
  "ClosedJump",
  [test_case("opens the holding item", `Quick, jump)],
);
