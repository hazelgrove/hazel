open Alcotest;
open Haz3lcore;
open Language;

/* a jump to a tile outside every open cell opens the item holding it */

let src = "module T = {\n  let a = 10q;\n  let b = 2\n};\n0";

let jump = () => {
  let seg = CorpusUtil.seg_of(~root=Mod, src);
  let term = MakeTerm.Incr.term_of_mod(seg);
  let outline = CorpusUtil.outline_of(~root=Mod, seg);
  let ds = DefStatics.calc(~settings=CoreSettings.on, term);
  let row = CorpusUtil.row_by_label(CorpusUtil.outline_of(~root=Mod, seg));
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
    switch (
      Web.Divided.open_(
        ~info_map=ds.merged,
        ~outline,
        row("b"),
        Web.Divided.of_whole(editor),
      )
    ) {
    | Some(d) => d
    | None => fail("split")
    };
  switch (Web.ScratchMode.Selection.cross_cell_target(~target_id=bad, ~d)) {
  | Some((Workspace(FocusEnsure(fid)), _, _)) =>
    check(bool, "opens a", true, fid == row("a"));
    check(
      bool,
      "a opens",
      true,
      Web.Divided.open_(~info_map=ds.merged, ~outline, row("a"), d) != None,
    );
  | Some(_) => fail("unexpected action")
  | None => fail("no target")
  };
};

let tests = (
  "ClosedJump",
  [test_case("opens the holding item", `Quick, jump)],
);
