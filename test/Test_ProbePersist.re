open Alcotest;
open Haz3lcore;
module Persist = Web.ScratchPersist;
module Scratchpad = Web.ScratchModel.Scratchpad;

/* Manual probes live in the zipper, not the segment, so per-item
   autosave stores them under their own key. */

let probe: Refractors.entry =
  Refractors.mk_entry(Language.ProjectorKind.Probe);

let seg_of = (src: string): Segment.t =>
  switch (FastParse.of_text(~root=Exp, src)) {
  | Some(seg) => seg
  | None => fail("parse failed: " ++ src)
  };

let key_roundtrip = () => {
  let seg = seg_of("let x = 1 in\nx + 2");
  let probes = [(List.hd(Segment.ids(seg)), probe)];
  Persist.write_probes("probetest", "keys", probes);
  check(
    bool,
    "read back while the anchor exists",
    true,
    Persist.read_probes("probetest", "keys", seg) == probes,
  );
  check(
    int,
    "dropped once the anchor is gone",
    0,
    List.length(Persist.read_probes("probetest", "keys", seg_of("1"))),
  );
};

let save_then_load = () => {
  let settings = Language.CoreSettings.on;
  let names =
    List.map(fst, snd(Lazy.force(Web.Init.startup).documentation));
  let m =
    Persist.load_all(
      "probetest",
      ~settings,
      ~default_names=names,
      ~default_current=0,
    );
  let sp = List.nth(m.scratchpads, m.current);
  switch (sp.kind) {
  | Drv(_) => fail("expected a code slide")
  | Code({editor, agent}) =>
    let z = editor.editor.editor.state.zipper;
    let anchor = List.hd(Segment.ids(Zipper.unselect_and_zip(z)));
    let z =
      ZipperBase.update_refractors(z, r =>
        Refractors.{
          ...r,
          manuals: [(anchor, probe)],
        }
      );
    let editor =
      Web.CellEditor.Model.mk(
        Editor.Model.mk(z, ~root=editor.editor.editor.root),
      );
    let sp = {
      ...sp,
      kind:
        Code({
          editor,
          agent,
        }),
    };
    Persist.save_current(
      "probetest",
      {
        ...m,
        scratchpads: Util.ListUtil.put_nth(m.current, sp, m.scratchpads),
      },
    );
    switch (Persist.load_scratchpad(~settings, "probetest", sp.name).kind) {
    | Code({editor, _}) =>
      check(
        bool,
        "the probe comes back on the per-item load",
        true,
        List.mem_assoc(
          anchor,
          editor.editor.editor.state.zipper.refractors.manuals,
        ),
      )
    | Drv(_) => fail("loaded a derivation slide")
    };
  };
};

let tests = (
  "ProbePersist",
  [
    test_case("probes key round trip", `Quick, key_roundtrip),
    test_case("save then load keeps probes", `Quick, save_then_load),
  ],
);
