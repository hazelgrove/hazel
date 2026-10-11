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
  Persist.write_side("probetest", "keys", side =>
    {
      ...side,
      probes,
    }
  );
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

/* a slide saved under the keys :side replaced loads them, and its first
   write folds them into :side */
let legacy_side_keys = () => {
  let seg = seg_of("let x = 1 in\nx + 2");
  let probes = [(List.hd(Segment.ids(seg)), probe)];
  let ck = Persist.content_key("legacytest", "s");
  let x =
    Web.OutlineTree.[
      {
        s_label: "x",
        s_occ: 0,
      },
    ];
  let ab_y =
    Web.OutlineTree.[
      {
        s_label: "a b",
        s_occ: 2,
      },
      {
        s_label: "y",
        s_occ: 1,
      },
    ];
  List.iter(
    ((suffix, v)) => Web.HazelDB.kv_save(ck ++ suffix, v),
    [
      (":caret", "3 5"),
      (
        ":pins",
        {|(((pin_path(((s_label x)(s_occ 0))))(pin_run true))((pin_path(((s_label"a b")(s_occ 2))((s_label y)(s_occ 1))))(pin_run false)))|},
      ),
      (
        ":view",
        {|((vf_zoom((((s_label"a b")(s_occ 2))((s_label y)(s_occ 1)))))(vf_parked true))|},
      ),
      (
        ":collapse",
        {|((((s_label x)(s_occ 0)))(((s_label"a b")(s_occ 2))((s_label y)(s_occ 1))))|},
      ),
      (
        ":probes",
        Sexplib.Sexp.to_string(Refractors.RefractorList.sexp_of_t(probes)),
      ),
    ],
  );
  let expected =
    Persist.{
      caret: Some(Util.Point.mk(~row=3, ~col=5)),
      pins: [(x, true), (ab_y, false)],
      zoom: Some(ab_y),
      parked: true,
      collapse: [x, ab_y],
      probes,
    };
  let read = () => Persist.read_side("legacytest", "s");
  check(bool, "the old keys load", true, read() == expected);
  Persist.write_side("legacytest", "s", side => side);
  check(
    bool,
    "a write drops them",
    true,
    List.for_all(
      sfx => Web.HazelDB.kv_get(ck ++ sfx) == None,
      Persist.legacy_suffixes,
    ),
  );
  check(bool, "and keeps what they held", true, read() == expected);
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
  | Code({program, agent, _}) =>
    let editor = Web.Program.whole(program);
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
          program: Whole(editor),
          view: Web.SlideView.init,
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
    | Code({program, _}) =>
      let editor = Web.Program.whole(program);
      check(
        bool,
        "the probe comes back on the per-item load",
        true,
        List.mem_assoc(
          anchor,
          editor.editor.editor.state.zipper.refractors.manuals,
        ),
      );
    | Drv(_) => fail("loaded a derivation slide")
    };
  };
};

/* a bad roster falls back to the text copy, which re-mints ids: the
   stored slices can never be read again, so the load drops them */
let bad_roster_drops_items = () => {
  let settings = Language.CoreSettings.on;
  let names =
    List.map(fst, snd(Lazy.force(Web.Init.startup).documentation));
  let m =
    Persist.load_all(
      "rostertest",
      ~settings,
      ~default_names=names,
      ~default_current=0,
    );
  Persist.save_current("rostertest", m);
  let name = List.nth(m.scratchpads, m.current).name;
  let ns = Persist.items_ns("rostertest", name);
  let items = () =>
    Util.Maps.StringMap.bindings(Web.HazelDB.cache^)
    |> List.filter(((k, _)) => String.starts_with(~prefix=ns, k));
  check(bool, "the save wrote items", true, items() != []);
  Web.HazelDB.kv_save(ns ++ ItemPersist.roster_key, "(not a roster");
  let _ = Persist.load_scratchpad(~settings, "rostertest", name);
  check(int, "no item keys left", 0, List.length(items()));
};

/* probe passes rebuild the zipper on every calculate; an idle autosave
   still writes nothing */
let idle_save_writes_nothing = () => {
  let settings = Language.CoreSettings.on;
  let names =
    List.map(fst, snd(Lazy.force(Web.Init.startup).documentation));
  let m =
    Persist.load_all(
      "idletest",
      ~settings,
      ~default_names=names,
      ~default_current=0,
    );
  let sp = List.nth(m.scratchpads, m.current);
  switch (sp.kind) {
  | Drv(_) => fail("expected a code slide")
  | Code({program, agent, view}) =>
    let recalc = (e: Web.CellEditor.Model.t) =>
      Web.CellEditor.Update.calculate(
        ~settings,
        ~is_edited=false,
        ~statics_mode=Force,
        ~queue_worker=None,
        ~stitch=x => x,
        e,
      );
    let e1 = recalc(Web.Program.whole(program));
    let e2 = recalc(e1);
    let zip = (e: Web.CellEditor.Model.t) => e.editor.editor.state.zipper;
    check(
      bool,
      "a recalculate keeps the content",
      true,
      Zipper.same_content(zip(e1), zip(e2)),
    );
    let with_editor = e => {
      ...m,
      scratchpads:
        Util.ListUtil.put_nth(
          m.current,
          {
            ...sp,
            kind:
              Code({
                program: Whole(e),
                view,
                agent,
              }),
          },
          m.scratchpads,
        ),
    };
    let caret = Persist.side_key("idletest", sp.name);
    Persist.save_current("idletest", with_editor(e1));
    check(
      bool,
      "the first save wrote the caret",
      true,
      Web.HazelDB.kv_get(caret) != None,
    );
    Web.HazelDB.kv_remove(caret);
    Persist.save_current("idletest", with_editor(e2));
    check(
      bool,
      "the idle save skipped",
      true,
      Web.HazelDB.kv_get(caret) == None,
    );
  };
};

/* a save, and a rename, each land in one storage transaction */
let one_transaction = () => {
  let settings = Language.CoreSettings.on;
  let names =
    List.map(fst, snd(Lazy.force(Web.Init.startup).documentation));
  let m =
    Persist.load_all(
      "txtest",
      ~settings,
      ~default_names=names,
      ~default_current=0,
    );
  let before = Web.HazelDB.write_transactions^;
  Persist.save_current("txtest", m);
  check(int, "a save", 1, Web.HazelDB.write_transactions^ - before);
  let name = List.nth(m.scratchpads, m.current).name;
  let before = Web.HazelDB.write_transactions^;
  Persist.rename_slide("txtest", name, name ++ " renamed");
  check(int, "a rename", 1, Web.HazelDB.write_transactions^ - before);
};

let tests = (
  "ProbePersist",
  [
    test_case("probes key round trip", `Quick, key_roundtrip),
    test_case("the keys :side replaced still load", `Quick, legacy_side_keys),
    test_case("save then load keeps probes", `Quick, save_then_load),
    test_case(
      "a bad roster drops the item keys",
      `Quick,
      bad_roster_drops_items,
    ),
    test_case(
      "an idle save writes nothing",
      `Quick,
      idle_save_writes_nothing,
    ),
    test_case("a save is one transaction", `Quick, one_transaction),
  ],
);
