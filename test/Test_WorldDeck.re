open Alcotest;
open Haz3lcore;
module Deck = Web.WorldDeck;
module Model = Web.ScratchModel.Model;

/* the world tree's deck: every slide in one program, the current one
   loaded, switching carrying the program, saves per slide (WorldDeck) */

let settings = Language.CoreSettings.on;

let current = (m: Model.t) =>
  switch (List.nth(m.scratchpads, m.current)) {
  | {name, kind: Code(code), dormant: false} => (name, code)
  | _ => fail("the current slide isn't live code")
  };

let index = (m: Model.t, name) =>
  switch (
    Util.ListUtil.findi_opt(
      (sp: Web.ScratchModel.Scratchpad.t) => sp.name == name,
      m.scratchpads,
    )
  ) {
  | Some((i, _)) => i
  | None => fail("no slide " ++ name)
  };

let body = (code: Web.ScratchModel.Scratchpad.code, name) =>
  switch (
    WorldTree.body(
      ~slide=Deck.slide_id(name),
      Web.Program.document(code.program),
    )
  ) {
  | Loaded(_) => "loaded"
  | Unloaded => "unloaded"
  | Gone => "gone"
  };

let tables = "Documentation / Tables";

let deck = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    let m = Deck.load(~settings);
    let (name, code) = current(m);
    check(
      bool,
      "Documentation's and Scratch's slides",
      true,
      List.mem(tables, Model.scratchpad_names(m))
      && List.mem("Scratch / Scratchpad 1", Model.scratchpad_names(m)),
    );
    check(string, "the current slide, loaded", "loaded", body(code, name));
    check(string, "another, not", "unloaded", body(code, tables));
    check(
      bool,
      "its module is the program's world",
      true,
      code.world == Some(Deck.slide_id(name)),
    );
    let m =
      Deck.hydrate_current(
        ~settings,
        {
          ...m,
          current: index(m, tables),
        },
      );
    let (name', code') = current(m);
    check(string, "switched to", tables, name');
    check(string, "loaded on the way", "loaded", body(code', tables));
    check(string, "the first still loaded", "loaded", body(code', name));
    check(
      bool,
      "the first slide holds no program",
      true,
      List.nth(m.scratchpads, index(m, name)).dormant,
    );
    Deck.save(m);
    check(
      bool,
      "stored under the world's key",
      true,
      Web.ScratchPersist.stored(Deck.prefix, tables) != None,
    );
    let (name'', _) = current(Deck.load(~settings));
    check(string, "reloads at the slide it was on", tables, name'');
  });

let add_delete = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    let m = Deck.add_scratch(~settings, Deck.load(~settings));
    let (name, code) = current(m);
    check(
      string,
      "a new Scratch slide, current",
      "Scratch / Scratchpad 2",
      name,
    );
    check(string, "loaded, empty", "loaded", body(code, name));
    check(
      bool,
      "in Scratch's deck too",
      true,
      List.mem("Scratchpad 2", Deck.deck_names("scratch")),
    );
    let m = Deck.delete_current(~settings, m);
    let (name', code') = current(m);
    check(
      string,
      "the slide before it, current",
      "Scratch / Scratchpad 1",
      name',
    );
    check(string, "gone from the program", "gone", body(code', name));
    check(
      bool,
      "and from Scratch's deck",
      false,
      List.mem("Scratchpad 2", Deck.deck_names("scratch")),
    );
  });

let rename = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    let m = Deck.add_scratch(~settings, Deck.load(~settings));
    let m = Deck.rename_current(~settings, "Scratch / Notes", m);
    let (name, code) = current(m);
    check(string, "renamed, current", "Scratch / Notes", name);
    check(string, "its body there", "loaded", body(code, name));
    check(
      string,
      "nothing left at the old name",
      "gone",
      body(code, "Scratch / Scratchpad 2"),
    );
    check(
      list(string),
      "Scratch's deck",
      ["Scratchpad 1", "Notes"],
      Deck.deck_names("scratch"),
    );
  });

/* zoomed into another slide's module, that slide is current, loaded */
let zoom_follows = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    let m = Deck.load(~settings);
    let (_, code) = current(m);
    let m =
      Model.with_current_kind(
        Code({
          ...code,
          view: {
            ...code.view,
            zoom: [
              WorldTree.folder_id(["Documentation"]),
              Deck.slide_id(tables),
            ],
          },
        }),
        m,
      );
    let (name, code) = current(Deck.follow_zoom(~settings, m));
    check(string, "Tables, current", tables, name);
    check(string, "and loaded", "loaded", body(code, tables));
  });

/* the fold right after the tile [id] undone, at whatever depth */
let rec unfold_after = (~id, seg: Segment.t): Segment.t => {
  let rec level = (ps: Segment.t, armed) =>
    switch (ps) {
    | [] => []
    | [Piece.Tile(t) as p, ...rest] when t.id == id => [
        p,
        ...level(rest, true),
      ]
    | [Piece.Projector(pr), ...rest] when armed => [pr.syntax, ...rest]
    | [Piece.Secondary(_) as p, ...rest] => [p, ...level(rest, armed)]
    | [Piece.Tile(t), ...rest] => [
        Piece.Tile({
          ...t,
          children: List.map(unfold_after(~id), t.children),
        }),
        ...level(rest, false),
      ]
    | [p, ...rest] => [p, ...level(rest, false)]
    };
  level(seg, false);
};

/* a slide's `⋯` opened in the code: it loads where it is */
let unfold_loads = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    let web = Web.Settings.Model.init;
    let m = Deck.load(~settings);
    let (name, code) = current(m);
    let slide = Deck.slide_id(tables);
    let doc = Web.Program.document(code.program);
    check(bool, "Tables folded", false, WorldTree.opened(~slide, doc));
    let doc = unfold_after(~id=slide, doc);
    check(bool, "Tables opened", true, WorldTree.opened(~slide, doc));
    let m =
      Deck.load_unfolded(
        ~settings=web,
        Model.with_current_kind(
          Code(Web.Workspace.with_segment(~settings=web, code, doc)),
          m,
        ),
      );
    let (name', code) = current(m);
    check(string, "still on its slide", name, name');
    check(string, "Tables loaded", "loaded", body(code, tables));
    check(
      string,
      "with its members",
      Printer.of_segment(~holes="?", fst(Deck.members(tables))),
      switch (WorldTree.body(~slide, Web.Program.document(code.program))) {
      | Loaded(members) => Printer.of_segment(~holes="?", members)
      | _ => "?"
      },
    );
  });

/* the unloaded slides summarized one a step: all but the perf corpora,
   errors where a slide shows them, their rows marked in the outline */
let summaries = () =>
  Test_WorldSlide.shipped(() => {
    Web.HazelDB.kv_clear();
    Hashtbl.reset(Web.WorldSummaries.table);
    let m = Deck.load(~settings);
    let (_, code) = current(m);
    let doc = Web.Program.document(code.program);
    let rec go = n =>
      if (n > 0 && Web.WorldSummaries.missing(doc) != []) {
        ignore(Web.WorldSummaries.step(~settings, doc));
        go(n - 1);
      } else {
        n;
      };
    let left = go(200);
    check(bool, "done in bounded steps", true, left > 0);
    check(
      list(string),
      "none missing",
      [],
      Web.WorldSummaries.missing(doc),
    );
    let errs = "Documentation / B2T2 / Errors / Malformed Tables";
    check(
      bool,
      "a slide showing errors has some",
      true,
      switch (Web.WorldSummaries.find(errs)) {
      | Some(s) => s.errors > 0
      | None => false
      },
    );
    check(
      bool,
      "the perf corpora skipped",
      true,
      Web.WorldSummaries.find("Documentation / Perf / Mega 4k") == None,
    );
    let rows = Web.WorldSummaries.error_rows();
    check(
      bool,
      "its row and folders marked",
      true,
      List.mem(Deck.slide_id(errs), rows)
      && List.mem(WorldTree.folder_id(["Documentation", "B2T2"]), rows),
    );
  });

let tests = (
  "WorldDeck",
  [
    test_case("load, switch, save, reload", `Quick, deck),
    test_case("adding and deleting a slide", `Quick, add_delete),
    test_case("renaming a slide", `Quick, rename),
    test_case("zooming into a slide follows it", `Quick, zoom_follows),
    test_case("opening a slide's ⋯ loads it", `Quick, unfold_loads),
    test_case("unloaded slides summarized", `Slow, summaries),
  ],
);
