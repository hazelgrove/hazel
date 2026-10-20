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

let tests = (
  "WorldDeck",
  [
    test_case("load, switch, save, reload", `Quick, deck),
    test_case("adding and deleting a slide", `Quick, add_delete),
  ],
);
