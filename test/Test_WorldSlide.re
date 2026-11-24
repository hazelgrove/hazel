open Alcotest;
open Haz3lcore;
open Poly;
module Persist = Web.ScratchPersist;
module Model = Web.ScratchModel.Model;
module LibraryDoc = Web.LibraryDoc;
module WorldSlide = Web.WorldSlide;

/* a slide in the library's world (WorldSlide): opened as the library's
   items then its module, stored as its members */

let settings = Language.CoreSettings.on;

/* [f] with the standard library shipped, as the app ships it at boot */
let shipped = (f: unit => unit): unit => {
  let (shipped, source, running) = (
    LibraryDoc.shipped^,
    LibraryDoc.source^,
    LibraryDoc.running_parse^,
  );
  LibraryDoc.ship(Test_World.library_text());
  Fun.protect(
    ~finally=
      () => {
        LibraryDoc.shipped := shipped;
        LibraryDoc.source := source;
        LibraryDoc.running_parse := running;
      },
    f,
  );
};

let load = (prefix: string): Model.t =>
  Persist.load_all(
    prefix,
    ~settings,
    ~default_names=
      List.map(~f=fst, snd(Lazy.force(Web.Init.startup).documentation)),
    ~default_current=0,
  );

let current = (m: Model.t): (string, Web.ScratchModel.Scratchpad.code) =>
  switch (List.nth_exn(m.scratchpads, m.current)) {
  | {name, kind: Code(code), _} => (name, code)
  | _ => fail("expected a code slide")
  };

let parts = (code: Web.ScratchModel.Scratchpad.code): World.parts =>
  switch (code.world) {
  | Some(slide) =>
    switch (World.split(~slide, Web.Program.document(code.program))) {
    | Some(parts) => parts
    | None => fail("the slide's module is lost")
    }
  | None => fail("not in the library's world")
  };

let text = (seg: Segment.t): string =>
  Printer.of_segment(~holes="?", seg) |> String.strip;

let contains = (s: string, sub: string): bool => {
  let (n, k) = (String.length(s), String.length(sub));
  let rec go = i =>
    i + k <= n && (String.sub(s, ~pos=i, ~len=k) == sub || go(i + 1));
  go(0);
};

let opens_and_stores = () =>
  shipped(() => {
    let m = load("worldtest");
    let (name, code) = current(m);
    let members = text(parts(code).members);
    check(
      int,
      "the library's items come first",
      List.length(WorldSlide.library()),
      List.length(parts(code).library),
    );
    check(
      bool,
      "a definition is a member",
      true,
      contains(members, "let empty_hole = ?;"),
    );
    Persist.save_current("worldtest", m);
    let stored =
      switch (Persist.load_slide_kind("worldtest", name)) {
      | Some(CodePersist({editor: Some(e), _})) => e.editor
      | _ => fail("nothing stored")
      };
    check(bool, "stored at a module root", true, stored.root == Sort.Mod);
    check(
      bool,
      "stored without the library",
      false,
      contains(
        PersistentZipper.to_string(
          PersistentZipper.unpersist(stored.zipper, ~root=Mod),
        ),
        "$prim.fst",
      ),
    );
    let (_, reloaded) =
      current({
        scratchpads: [Persist.load_scratchpad(~settings, "worldtest", name)],
        current: 0,
      });
    check(
      string,
      "reloads as the same members",
      members,
      text(parts(reloaded).members),
    );
  });

/* zipping rebuilds the tiles around the caret: the library is still the
   one that runs. An edit to it is not */
let library_edits = () =>
  shipped(() => {
    let (_, code) = current(load("worldedit"));
    let slide = Option.value_exn(code.world);
    let doc = Web.Program.document(code.program);
    check(
      bool,
      "as loaded",
      true,
      Option.is_none(WorldSlide.library_edit(~slide, doc)),
    );
    let rebuilt =
      List.map(
        ~f=
          (p: Piece.t) =>
            switch (p) {
            | Tile(t) =>
              Piece.Tile({
                ...t,
                children:
                  List.map(~f=kid => List.map(~f=Fun.id, kid), t.children),
              })
            | p => p
            },
        doc,
      );
    check(
      bool,
      "its tiles rebuilt as they were",
      true,
      Option.is_none(WorldSlide.library_edit(~slide, rebuilt)),
    );
    let (edited, found) =
      CorpusUtil.edit_token(
        ~at=`First,
        ~keep_id=true,
        ~needle="fst",
        ~repl="fst2",
        doc,
      );
    check(bool, "the library binds fst", true, found);
    check(
      bool,
      "a renamed definition",
      true,
      Option.is_some(WorldSlide.library_edit(~slide, edited)),
    );
  });

/* no library shipped (as in the other tests): slides stay as they are */
let unshipped = () => {
  let (_, code) = current(load("worldnone"));
  check(bool, "no world", true, Option.is_none(code.world));
};

let tests = (
  "WorldSlide",
  [
    test_case(
      "opens in the world, stores its members",
      `Quick,
      opens_and_stores,
    ),
    test_case("library edits", `Quick, library_edits),
    test_case("no library, no world", `Quick, unshipped),
  ],
);
