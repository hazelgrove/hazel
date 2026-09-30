open Alcotest;

module TermData = Haz3lcore.TermData;

let syntax_from_string = (s: string): Haz3lcore.CachedSyntax.t =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, s)) {
  | None => fail("Failed to parse: " ++ s)
  | Some(z) =>
    let editor = Haz3lcore.Editor.Model.mk(z, ~root=Exp);
    editor.syntax;
  };

let root_piece_finds_grout = () => {
  let syntax = syntax_from_string("1 +");
  let holes = Haz3lcore.Segment.holes(syntax.segment);
  check(bool, "parse produced at least one hole/grout", true, holes != []);
  List.iter(
    (g: Haz3lcore.Grout.t) => {
      check(
        bool,
        "root_piece returns Some for grout id",
        true,
        TermData.root_piece(g.id, syntax.term_data) != None,
      );
      check(
        bool,
        "root_tile returns None for grout id",
        true,
        TermData.root_tile(g.id, syntax.term_data) == None,
      );
    },
    holes,
  );
};

let root_piece_finds_tile = () => {
  let syntax = syntax_from_string("let x = 1 in x");
  let tile_ids =
    List.filter_map(
      (p: Haz3lcore.Piece.t) =>
        switch (p) {
        | Tile(t) => Some(t.id)
        | _ => None
        },
      syntax.segment,
    );
  check(bool, "found tiles", true, tile_ids != []);
  List.iter(
    id => {
      check(
        bool,
        "root_piece returns Some for tile id",
        true,
        TermData.root_piece(id, syntax.term_data) != None,
      );
      check(
        bool,
        "root_tile returns Some for tile id",
        true,
        TermData.root_tile(id, syntax.term_data) != None,
      );
    },
    tile_ids,
  );
};

/* What the probe toggles (Cmd+E, statics, auto) target with the caret
 * at a derived hole. Holes never live in the edit state, so the
 * indicated piece is a neighbour; Indicated.virtual_hole_id re-derives
 * the hole's id caret-side. KNOWN GAP: inside an unclosed delimiter
 * statics analyzes the canonically completed form, whose hole id the
 * caret side doesn't derive, so the toggles fall back to the indicated
 * piece (`f(` probes the application, `f(1, ` finds nothing). */
let probe_targets_hole = () => {
  let targets = (prog: string): string => {
    let z =
      switch (Haz3lcore.Parser.to_zipper(~root=Exp, prog)) {
      | None => fail("parse failed: " ++ prog)
      | Some(z) => z
      };
    let statics =
      Haz3lcore.CachedStatics.init(
        ~settings=Language.CoreSettings.on,
        ~is_dynamic_term=false,
        ~stitch=x => x,
        ~root=Exp,
        z,
      );
    let syntax = Haz3lcore.Editor.Model.mk(z, ~root=Exp).syntax;
    /* `hole` = the caret's hole; anything else by its term class */
    let name = (id: Haz3lcore.Id.t): string =>
      switch (Haz3lcore.Indicated.virtual_hole_id(z)) {
      | Some(h) when Haz3lcore.Id.equal(h, id) => "hole"
      | _ =>
        switch (Language.Statics.Map.lookup(id, statics.info_map)) {
        | Some(info) => Language.Cls.show(Language.Info.cls_of(info))
        | None => "?"
        }
      };
    let names = ids =>
      ids == [] ? "none" : ids |> List.map(name) |> String.concat(",");
    let go = a => Haz3lcore.ProbePerform.go(~statics, ~syntax, a, z);
    Printf.sprintf(
      "%s¦  manual=%s statics=%s auto=%s",
      prog,
      go(ToggleManual).refractors.manuals |> List.map(fst) |> names,
      go(ToggleStatics).refractors.manuals |> List.map(fst) |> names,
      go(ToggleAuto).refractors.multis.ids
      |> Haz3lcore.Id.Map.bindings
      |> List.map(fst)
      |> names,
    );
  };
  check(
    testable(Fmt.string, String.equal),
    "probe targets",
    {|1 + ¦  manual=hole statics=hole auto=hole
let x = ¦  manual=hole statics=hole auto=hole
let x = 1 in ¦  manual=hole statics=hole auto=hole
fun x -> ¦  manual=hole statics=hole auto=hole
f(¦  manual=Application statics=Application auto=Application
f(1, ¦  manual=none statics=none auto=none|},
    ["1 + ", "let x = ", "let x = 1 in ", "fun x -> ", "f(", "f(1, "]
    |> List.map(targets)
    |> String.concat("\n"),
  );
};

let tests = (
  "TermData",
  [
    test_case("root_piece finds grout", `Quick, root_piece_finds_grout),
    test_case("root_piece finds tile", `Quick, root_piece_finds_tile),
    test_case("probe targets hole", `Quick, probe_targets_hole),
  ],
);
