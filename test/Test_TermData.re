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
  let holes =
    Haz3lcore.Segment.holes(Haz3lcore.CachedSyntax.segment(syntax));
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
      Haz3lcore.CachedSyntax.segment(syntax),
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

let tests = (
  "TermData",
  [
    test_case("root_piece finds grout", `Quick, root_piece_finds_grout),
    test_case("root_piece finds tile", `Quick, root_piece_finds_tile),
  ],
);
