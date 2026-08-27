open Alcotest;
open Haz3lcore;

/* The config slides are GENERATED (see ColorConfiguration / ShortcutConfiguration)
   rather than parsed from committed .hz text, which makes one mistake easy and
   invisible: building a term with a module-level FreshGrammar value shares one
   id across every occurrence. Statics does not care — the program still checks —
   but the editor collapses the duplicates into a single tile with N shards and
   Highlight.of_tile fails at render time. These pin both the symptom and the
   cause, since neither shows up in a type-level test. */

let pieces_of = (config_type): Base.segment => {
  let (_, source) = Web.ConfigurationMode.Model.default_source(config_type);
  Zipper.zip(PersistentZipper.unpersist(source, ~root=Exp));
};

/* Editor invariant: a tile has exactly one more shard than it has children. */
let rec bad_tiles = (seg: Base.segment): list(string) =>
  List.concat_map(
    (p: Base.piece) =>
      switch (p) {
      | Tile(t) =>
        let here =
          List.length(t.shards) != List.length(t.children) + 1
            ? [
              String.concat("", t.label)
              ++ " shards="
              ++ string_of_int(List.length(t.shards))
              ++ " children="
              ++ string_of_int(List.length(t.children)),
            ]
            : [];
        here @ List.concat_map(bad_tiles, t.children);
      | _ => []
      },
    seg,
  );

let rec ids_of = (seg: Base.segment): list(Id.t) =>
  List.concat_map(
    (p: Base.piece) =>
      switch (p) {
      | Tile(t) => [t.id, ...List.concat_map(ids_of, t.children)]
      | Grout(g) => [g.id]
      | Secondary(s) => [s.id]
      | Projector(pr) => [pr.id]
      },
    seg,
  );

let duplicate_ids = (seg: Base.segment): int => {
  let ids = ids_of(seg);
  List.length(ids) - List.length(List.sort_uniq(Id.compare, ids));
};

let well_formed = (config_type, ()) => {
  let seg = pieces_of(config_type);
  let name = Web.ConfigurationMode.Model.config_name_of_type(config_type);
  check(
    list(string),
    name ++ ": no tile has a shard/children mismatch",
    [],
    bad_tiles(seg),
  );
  check(int, name ++ ": no duplicated piece ids", 0, duplicate_ids(seg));
};

let tests = [
  (
    "ConfigurationMode.default_source",
    List.map(
      config_type =>
        test_case(
          Web.ConfigurationMode.Model.config_name_of_type(config_type)
          ++ " source is well-formed",
          `Quick,
          well_formed(config_type),
        ),
      Web.ConfigurationMode.Model.all_of_config_type,
    ),
  ),
];
