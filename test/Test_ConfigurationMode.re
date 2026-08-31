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

/* ── Colour theme cache ────────────────────────────────────────────────

   The cache is only ever read when the key computed at startup, from the
   store, equals the key written at evaluation time, from the live model. So
   the whole thing rests on `theme_key` surviving `unpersist` -> `persist`.
   If it does not, the cache silently never hits: no error, no warning, just
   the slow path forever. That is worth pinning explicitly. */
module CM = Web.ConfigurationMode;

let colors_key = CM.Model.persistence_key(ColorScheme);

let key_round_trips = (label, p) =>
  check(
    string,
    label ++ ": theme key survives unpersist -> persist",
    CM.theme_key(p),
    CM.theme_key(
      CM.Model.persist(
        CM.Model.unpersist(~settings=Language.CoreSettings.on, p),
      ),
    ),
  );

/* An untouched slide persists as None, so the key must be stable across a
   reload that never opens Config mode. */
let default_key_round_trips = () =>
  key_round_trips("untouched", CM.StoreConfig.default());

/* A user-edited slide persists as Some(..); the key has to round-trip the
   serialized zipper too, not just the None. */
let edited_source = () => {
  let text = Web.ColorConfiguration.source.backup_text;
  let edited =
    Str.replace_first(
      Str.regexp_string("^^check(false)"),
      "^^check(true)",
      text,
    );
  check(bool, "the edit actually changed the source", true, edited != text);
  Haz3lcore.PersistentZipper.of_slide_text(edited);
};

let normalize = p =>
  CM.Model.persist(
    CM.Model.unpersist(~settings=Language.CoreSettings.on, p),
  );

/* Normalized once, because that is the shape both the store and the cache
   actually hold — each is written by `persist`. */
let edited_persistent = () =>
  normalize((
    0,
    [
      (
        colors_key,
        Some(
          Web.CellEditor.Model.from_persistent_zipper(
            ~root=Exp,
            edited_source(),
          ),
        ),
      ),
    ],
  ));

let edited_key_round_trips = () =>
  key_round_trips("edited", edited_persistent());

/* And the two must not collide, or a dark-mode user gets served the light
   theme out of the cache. */
let edited_key_differs = () =>
  check(
    bool,
    "an edited slide keys differently from the built-in one",
    true,
    CM.theme_key(edited_persistent())
    != CM.theme_key(CM.StoreConfig.default()),
  );

/* The inline <head> script in index.html parses this format too, so the
   values that actually occur — parens, commas, spaces, percent, hash — have
   to survive it. Newline is the delimiter precisely because no CSS colour
   value can contain one. */
let theme_cache_round_trips = () => {
  let vars = [
    ("BR1", "oklch(0.85 0.07 90)"),
    ("SHADOW", "color-mix(in oklab, oklch(0.5 0.1 90) 33%, transparent)"),
    ("NONE", "transparent"),
    ("HEX", "#a1b2c3"),
  ];
  check(
    option(pair(string, list(pair(string, string)))),
    "encode -> decode is the identity",
    Some(("k1:k2", vars)),
    CM.decode_theme(CM.encode_theme(~key="k1:k2", vars)),
  );
};

/* Cheap but load-bearing: the startup theme is only complete if the slide
   really produces every variable the stylesheets consume. */
let startup_theme_is_complete = () => {
  let produced =
    List.map(
      fst,
      Web.ColorConfiguration.vars_of_source(
        CM.colors_source(CM.StoreConfig.default()),
      ),
    );
  let declared =
    Web.ColorConfiguration.palette
    @ List.concat_map(snd, Web.ColorConfiguration.role_groups);
  check(
    list(string),
    "the startup theme defines every declared colour",
    [],
    List.filter(n => !List.mem(n, produced), declared),
  );
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
  (
    "ConfigurationMode.theme_key",
    [
      test_case(
        "untouched slide round-trips",
        `Quick,
        default_key_round_trips,
      ),
      test_case("edited slide round-trips", `Quick, edited_key_round_trips),
      test_case("edited slide keys differently", `Quick, edited_key_differs),
      test_case("cache format round-trips", `Quick, theme_cache_round_trips),
      test_case(
        "startup theme is complete",
        `Quick,
        startup_theme_is_complete,
      ),
    ],
  ),
];
