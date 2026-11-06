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
              String.concat("", Tile.label(t))
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

/* ── Color theme cache ────────────────────────────────────────────────

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
   to survive it. Newline is the delimiter precisely because no CSS color
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
  let declared = Web.ColorConfiguration.all_targets;
  check(
    list(string),
    "the startup theme defines every declared color",
    [],
    List.filter(n => !List.mem(n, produced), declared),
  );
};

/* ── The theme follows the model, not the event ────────────────────────

   `perform_side_effect` paints the theme when an evaluation ARRIVES. Undo
   installs a whole `Page.Model.t` from the history stack and never replays the
   actions that produced it, so no `UpdateResult` is sent and nothing repaints
   -- the buffer and the printed result went back while the document kept the
   colors of a future that had been undone. `Update.calculate` reconciles
   against the model instead, and it reads the value through
   `EvalResult.Model.get_value`.

   That accessor is the hinge. If it ever stops returning what `UpdateResult`
   carried -- a `Calc` change, a different result wrapper -- the reconcile
   turns into a no-op and undo silently stops repainting again, with nothing
   failing. So pin the two against each other. */
let evaluated = (result: Language.Exp.t): Web.EvalResult.Model.t => {
  let updated =
    Web.EvalResult.Update.update(
      ~settings=Web.Settings.Model.init,
      UpdateResult(
        ResultOk({
          result,
          state: Language.EvaluatorState.empty,
        }),
      ),
      Web.EvalResult.Model.init,
    );
  updated.model;
};

let value_read_back_matches_the_event = () => {
  let result = Language.Exp.fresh(EmptyHole);
  check(
    bool,
    "get_value returns the very value UpdateResult carried",
    true,
    switch (Web.EvalResult.Model.get_value(evaluated(result))) {
    | Some(v) => v === result
    | None => false
    },
  );
};

/* ── The config editor is selected in Configuration mode ──────────────

   The page keeps one selection, and a selection from another mode reaches
   no editor: keys go nowhere, and cursor actions (Reparse, Problems
   next/previous) find nothing, until the user clicks into the editor. */
let ignore_inject = _ => Ui_effect.Ignore;

let selects_an_editor = (selection, editors: Web.Editors.Model.t) =>
  Web.Editors.Selection.get_cursor_info(
    ~inject=ignore_inject,
    ~selection,
    editors,
  ).
    editor
  != None;

/* A reload in Configuration mode starts from default_selection. */
let reload_selects_config_editor = () => {
  let editors: Web.Editors.Model.t =
    Config(
      CM.Model.unpersist(
        ~settings=Language.CoreSettings.on,
        CM.StoreConfig.default(),
      ),
    );
  check(
    bool,
    "the default selection reaches the config editor",
    true,
    selects_an_editor(
      Web.Editors.Selection.default_selection(editors),
      editors,
    ),
  );
};

/* Switching into Configuration mode replaces the old mode's selection. */
let switch_selects_config_editor = (action: Web.Editors.Update.t, ()) => {
  let globals = Web.Globals.Model.init();
  let (default_current, slides) = Lazy.force(Web.Init.startup).scratch;
  let editors: Web.Editors.Model.t =
    Scratch(
      Web.ScratchMode.Persist.load_all(
        "scratch",
        ~settings=globals.settings.core,
        ~default_names=List.map(fst, slides),
        ~default_current,
      ),
    );
  let page: Web.Page.Model.t = {
    globals,
    editors,
    explain_this: Web.ExplainThisModel.init,
    selection: Web.Editors.Selection.default_selection(editors),
  };
  let page =
    Web.Page.Update.update(
      ~import_log=_ => (),
      ~get_log_and=_ => (),
      ~schedule_action=_ => (),
      Editors(action),
      page,
    ).
      model;
  check(
    bool,
    "in Configuration mode",
    true,
    switch (page.editors) {
    | Config(_) => true
    | Scratch(_)
    | Documentation(_)
    | Exercises(_)
    | Tutorial(_) => false
    },
  );
  check(
    bool,
    "the selection reaches the config editor",
    true,
    selects_an_editor(page.selection, page.editors),
  );
};

let tests = [
  (
    "ConfigurationMode.selection",
    [
      test_case(
        "reload in Configuration mode selects the config editor",
        `Quick,
        reload_selects_config_editor,
      ),
      test_case(
        "SwitchMode(Config) selects the config editor",
        `Quick,
        switch_selects_config_editor(SwitchMode(Config)),
      ),
      test_case(
        "ShowConfig selects the config editor",
        `Quick,
        switch_selects_config_editor(ShowConfig(ColorScheme)),
      ),
    ],
  ),
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
      /* Slow: each loads the edited Colors slide through the store */
      test_case("edited slide round-trips", `Slow, edited_key_round_trips),
      test_case("edited slide keys differently", `Slow, edited_key_differs),
      test_case("cache format round-trips", `Quick, theme_cache_round_trips),
      /* Slow: it analyzes the whole Colors slide (see
         Test_ColorConfiguration) */
      test_case(
        "startup theme is complete",
        `Slow,
        startup_theme_is_complete,
      ),
    ],
  ),
  (
    "ConfigurationMode.reconcile",
    [
      test_case(
        "value read back matches the event",
        `Quick,
        value_read_back_matches_the_event,
      ),
    ],
  ),
];
