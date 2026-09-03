open Haz3lcore;
open Util;

/* Dedicated ConfigurationMode module for handling different types of configuration
   with side effects. Currently supports ColorScheme configuration.

   This has a lot of overlap with ScratchMode as they're both full slide editors but configuration slides can not be added/deleted
   and each has a side effect after evaluation */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type config_type =
    | ColorScheme
    | Shortcuts;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    configs: list((config_type, CellEditor.Model.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (
    int,
    list((string, option(CellEditor.Model.persistent))),
  );

  let get_current_config = (model: t): (config_type, CellEditor.Model.t) => {
    List.nth(model.configs, model.current);
  };

  let get_current_config_type = (model: t): config_type => {
    get_current_config(model) |> fst;
  };

  let config_name_of_type = (config_type: config_type): string => {
    switch (config_type) {
    | ColorScheme => "Colors"
    | Shortcuts => "Shortcuts"
    };
  };

  /* The key a slide is persisted under, deliberately separate from the name
     it displays under: one is storage, the other is UI text, and coupling
     them is how a rename becomes a silent data migration.

     No version in the key. It used to carry one, so that a slide saved
     against an older contract would not be restored into a newer build --
     but a slide that no longer satisfies the contract simply yields no
     colours, and `apply_theme_at_startup` treats that as "use the defaults"
     (every palette var has a literal default in variables.css). Resetting
     the slide is how the user gets a working one back. That is a better
     answer than a version we have to remember to bump, and it covers shapes
     no bump would have anticipated. */
  let persistence_key = (config_type: config_type): string => {
    switch (config_type) {
    | ColorScheme => "Colors"
    | Shortcuts => "Shortcuts"
    };
  };

  let type_of_persistence_key = (key: string): option(config_type) => {
    List.find_opt(
      config_type => persistence_key(config_type) == key,
      all_of_config_type,
    );
  };

  /* The built-in source for each config slide, used as both the initial
     buffer and the baseline for "has the user changed this?". */
  let default_source = config_type => {
    switch (config_type) {
    | ColorScheme => ("Colors", ColorConfiguration.source)
    | Shortcuts => ("Shortcuts", ShortcutConfiguration.source)
    };
  };

  /* The type a config slide's editor is analyzed against. */
  let expected_type = config_type =>
    switch (config_type) {
    | ColorScheme => Some(ColorConfiguration.expected_type)
    | Shortcuts => Some(ShortcutConfiguration.expected_type)
    };

  let persist = (model: t): persistent => (
    model.current,
    List.map(
      ((config_type: config_type, m: CellEditor.Model.t)) => {
        let name = persistence_key(config_type);
        let current_zipper = m.editor.editor.state.zipper;
        /* Built-in sources are text-backed and mint fresh ids on every
           parse, so id-sensitive segment equality can never match (same
           reasoning as ScratchMode.Scratchpad.persist). Compare the text
           projection instead, and store nothing for an untouched slide
           so a later change to the default is picked up. */
        let default_text =
          default_source(config_type)
          |> snd
          |> ((z: PersistentZipper.t) => z.backup_text)
          |> StringUtil.strip_final_newline;
        let unchanged =
          MarkerParse.seg_to_text(
            ~refractors=current_zipper.refractors.manuals,
            Zipper.zip(current_zipper),
          )
          == default_text;
        (name, unchanged ? None : Some(CellEditor.Model.persist(m)));
      },
      model.configs,
    ),
  );

  let unpersist = (~settings, (current, slides): persistent): t => {
    let get_persistent =
        ((s: string, m: option(CellEditor.Model.persistent))) => {
      let config_type =
        switch (type_of_persistence_key(s)) {
        | Some(ct) => ct
        | None =>
          // Fallback to first config type if name is not recognized
          List.hd(all_of_config_type)
        };
      (
        config_type,
        OptUtil.get(
          () =>
            default_source(config_type)
            |> snd
            |> CellEditor.Model.from_persistent_zipper(~root=Exp),
          m,
        )
        |> CellEditor.Model.unpersist(~settings),
      );
    };
    {
      current:
        List.find_index(
          config_type =>
            persistence_key(config_type)
            == (List.nth(slides, current) |> fst),
          all_of_config_type,
        )
        |> Option.value(~default=0),
      configs:
        List.map(
          (config_type: config_type) =>
            List.find_map(
              s =>
                s |> fst == persistence_key(config_type)
                  ? Some(get_persistent(s)) : None,
              slides,
            )
            |> OptUtil.get(() =>
                 (
                   config_type,
                   default_source(config_type)
                   |> snd
                   |> CellEditor.Model.from_persistent_zipper(~root=Exp)
                   |> CellEditor.Model.unpersist(~settings),
                 )
               ),
          all_of_config_type,
        ),
    };
  };
};

module StoreConfig =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Configuration;
    let default = () => (
      0,
      List.map(
        x =>
          Model.default_source(x)
          |> PairUtil.map_snd(
               CellEditor.Model.from_persistent_zipper(~root=Exp),
             )
          |> PairUtil.map_snd(Option.some),
        Model.all_of_config_type,
      ),
    );
  });

/* ── The colour theme, applied at startup ───────────────────────────────

   HARD REQUIREMENT: the user's theme applies in EVERY mode. `Editors` loads
   and calculates only the mode you are in, and the config side effect below
   only fires while Config mode is open — so before this existed, setting dark
   mode and switching to Scratch gave you back the light stylesheet defaults
   on the next load. The theme therefore has to be established here, at
   startup, with no reference to which mode is active.

   Cached because the slide is a ~500-line Hazel program: parse, statics and
   evaluate cost ~170ms, and in the app the colours normally arrive with the
   Web Worker's evaluation result, well after the first frame. On a cache miss
   the slide is evaluated synchronously instead of being left unthemed — once,
   and only when the slide or the contract has changed.

   The cache lives in localStorage, not the IndexedDB store, because an inline
   <head> script reads it to theme the loading screen before the first paint;
   IndexedDB only opens asynchronously, by which time the spinner has painted.

   Format is newline-delimited — key, then name/value pairs — because a CSS
   colour value cannot contain a newline, so nothing needs escaping and the
   inline script stays three lines. */
let theme_storage_key = "HAZEL_THEME";

/* Which slide the colours come from: the user's if they have edited it, the
   built-in source otherwise. One definition, used for both the cache key and
   the fallback evaluation, so those two can never disagree about the source. */
let colors_source = ((_, slides): Model.persistent): PersistentZipper.t =>
  switch (List.assoc_opt(Model.persistence_key(ColorScheme), slides)) {
  | Some(Some({editor: {zipper, _}, _}: CellEditor.Model.persistent)) => zipper
  | Some(None)
  | None => ColorConfiguration.source
  };

/* Keyed on the slide TEXT, plus the contract's variable names.

   Text rather than the persisted slot because the two sides disagree on
   shape: the store's `default()` carries the built-in source as `Some(..)`
   while `Model.persist` collapses an untouched slide to `None`. Keying the
   slot directly makes those hash differently, and since a stale key only
   means "recompute", the cache would have silently never hit. The contract
   goes in so a build that adds a variable does not serve a cache that never
   defines it. */
let theme_key = (persistent: Model.persistent): string =>
  Printf.sprintf(
    "%d:%d",
    Hashtbl.hash(colors_source(persistent).backup_text),
    /* Joined into one string on purpose: `Hashtbl.hash` samples only the
       first few nodes of a list, so a name added at the end of a 142-entry
       contract would not change the hash. */
    Hashtbl.hash(
      String.concat(
        ",",
        ColorConfiguration.palette
        @ List.concat_map(snd, ColorConfiguration.role_groups),
      ),
    ),
  );

let apply_colors = (vars: list((string, string))): unit =>
  List.iter(
    ((var, color)) => JsUtil.set_css_variable("--" ++ var, color),
    vars,
  );

/* Encode/decode kept pure and separate from storage: the inline <head>
   script in index.html parses this same format, so an off-by-one or a
   delimiter that turns up inside a value would quietly yield a partial
   theme rather than an error. Test_ConfigurationMode round-trips it. */
let encode_theme = (~key: string, vars: list((string, string))): string =>
  String.concat(
    "\n",
    [key, ...List.concat_map(((n, v)) => [n, v], vars)],
  );

/* Names are stored bare; every reader adds the `--`, as `apply_colors` does. */
let decode_theme =
    (blob: string): option((string, list((string, string)))) =>
  switch (String.split_on_char('\n', blob)) {
  | [] => None
  | [key, ...rest] =>
    let rec pairs = (
      fun
      | [name, value, ...tl] => [(name, value), ...pairs(tl)]
      | _ => []
    );
    Some((key, pairs(rest)));
  };

let write_theme_cache = (~key: string, vars: list((string, string))): unit =>
  JsUtil.set_local_storage(theme_storage_key, encode_theme(~key, vars));

let read_theme_cache = (): option((string, list((string, string)))) =>
  JsUtil.get_local_storage(theme_storage_key)
  |> Option.map(decode_theme)
  |> Option.join;

/* Called before the app starts, so the first frame is already themed. */
let apply_theme_at_startup = (): unit => {
  let persistent = StoreConfig.load();
  let key = theme_key(persistent);
  let vars =
    switch (read_theme_cache()) {
    | Some((cached_key, vars)) when cached_key == key && vars != [] => vars
    | _ => ColorConfiguration.vars_of_source(colors_source(persistent))
    };
  /* Nothing on failure -- deliberately. A slide that does not satisfy the
     contract leaves the last theme the inline <head> script painted, and
     leaves the cache holding it, so the editor stays in the colours the user
     chose while they go and fix the slide. Snapping to the defaults instead
     would hand someone a bright editor to repair a dark theme in, and it
     would disagree with the in-session behaviour: `perform_side_effect` only
     fires on a SUCCESSFUL evaluation, so a mid-edit broken slide already
     leaves the last good theme up. With no cache to fall back on there is
     nothing painted, and the literal defaults in variables.css show. */
  if (vars != []) {
    apply_colors(vars);
    write_theme_cache(~key, vars);
  };
};

/* Applied on every successful evaluation of a config slide.

   Colors is a direct DOM effect: CSS variables live on the document, so they
   outlive any re-render on their own. Shortcuts cannot do that — the command
   palette is rebuilt from scratch on every cursor change — so it records the
   override table in settings instead, where the palette build reads it and
   persistence carries it across reloads. */
/* What is currently painted on the document, so `reconcile_colors` can tell
   whether anything needs doing. Physical equality is the right test: the
   evaluated value is shared, so an unchanged result is the same object, and
   the worst a false negative costs is an idempotent rewrite of 269 CSS
   variables. */
let applied_theme: ref(option(Language.Exp.t)) = ref(None);

let apply_color_theme = (model: Model.t, value: Language.Exp.t): unit => {
  let vars = ColorConfiguration.css_vars_of_value(value);
  apply_colors(vars);
  write_theme_cache(~key=theme_key(Model.persist(model)), vars);
  applied_theme := Some(value);
};

let perform_side_effect =
    (
      ~schedule_global: Globals.Update.t => unit,
      model: Model.t,
      config_type: Model.config_type,
      value: Language.Exp.t,
    )
    : unit => {
  switch (config_type) {
  | ColorScheme => apply_color_theme(model, value)
  | Shortcuts =>
    schedule_global(
      Set(
        SetShortcutOverrides(
          ShortcutConfiguration.overrides_of_value(value),
        ),
      ),
    )
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | SwitchConfig(int)
    | ResetCurrent
    | RefreshStatics;

  let update =
      (
        ~schedule_action as _,
        ~schedule_global: Globals.Update.t => unit,
        ~settings: Settings.t,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | CellAction(a) =>
      switch (a) {
      | CellEditor.Update.ResultAction(UpdateResult(ResultOk({result, _}))) =>
        let (config_type, _) = Model.get_current_config(model);
        perform_side_effect(~schedule_global, model, config_type, result);
      // Continue with normal cell update
      | _ => ()
      };

      let (_, ed) = Model.get_current_config(model);
      let* new_ed = CellEditor.Update.update(~settings, a, ed);
      {
        ...model,
        configs:
          ListUtil.put_nth(
            model.current,
            (Model.get_current_config_type(model), new_ed),
            model.configs,
          ),
      };
    | SwitchConfig(i) =>
      Updated.return({
        ...model,
        current: i,
      })
    | ResetCurrent =>
      let (config_type, _) = Model.get_current_config(model);
      let (_, source) = Model.default_source(config_type);
      Updated.return({
        ...model,
        configs:
          ListUtil.put_nth(
            model.current,
            (
              config_type,
              source
              |> CellEditor.Model.from_persistent_zipper(~root=Exp)
              |> CellEditor.Model.unpersist(~settings),
            ),
            model.configs,
          ),
      });
    | RefreshStatics =>
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      model |> Updated.return_quiet(~recalculate=true);
    };
  };
  let reconcile_colors =
      (config_type: Model.config_type, model: Model.t, ed: CellEditor.Model.t)
      : unit =>
    switch (
      config_type,
      EvalResult.Model.get_value(ed.result),
      applied_theme^,
    ) {
    | (ColorScheme, Some(value), Some(painted)) when value === painted => ()
    | (ColorScheme, Some(value), _) => apply_color_theme(model, value)
    | (ColorScheme, None, _)
    | (Shortcuts, _, _) => ()
    };

  let calculate =
      (
        ~settings,
        ~autoprobe_mode,
        ~schedule_action,
        ~is_edited,
        model: Model.t,
      )
      : Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );
    let (config_type, ed) = List.nth(model.configs, model.current);
    let worker_request = ref([]);
    let queue_worker =
      Some(
        (req_value: WorkerServer.Request.value) => {
          worker_request := worker_request^ @ [("", req_value)]
        },
      );
    let new_ed =
      CellEditor.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        ~statics_mode,
        /* the Shortcuts slide is checked against the known action set */
        ~ana=?Model.expected_type(config_type),
        ~queue_worker,
        ~stitch=x => x,
        ed,
      );
    let dispatch = (_key, action) =>
      schedule_action(CellAction(ResultAction(action)));
    EvalRequest.request(
      worker_request^,
      ~pos_of_key=key => key,
      ~dispatch,
      ~on_timeout=
        List.iter(((key, _)) =>
          dispatch(key, UpdateResult(ResultFail(Timeout)))
        ),
    );
    let model = {
      ...model,
      configs:
        ListUtil.put_nth(
          model.current,
          (config_type, new_ed),
          model.configs,
        ),
    };
    /* The painted theme is DOM state, so unlike a rendered view it does not
       follow the model on its own -- something has to put it back. Applying it
       when an evaluation ARRIVES is not enough: undo installs a whole snapshot
       and never replays the actions that built it, so the buffer and the
       printed result go back while the document keeps the colours of a future
       that was undone. Reconciling here, against whatever model is current,
       covers undo and every other path that swaps a model in wholesale. */
    reconcile_colors(config_type, model, new_ed);
    model;
  };
};
module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | Cell(selection) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(CellAction(a)),
          ~selection,
          List.nth(model.configs, model.current) |> snd,
        );
      Update.CellAction(a);
    | TextBox => empty
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) =>
    CellEditor.Selection.jump_to_tile(
      tile,
      List.nth(model.configs, model.current) |> snd,
    )
    |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)));
};
module View = {
  type event =
    | MakeActive(Selection.t);
  let view =
      (
        ~globals,
        ~signal: event => 'a,
        ~inject: Update.t => 'a,
        ~selected: option(Selection.t),
        model: Model.t,
      ) => {
    [
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(selection) => signal(MakeActive(Cell(selection))),
        ~inject=a => inject(CellAction(a)),
        ~selected=
          switch (selected) {
          | Some(Selection.Cell(s)) => Some(s)
          | _ => None
          },
        ~locked=false,
        List.nth(model.configs, model.current) |> snd,
      ),
    ];
  };

  let top_bar = (~inject: Update.t => 'a, model: Model.t) => {
    EditorModeView.view(
      ~edit_buttons=false,
      ~nav_buttons=false,
      ~signal=
        fun
        | Previous =>
          inject(
            SwitchConfig(
              (model.current + List.length(model.configs) - 1)
              mod List.length(model.configs),
            ),
          )
        | Next =>
          inject(
            SwitchConfig((model.current + 1) mod List.length(model.configs)),
          )
        | Add => Virtual_dom.Vdom.Effect.Ignore
        | Rename => Virtual_dom.Vdom.Effect.Ignore
        | Delete => Virtual_dom.Vdom.Effect.Ignore,
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(SwitchConfig(i)),
          model.current,
          List.map(
            ((config_type, _)) => Model.config_name_of_type(config_type),
            model.configs,
          ),
        ),
      (),
    );
  };

  let file_menu = (~globals: Globals.t, ~inject: Update.t => 'a, _: Model.t) => {
    let export_button_for_init =
      Widgets.button_named(
        Icons.export,
        _ => globals.inject_global(ExportForInit),
        ~tooltip="Export for Init",
      );

    let file_group_scratch =
      NutMenu.item_group(~inject, "File", [export_button_for_init]);

    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this configuration? You will lose any existing settings.",
            );
          if (confirmed) {
            inject(ResetCurrent);
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Editor",
      );

    let reparse =
      Widgets.button_named(
        Icons.backpack,
        _ => globals.inject_global(ActiveEditor(Reparse)),
        ~tooltip="Reparse Editor",
      );

    let reset_hazel =
      Widgets.button_named(
        Icons.bomb,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            HazelDB.clear_all();
            Js_of_ocaml.Dom_html.window##.location##reload;
          };
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Reset Hazel (LOSE ALL DATA)",
      );

    let reset_group_scratch =
      NutMenu.item_group(
        ~inject,
        "Reset",
        [reset_button, reparse, reset_hazel],
      );

    [file_group_scratch, reset_group_scratch];
  };
};
