open Language;
open Language.Unboxing;

/* A modifier as the *user* writes it. `Meta` is deliberately abstract: it
   means "the platform's command modifier", and is resolved to cmd/ctrl only
   at the moment a binding is applied (see string_of_key_mod). That is what
   lets one config program mean the same thing on every machine. `Ctrl` is
   the literal control key, for bindings that should not follow the
   platform. Mirrors the KeyMod type in the builtin context. */
type key_mod =
  | Meta
  | Ctrl
  | Shift
  | Alt;

/* Mirrors the builtin Shortcut type: an action either has a chord or has no
   shortcut at all. `Unbound` is why this is a sum and not a bare String. */
type binding =
  | Unbound
  | Bound(list(key_mod), string);

type shortcut = {
  action_name: string,
  binding,
};

module DefaultConfiguration = {
  let bound = (action_name, mods, key) => {
    action_name,
    binding: Bound(mods, key),
  };
  let unbound = action_name => {
    action_name,
    binding: Unbound,
  };

  /* The action names must match the ContextualAction labels built in
     Page.re / CodeEditable.re — those labels are the palette entry ids that
     an override is looked up by. */
  let shortcuts = [
    bound("Undo", [Meta], "z"),
    bound("Redo", [Meta, Shift], "z"),
    bound("Go to Definition", [], "F12"),
    bound("Go to Previous Hole", [Shift], "tab"),
    unbound("Go To Next Hole"),
    bound("Select current term", [Meta], "d"),
    bound("Select All", [Meta], "a"),
    unbound("Toggle Selection Focus"),
    bound("Set Selection Focus Left", [Meta, Alt, Shift], "left"),
    bound("Set Selection Focus Right", [Meta, Alt, Shift], "right"),
    bound("Fold", [Alt], "f"),
    bound("Probe", [Alt], "v"),
    bound("Type", [Alt], "t"),
    bound("Livelit", [Alt], "l"),
    unbound("Toggle Statics"),
    unbound("Toggle Completion"),
    unbound("Toggle Show Whitespace"),
    unbound("Toggle Print Benchmarks"),
    unbound("Toggle Toggle Dynamics"),
    unbound("Toggle Show Elaboration"),
    unbound("Toggle Show Function Bodies"),
    unbound("Toggle Show Case Clauses"),
    unbound("Toggle Show fixpoints"),
    unbound("Toggle Show Ascription Steps"),
    unbound("Toggle Show Lookup Steps"),
    unbound("Toggle Show Stepper Filters"),
    unbound("Toggle Show Hidden Steps"),
    unbound("Toggle Show Sidebar"),
    unbound("Toggle Show Docs Feedback"),
    bound("TyDi Assistant", [Meta], "/"),
    unbound("Export Scratch Slide"),
    unbound("Export For Init"),
    unbound("Export Submission"),
    unbound("Reparse Current Editor"),
    bound("Run Benchmark", [], "F7"),
    bound("Introduce", [Meta], "i"),
    unbound("Add New Buffer"),
    unbound("Rename Current Buffer"),
    unbound("Delete Current Buffer"),
    unbound("Export Exercise Module"),
    unbound("Export Transitionary Exercise Module"),
    unbound("Export Grading Exercise Module"),
  ];
};

/* Built fresh per occurrence, never hoisted to a module-level value:
   FreshGrammar mints the id when the combinator is CALLED, so a shared
   value would give every occurrence the same id and the editor would
   collapse them into one tile with N shards. Unannotated, exactly as a
   constructor the user typed would parse — statics resolves it from the
   builtin context. */
let ctr = (name: string): Exp.t =>
  IdTagged.FreshGrammar.Exp.constructor(name, None);

let exp_of_key_mod = (m: key_mod): Exp.t =>
  ctr(
    switch (m) {
    | Meta => "Meta"
    | Ctrl => "Ctrl"
    | Shift => "Shift"
    | Alt => "Alt"
    },
  );

let exp_of_binding = (b: binding): Exp.t => {
  IdTagged.FreshGrammar.Exp.(
    switch (b) {
    | Unbound => ctr("Unbound")
    | Bound(mods, key) =>
      ap(
        Forward,
        ctr("Bound"),
        tuple([list_lit(List.map(exp_of_key_mod, mods)), string(key)]),
      )
    }
  );
};

let shortcut_theme = (shortcuts: list(shortcut)): Exp.t => {
  IdTagged.FreshGrammar.Exp.(
    tuple(
      List.map(
        ({action_name, binding}) =>
          tup_label(label(action_name), exp_of_binding(binding)),
        shortcuts,
      ),
    )
  );
};

let source = {
  open Haz3lcore;
  let exp =
    IdTagged.FreshGrammar.(
      Exp.(
        let_(
          Pat.var("shortcuts"),
          shortcut_theme(DefaultConfiguration.shortcuts),
          var("shortcuts"),
        )
      )
    );

  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.editable(~inline=false),
    exp,
  )
  |> PrettySegment.prettify
  |> Zipper.unzip(~direction=Left)
  |> PersistentZipper.persist;
};

/* The type the Shortcuts slide is analyzed against: one `Shortcut` field per
   known action. `Shortcut` and `KeyMod` come from the builtin context (see
   BuiltinsADT), so the slide needs no type declarations of its own. */
let expected_type = {
  IdTagged.FreshGrammar.Typ.(
    prod(
      List.map(
        ({action_name, _}) =>
          tup_label(label(action_name), var("Shortcut")),
        DefaultConfiguration.shortcuts,
      ),
    )
  );
};

/* Resolve a modifier to what hotkeys-js expects. This is the ONLY place the
   platform is consulted, which is what keeps the config program itself
   system-independent. */
let string_of_key_mod = (m: key_mod): string =>
  switch (m) {
  | Meta => Keyboard.meta()
  | Ctrl => "ctrl"
  | Shift => "shift"
  | Alt => "alt"
  };

/* Canonical order (meta, ctrl, alt, shift) so the palette shows a stable
   label; hotkeys-js itself compares sorted key codes, so order is display
   only. */
let string_of_chord = (mods: list(key_mod), key: string): string => {
  let ordered =
    List.filter(m => List.mem(m, mods), [Meta, Ctrl, Alt, Shift]);
  String.concat("+", List.map(string_of_key_mod, ordered) @ [key]);
};

let key_mod_of_value = (v: Exp.t): option(key_mod) =>
  List.find_map(
    ((name, m)) =>
      switch (unbox(SumNoArg(name), v)) {
      | Matches () => Some(m)
      | _ => None
      },
    [("Meta", Meta), ("Ctrl", Ctrl), ("Shift", Shift), ("Alt", Alt)],
  );

let binding_of_value = (v: Exp.t): option(binding) =>
  switch (unbox(SumNoArg("Unbound"), v)) {
  | Matches () => Some(Unbound)
  | _ =>
    switch (unbox(SumWithArg("Bound"), v)) {
    | Matches(arg) =>
      switch (unbox(Tuple(2), arg)) {
      | Matches([mods, key]) =>
        switch (unbox(ListLit, mods), unbox(Atom(String), key)) {
        | (Matches(ms), Matches(k)) =>
          Some(Bound(List.filter_map(key_mod_of_value, ms), k))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    }
  };

/* Read the evaluated Shortcuts slide back out as an override table.
   Actions the program leaves `Unbound` map to None, which is what clears a
   default binding rather than leaving it in place. */
let overrides_of_value = (value: Exp.t): list((string, option(string))) =>
  switch (value.term) {
  | Tuple(entries) =>
    List.filter_map(
      (x: Exp.t) =>
        switch (x.term) {
        | TupLabel(l, v) =>
          switch (l.term, binding_of_value(v)) {
          | (Label(action_name), Some(Unbound)) =>
            Some((action_name, None))
          | (Label(action_name), Some(Bound(mods, key))) =>
            Some((action_name, Some(string_of_chord(mods, key))))
          | _ => None
          }
        | _ => None
        },
      entries,
    )
  | _ => []
  };
