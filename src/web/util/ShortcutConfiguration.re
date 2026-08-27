open Language;
open Language.Unboxing;

/* Generates the Shortcuts config slide and reads it back. Every action name
   and default here comes from ShortcutAction, so this file cannot disagree
   with the palette about what an action is called. */
module A = ShortcutAction;

/* Built fresh per occurrence, never hoisted to a module-level value:
   FreshGrammar mints the id when the combinator is CALLED, so a shared value
   would give every occurrence the same id — statics still passes, but the
   editor collapses them into one tile with N shards and Highlight.of_tile
   fails at render. Unannotated, exactly as a constructor the user typed
   would parse; statics resolves it from the builtin context. */
let ctr = (name: string): Exp.t =>
  IdTagged.FreshGrammar.Exp.constructor(name, None);

let exp_of_key_mod = (m: A.key_mod): Exp.t =>
  ctr(
    switch (m) {
    | Meta => "Meta"
    | Ctrl => "Ctrl"
    | Shift => "Shift"
    | Alt => "Alt"
    },
  );

let exp_of_binding = (b: A.binding): Exp.t => {
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

/* One labeled field per action, grouped into one labeled field per section —
   the same grouping the command palette displays. */
let exp_of_section = (s: A.section): Exp.t => {
  IdTagged.FreshGrammar.Exp.(
    tuple(
      List.map(
        a =>
          tup_label(
            label(A.label(a)),
            exp_of_binding(A.default_binding(a)),
          ),
        A.in_section(s),
      ),
    )
  );
};

let shortcut_theme = (): Exp.t => {
  IdTagged.FreshGrammar.Exp.(
    tuple(
      List.map(
        s => tup_label(label(A.section_label(s)), exp_of_section(s)),
        A.populated_sections,
      ),
    )
  );
};

let source = {
  open Haz3lcore;
  let exp =
    IdTagged.FreshGrammar.(
      Exp.(let_(Pat.var("shortcuts"), shortcut_theme(), var("shortcuts")))
    );

  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.editable(~inline=false),
    exp,
  )
  |> PrettySegment.prettify
  |> Zipper.unzip(~direction=Left)
  |> PersistentZipper.persist;
};

/* The analyzed type, with the same shape: a labeled tuple of sections, each
   a labeled tuple of `Shortcut`. `Shortcut` and `KeyMod` live in the builtin
   context, so the slide needs no type declarations of its own. */
let expected_type = {
  IdTagged.FreshGrammar.Typ.(
    prod(
      List.map(
        s =>
          tup_label(
            label(A.section_label(s)),
            prod(
              List.map(
                a => tup_label(label(A.label(a)), var("Shortcut")),
                A.in_section(s),
              ),
            ),
          ),
        A.populated_sections,
      ),
    )
  );
};

let key_mod_of_value = (v: Exp.t): option(A.key_mod) =>
  List.find_map(
    ((name, m)) =>
      switch (unbox(SumNoArg(name), v)) {
      | Matches () => Some(m)
      | _ => None
      },
    [
      ("Meta", A.Meta),
      ("Ctrl", A.Ctrl),
      ("Shift", A.Shift),
      ("Alt", A.Alt),
    ],
  );

let binding_of_value = (v: Exp.t): option(A.binding) =>
  switch (unbox(SumNoArg("Unbound"), v)) {
  | Matches () => Some(A.Unbound)
  | _ =>
    switch (unbox(SumWithArg("Bound"), v)) {
    | Matches(arg) =>
      switch (unbox(Tuple(2), arg)) {
      | Matches([mods, key]) =>
        switch (unbox(ListLit, mods), unbox(Atom(String), key)) {
        | (Matches(ms), Matches(k)) =>
          Some(A.Bound(List.filter_map(key_mod_of_value, ms), k))
        | _ => None
        }
      | _ => None
      }
    | _ => None
    }
  };

let entries_of = (v: Exp.t): list(Exp.t) =>
  switch (v.term) {
  | Tuple(es) => es
  | _ => []
  };

/* Read the evaluated slide back out as an override table, flattening the
   sections away — the palette keys off the action label alone. An action the
   program leaves `Unbound` maps to None, which CLEARS a default binding
   rather than falling back to it. */
let overrides_of_value = (value: Exp.t): list((string, option(string))) =>
  List.concat_map(
    (section: Exp.t) =>
      switch (section.term) {
      | TupLabel(_, group) =>
        List.filter_map(
          (entry: Exp.t) =>
            switch (entry.term) {
            | TupLabel(l, v) =>
              switch (l.term, binding_of_value(v)) {
              | (Label(action_name), Some(b)) =>
                Some((action_name, A.string_of_binding(b)))
              | _ => None
              }
            | _ => None
            },
          entries_of(group),
        )
      | _ => []
      },
    entries_of(value),
  );
