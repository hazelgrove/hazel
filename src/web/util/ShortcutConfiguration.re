open Language;

/* Generates the Shortcuts config slide and reads it back. Every action name
   and default comes from ShortcutAction, and the shortcut encoding itself
   from Language.BuiltinsADT.Shortcut, so this file cannot disagree with
   either the palette or the keybinding projector. */
module A = ShortcutAction;
module S = Language.BuiltinsADT.Shortcut;

/* Each binding ships already wrapped in a keybinding projector, so the slide
   opens as a set of click-to-record widgets rather than raw constructor
   syntax. The underlying term is unchanged — removing a projector reveals the
   same `Bound([Meta], "z")` — so statics and the override read-back are
   unaffected. */
let projected_binding = (b: S.binding): Exp.t =>
  IdTagged.FreshGrammar.Exp.projector(
    {
      kind: ProjectorKind.Keybinding,
      model: Haz3lcore.KeybindingProj.model_string(b),
    },
    S.exp_of_binding(b),
  );

/* One labeled field per action, grouped into one labeled field per section —
   the same grouping the command palette displays. */
let exp_of_section = (s: A.section): Exp.t => {
  IdTagged.FreshGrammar.Exp.(
    tuple(
      List.map(
        a =>
          tup_label(
            label(A.label(a)),
            projected_binding(A.default_binding(a)),
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
              switch (l.term, S.binding_of_exp(v)) {
              | (Label(action_name), Some(b)) =>
                Some((action_name, S.string_of_binding(b)))
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
