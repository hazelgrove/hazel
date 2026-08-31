open Language;

module C = Language.BuiltinsADT.Color;

/* The Colors config slide.

   The program itself is `hazel-programs/config/colors.hz` -- plain, committed,
   hand-editable Hazel text, embedded here at compile time the way the
   documentation slides are. It is not generated: a generator cannot emit
   comments, and the layering is much easier to read stated directly.

   This module is only the CONTRACT around that text: the CSS custom properties
   the app expects the slide to define, the type it is analyzed against, and
   the read-back that turns the evaluated slide into CSS.

   Keeping the names here rather than scraping them from the .hz is deliberate.
   They are what the stylesheets consume, so they are an interface, and
   Test_ColorConfiguration pins that the slide and this list agree -- both that
   the slide type-checks against `expected_type` with no static errors, and
   that evaluating it yields exactly these variables. */

let source: Haz3lcore.PersistentZipper.t =
  Haz3lcore.PersistentZipper.of_slide_text([%blob "../colors.hz"]);

/* The palette layer, published under its own names. Stylesheets consume
   roles (see style/roles.css), but the palette stays the theme-settable input
   layer and saved user themes write these names inline. */
let palette: list(string) = [
  "NONE",
  "SAND",
  "STONE",
  "BLACK",
  "BR1",
  "BR2",
  "BR3",
  "BR4",
  "T1",
  "T2",
  "T3",
  "T4",
  "Y0",
  "Y1",
  "Y2",
  "Y3",
  "R0",
  "R1",
  "R2",
  "TYP",
  "PAT",
  "TPAT",
  "LABEL",
  "highlight-a",
  "highlight-b",
  "highlight-c",
  "G0",
  "G1",
  "G2",
  "GB0",
  "GB1",
];

/* Semantic roles, grouped by the part of the UI they dress. The label IS
   the CSS custom-property name, verbatim -- there is no registry to drift
   from, because `set_css_variable` is the only consumer. */
let role_groups: list((string, list(string))) = [
  (
    "ui",
    [
      "nut-menu",
      "menu-bkg",
      "menu-item-hover-bkg",
      "menu-divider",
      "menu-shadow",
      "ui-bkg",
      "ui-header-text",
      "df-hover-bg",
      "context-meter-fill",
    ],
  ),
  (
    "code",
    ["cell-active", "main-scroll-thumb", "token-buffer", "token-drv"],
  ),
  (
    "shard",
    [
      "shard-lines-exp",
      "shard-caret-pat",
      "shard-caret-typ",
      "shard-caret-tpat",
      "shard-caret-drv",
      "shard-caret-mod",
      "shard-caret-sig",
      "shard-caret-mpat",
    ],
  ),
  (
    "hole",
    [
      "empty-hole-stroke",
      "empty-hole-fill",
      "error-hole-fill",
      "warning-hole-fill",
      "warning-hole-stroke",
      "hole-fill",
      "hole-stroke",
      "hole-active",
    ],
  ),
  ("backpack", ["backpack-selection-outline"]),
  ("projector", ["textarea-text", "fold-bkg"]),
  (
    "dynamics",
    [
      "cell-result-border",
      "incremental-frozen",
      "incremental-frozen-edge",
      "incremental-active-sweep",
    ],
  ),
  ("ci", ["ci-icon-bkg", "ci-status-text", "context-inspector-colon"]),
  ("exercise", ["cell-exercises-border"]),
  (
    "special",
    ["textarea-v-stripe", "textarea-h-stripe", "textarea-h-strip-selected"],
  ),
  (
    "projector_extended",
    [
      "exp-ap",
      "exp-base",
      "pat-base",
      "exp-shadow",
      "pat-shadow",
      "pat-cell",
      "main-base",
      "main-shadow",
      "depth-shadow",
      "sample-above-text",
      "sample-above-bg",
      "sample-above-shadow",
      "sample-below-text",
      "sample-below-bg",
      "sample-below-shadow",
      "sample-neutral-text",
      "sample-neutral-bg",
      "sample-focus-text",
    ],
  ),
];

/* The analyzed type: a labeled tuple of `palette` and `roles`, every leaf a
   `ColorValue`. `ColorValue` lives in the builtin context, so the slide needs
   no type declaration of its own. The editor threads this in as `~ana`, so a
   slide that stops matching it goes red in the buffer. */
/* ── Hazel field -> CSS custom properties ───────────────────────────────

   The slide's field names and the CSS variable names used to be the same
   thing, which is why a themer met `shard-caret-tpat` and `backback-targets`.
   They are separated here: a field sets the properties listed against it, and
   a field with no entry sets the single property of its own name.

   That indirection is what lets one semantic field stand in for a family of
   CSS names -- `token-inconsistent`, `token-rul` and `token-exp` are three
   properties carrying one decision -- without the slide having to name all
   three. Empty for now: this commit changes no output. */
let aliases: list((string, list(string))) = [
  (
    "T2",
    [
      "T2",
      "shard_projector",
      "token-secondary",
      "shard-caret-exp",
      "shard-exp",
      "shard-rul",
      "shard-any",
    ],
  ),
  (
    "R0",
    [
      "R0",
      "shadow-selected",
      "shadow-any",
      "eval-exception",
      "ci-status-error-bkg",
      "test-fail-active",
    ],
  ),
  (
    "STONE",
    [
      "STONE",
      "menu-item-text",
      "token-exp",
      "token-inconsistent",
      "token-rul",
    ],
  ),
  (
    "Y1",
    [
      "Y1",
      "shard-selected",
      "backpack-selection",
      "backpack-joiner",
      "backpack-genie",
    ],
  ),
  (
    "R1",
    [
      "R1",
      "cell-selected-accent",
      "caret-color",
      "error-hole-stroke",
      "test-fail",
    ],
  ),
  (
    "SAND",
    ["SAND", "toggle-knob", "textarea-indicated", "test-percent-text"],
  ),
  (
    "BR1",
    ["BR1", "menu-scroll-track", "cell-result-hidden", "test-indet-active"],
  ),
  ("BR2", ["BR2", "menu-outline", "menu-scroll-thumb", "test-indet"]),
  ("BR4", ["BR4", "menu-icon", "menu-group-name", "cell-result-text"]),
  ("T3", ["T3", "main-bkg", "cell-result", "live-env-bkg"]),
  (
    "R2",
    ["R2", "token-any", "eval-exception-stroke", "ci-status-error-text"],
  ),
  ("G0", ["G0", "primary-accent", "test-pass", "exp-indicated"]),
  ("Y3", ["Y3", "token-string-lit", "token-incomplete"]),
  ("TYP", ["TYP", "token-typ", "main-indicated"]),
  ("PAT", ["PAT", "token-pat", "pat-indicated"]),
  ("NONE", ["NONE", "main-scroll-track"]),
  ("BLACK", ["BLACK", "token-explicit-hole-shadow"]),
  ("Y2", ["Y2", "token-explicit-hole"]),
  ("TPAT", ["TPAT", "token-tpat"]),
  ("LABEL", ["LABEL", "token-label"]),
  ("G1", ["G1", "test-pass-active"]),
  ("G2", ["G2", "token-comment"]),
  ("menu-bkg", ["menu-bkg", "test-panel-bkg"]),
  ("menu-divider", ["menu-divider", "CREASE"]),
  ("menu-shadow", ["menu-shadow", "SHADOW"]),
  ("shard-lines-exp", ["shard-lines-exp", "shard-lines-rul"]),
  ("shard-caret-pat", ["shard-caret-pat", "shard-pat"]),
  ("shard-caret-typ", ["shard-caret-typ", "shard-typ"]),
  ("shard-caret-tpat", ["shard-caret-tpat", "shard-tpat"]),
  ("error-hole-fill", ["error-hole-fill", "ERRHOLE"]),
];

let field_names: list(string) = palette @ List.concat_map(snd, role_groups);

let targets_of = (name: string): list(string) =>
  switch (List.assoc_opt(name, aliases)) {
  | Some(targets) => targets
  | None => [name]
  };

/* Every CSS custom property the slide is responsible for. This, not
   `field_names`, is the output contract: it is what the stylesheets consume,
   what the tests check against, and what `theme_key` must be salted with. */
let all_targets: list(string) = List.concat_map(targets_of, field_names);

let expected_type =
  IdTagged.FreshGrammar.Typ.(
    prod([
      tup_label(
        label("palette"),
        prod(
          List.map(n => tup_label(label(n), var("ColorValue")), palette),
        ),
      ),
      tup_label(
        label("roles"),
        prod(
          List.map(
            ((group, members)) =>
              tup_label(
                label(group),
                prod(
                  List.map(
                    n => tup_label(label(n), var("ColorValue")),
                    members,
                  ),
                ),
              ),
            role_groups,
          ),
        ),
      ),
    ])
  );

let entries_of = (v: Exp.t): list(Exp.t) =>
  switch (v.term) {
  | Tuple(es) => es
  | _ => []
  };

/* One pair per CSS property a labeled field is responsible for -- usually
   one, but a field standing in for a family sets several. */
let colors_of_group = (group: Exp.t): list((string, string)) =>
  List.concat_map(
    (entry: Exp.t) =>
      switch (entry.term) {
      | TupLabel(l, v) =>
        switch (l.term, C.of_exp(v)) {
        | (Label(name), Some(c)) =>
          let css = C.to_css(c);
          List.map(t => (t, css), targets_of(name));
        | _ => []
        }
      | _ => []
      },
    entries_of(group),
  );

/* Read the evaluated slide back out as the CSS custom properties to write.
   Both layers are emitted: stylesheets consume role names via roles.css, and
   saved user themes plus roles.css itself still consume the palette. */
let decoded_vars = (value: Exp.t): list((string, string)) =>
  List.concat_map(
    (section: Exp.t) =>
      switch (section.term) {
      | TupLabel(l, body) =>
        switch (l.term) {
        | Label("palette") => colors_of_group(body)
        /* roles nest one level deeper: group -> entries */
        | Label("roles") =>
          List.concat_map(
            (g: Exp.t) =>
              switch (g.term) {
              | TupLabel(_, members) => colors_of_group(members)
              | _ => []
              },
            entries_of(body),
          )
        | _ => []
        }
      | _ => []
      },
    entries_of(value),
  );

/* All of the properties, or none of them.

   A slide that yields most of a theme is worse than one that yields none: the
   editor ends up half in the user's colours and half in the stylesheet
   defaults, with no indication which is which. `apply_theme_at_startup`
   already reads `[]` as "leave the last theme up", so the empty list is the
   honest answer to a slide that cannot fill the contract. */
let css_vars_of_value = (value: Exp.t): list((string, string)) => {
  let vars = decoded_vars(value);
  let produced = List.sort_uniq(compare, List.map(fst, vars));
  produced == List.sort_uniq(compare, all_targets) ? vars : [];
};

/* The whole load path for a Colors slide: parse, analyze, evaluate, read
   back. Product code rather than a test helper because the startup path
   needs it too — and if the two ran different pipelines, the theme applied
   on load could differ from the one the slide shows.

   Total: any slide the user can save, including one that does not typecheck.
   A broken slide yields no variables (the stylesheet defaults stand); it must
   never take the app down on startup. */
let vars_of_source =
    (slide: Haz3lcore.PersistentZipper.t): list((string, string)) =>
  try({
    let zipper = Haz3lcore.PersistentZipper.unpersist(slide, ~root=Exp);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zipper, ~root=Exp).term;
    let (_, elaborated) =
      Statics.mk(
        ~ana=expected_type,
        CoreSettings.on,
        Builtins.ctx_init(Some(Int)),
        term,
      );
    let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elaborated);
    css_vars_of_value(result);
  }) {
  | _ => []
  };
