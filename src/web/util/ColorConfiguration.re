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
      "primary-accent",
      "nut-menu",
      "nut-menu-active",
      "menu-bkg",
      "menu-item-hover-bkg",
      "menu-item-text",
      "menu-outline",
      "menu-icon",
      "menu-group-name",
      "menu-scroll-thumb",
      "menu-scroll-track",
      "menu-divider",
      "menu-shadow",
      "ui-bkg",
      "ui-header-text",
      "toggle-knob",
      "df-hover-bg",
      "context-meter-fill",
    ],
  ),
  (
    "code",
    [
      "main-bkg",
      "cell-active",
      "main-scroll-thumb",
      "main-scroll-track",
      "cell-selected-accent",
      "caret-color",
      "error-hole-stroke",
      "token-exp",
      "token-pat",
      "token-typ",
      "token-tpat",
      "token-label",
      "token-string-lit",
      "token-comment",
      "token-incomplete",
      "token-inconsistent",
      "token-buffer",
      "token-explicit-hole",
      "token-explicit-hole-shadow",
      "token-secondary",
      "token-rul",
      "token-any",
      "token-drv",
    ],
  ),
  (
    "shard",
    [
      "shard-caret-exp",
      "shard-lines-exp",
      "shard-exp",
      "shard-caret-pat",
      "shard-caret-typ",
      "shard-caret-tpat",
      "shard-pat",
      "shard-typ",
      "shard-tpat",
      "shard-selected",
      "shard-buffer",
      "shard_projector",
      "shard-rul",
      "shard-lines-rul",
      "shadow-selected",
      "shard-any",
      "shadow-any",
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
  (
    "backpack",
    [
      "backpack-selection",
      "backpack-joiner",
      "backpack-genie",
      "backpack-selection-outline",
      "backback-targets",
    ],
  ),
  ("projector", ["textarea-indicated", "textarea-text", "fold-bkg"]),
  (
    "dynamics",
    [
      "cell-result-text",
      "cell-result-border",
      "cell-result-hidden",
      "eval-exception",
      "eval-exception-stroke",
      "step-hole-color",
      "incremental-frozen",
      "incremental-frozen-edge",
      "incremental-active-sweep",
    ],
  ),
  (
    "ci",
    [
      "ci-icon-bkg",
      "ci-status-text",
      "ci-status-error-text",
      "ci-status-error-bkg",
      "context-inspector-colon",
    ],
  ),
  (
    "exercise",
    [
      "cell-caption",
      "cell-result",
      "cell-exercises-border",
      "test-panel-bkg",
      "test-percent-text",
      "test-pass",
      "test-pass-active",
      "test-fail",
      "test-fail-active",
      "test-indet",
      "test-indet-active",
    ],
  ),
  (
    "special",
    [
      "textarea-v-stripe",
      "textarea-h-stripe",
      "textarea-h-strip-selected",
      "SHADOW",
      "ERRHOLE",
      "CREASE",
    ],
  ),
  (
    "projector_extended",
    [
      "live-env-bkg",
      "num-closures",
      "num-closures-indicated",
      "exp-ap",
      "pat-ap",
      "exp-indicated",
      "pat-indicated",
      "exp-ap-indicated",
      "exp-base",
      "pat-base",
      "exp-shadow",
      "pat-shadow",
      "exp-ap-shadow",
      "exp-cell",
      "pat-cell",
      "main-base",
      "main-shadow",
      "main-indicated",
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

/* One `(name, value)` pair per labeled field holding a decodable colour. */
let colors_of_group = (group: Exp.t): list((string, string)) =>
  List.filter_map(
    (entry: Exp.t) =>
      switch (entry.term) {
      | TupLabel(l, v) =>
        switch (l.term, C.of_exp(v)) {
        | (Label(name), Some(c)) => Some((name, C.to_css(c)))
        | _ => None
        }
      | _ => None
      },
    entries_of(group),
  );

/* Read the evaluated slide back out as the CSS custom properties to write.
   Both layers are emitted: stylesheets consume role names via roles.css, and
   saved user themes plus roles.css itself still consume the palette. */
let css_vars_of_value = (value: Exp.t): list((string, string)) =>
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
