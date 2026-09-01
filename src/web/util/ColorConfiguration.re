open Language;

module C = Language.BuiltinsADT.Color;

/* The Colors config slide.

   The program itself is `hazel-programs/config/colors.hz` -- plain, committed,
   hand-editable Hazel text, embedded here at compile time the way the
   documentation slides are. It is not generated: a generator cannot emit
   comments, and the layering is much easier to read stated directly.

   This module is only the CONTRACT around that text, and it has two levels.
   The slide names ROLES -- what a colour is for, in the user's terms. The
   `aliases` table below fans each role out to the CSS custom properties that
   carry it. So `expected_type` is built from the role names, `all_targets` is
   the list of properties the stylesheets actually read, and the read-back
   walks the evaluated slide from one to the other.

   Keeping the names here rather than scraping them from the .hz is deliberate.
   They are what the stylesheets consume, so they are an interface, and
   Test_ColorConfiguration pins that the slide and this list agree -- both that
   the slide type-checks against `expected_type` with no static errors, and
   that evaluating it yields exactly these variables. */

let source: Haz3lcore.PersistentZipper.t =
  Haz3lcore.PersistentZipper.of_slide_text([%blob "../colors.hz"]);

/* The palette layer, published under its own names. It is the input the rest
   of the theme is derived from, and the names a saved user theme writes; the
   `aliases` rows below hang each role off the palette colour that decides it,
   so component stylesheets never have to name a palette colour directly. */
let palette: list(string) = [
  "none",
  "code-background",
  "ink",
  "black",
  "frame-1",
  "frame-2",
  "frame-3",
  "frame-4",
  "surface-1",
  "surface-2",
  "surface-3",
  "surface-4",
  "attention-1",
  "attention-2",
  "attention-3",
  "attention-4",
  "error-1",
  "error-2",
  "error-3",
  "type",
  "pattern",
  "type-pattern",
  "label",
  "doc-1",
  "doc-2",
  "doc-3",
  "success",
  "success-soft",
  "success-muted",
  "info",
  "info-strong",
];

/* Semantic roles, grouped by the part of the UI they dress. These are the
   names a themer meets, so they say what a colour is for rather than where it
   happens to be plumbed; `aliases` maps them onto the CSS properties. Groups
   are for reading -- the decoder matches them with a wildcard -- but they must
   agree with `aliases`, which is keyed by (group, name). */
let role_groups: list((string, list(string))) = [
  ("menu", ["nut", "background", "hover", "divider", "shadow"]),
  ("chrome", ["background", "heading", "meter", "table-row-hover"]),
  (
    "editor",
    [
      "cell",
      "scrollbar",
      "buffer",
      "derivation",
      "locked-cell",
      "backpack-outline",
      "string",
      "comment",
      "selection",
    ],
  ),
  (
    "cursor",
    [
      "connector",
      "pattern",
      "type",
      "type-pattern",
      "derivation",
      "module",
      "signature",
      "module-pattern",
    ],
  ),
  ("hole", ["empty", "empty-edge", "error", "warning", "warning-edge"]),
  ("problems", ["row", "row-edge", "row-active"]),
  ("results", ["divider", "reused", "reused-edge", "sweep"]),
  ("inspector", ["badge", "text", "separator"]),
  (
    "probe",
    [
      "value",
      "value-edge",
      "pattern",
      "pattern-edge",
      "application",
      "timeline",
      "depth",
      "caller",
      "caller-text",
      "caller-edge",
      "callee",
      "callee-text",
      "callee-edge",
      "other",
      "other-text",
      "focus-text",
    ],
  ),
  (
    "projector",
    [
      "statics-background",
      "statics-edge",
      "textarea-text",
      "fold-background",
      "textarea-margin",
      "textarea-rule",
      "textarea-rule-selected",
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
   three. It also pins the properties whose names would otherwise follow a
   role's: renaming a role must not rename a CSS variable, so a renamed role
   keeps a row here naming the property it always wrote. */
let aliases: list(((string, string), list(string))) = [
  (
    ("palette", "surface-2"),
    [
      "surface-2",
      "shard_projector",
      "token-secondary",
      "shard-caret-exp",
      "shard-exp",
      "shard-rul",
      "shard-any",
      "border-raised",
      "surface-raised",
      "df-zebra-bg",
      "table-header-bg",
    ],
  ),
  (
    ("palette", "error-1"),
    [
      "error-1",
      "shadow-selected",
      "shadow-any",
      "eval-exception",
      "ci-status-error-bkg",
      "test-fail-active",
      "shadow-error-soft",
      "surface-error-soft",
    ],
  ),
  (
    ("palette", "ink"),
    [
      "ink",
      "menu-item-text",
      "token-exp",
      "token-inconsistent",
      "token-rul",
      "surface-inverse",
      "text-default",
      "border-inverse",
      "code-text",
      "token-mod",
    ],
  ),
  (
    ("palette", "error-2"),
    [
      "error-2",
      "cell-selected-accent",
      "caret-color",
      "error-hole-stroke",
      "test-fail",
      "border-error",
      "shadow-error",
      "surface-error",
      "text-error",
      "num-samples-indicated",
    ],
  ),
  (
    ("palette", "code-background"),
    [
      "code-background",
      "toggle-knob",
      "textarea-indicated",
      "test-percent-text",
      "border-seam",
      "surface-code",
      "text-inverse",
      "df-bg",
    ],
  ),
  (
    ("palette", "frame-1"),
    [
      "frame-1",
      "menu-scroll-track",
      "cell-result-hidden",
      "test-indet-active",
      "border-soft",
      "shadow-soft",
      "surface-shard",
      "text-faint",
    ],
  ),
  (
    ("palette", "frame-2"),
    [
      "frame-2",
      "menu-outline",
      "menu-scroll-thumb",
      "test-indet",
      "border-default",
      "surface-shard-strong",
      "text-muted",
    ],
  ),
  (
    ("palette", "frame-4"),
    [
      "frame-4",
      "menu-icon",
      "menu-group-name",
      "cell-result-text",
      "border-stronger",
      "surface-accent-strong",
      "text-strong",
      "context-meter-track",
      "editor-mode-text",
      "select-text",
    ],
  ),
  (
    ("palette", "surface-3"),
    [
      "surface-3",
      "main-bkg",
      "cell-result",
      "live-env-bkg",
      "surface-sunken",
      "text-sunken",
      "border-sunken",
    ],
  ),
  (
    ("palette", "error-3"),
    [
      "error-3",
      "token-any",
      "eval-exception-stroke",
      "ci-status-error-text",
      "border-error-strong",
      "surface-error-strong",
      "text-error-strong",
    ],
  ),
  (
    ("palette", "success"),
    [
      "success",
      "primary-accent",
      "test-pass",
      "exp-indicated",
      "border-success",
      "shadow-success",
      "surface-success",
      "text-success",
      "fold-accent",
    ],
  ),
  (
    ("palette", "type"),
    [
      "type",
      "token-typ",
      "main-indicated",
      "border-typ",
      "shadow-typ",
      "text-typ",
      "exp-ap-indicated",
      "token-sig",
    ],
  ),
  (
    ("palette", "pattern"),
    [
      "pattern",
      "token-pat",
      "pat-indicated",
      "shadow-pat",
      "text-pat",
      "surface-pat",
      "token-mpat",
    ],
  ),
  (
    ("palette", "none"),
    ["none", "main-scroll-track", "surface-none", "text-none"],
  ),
  (
    ("palette", "black"),
    ["black", "token-explicit-hole-shadow", "border-black", "text-black"],
  ),
  (
    ("palette", "attention-3"),
    [
      "attention-3",
      "token-explicit-hole",
      "border-highlight-strong",
      "surface-highlight-strong",
    ],
  ),
  (
    ("palette", "type-pattern"),
    ["type-pattern", "token-tpat", "text-tpat"],
  ),
  (("palette", "label"), ["label", "token-label", "surface-label"]),
  (
    ("palette", "success-soft"),
    [
      "success-soft",
      "test-pass-active",
      "surface-success-soft",
      "text-success-strong",
    ],
  ),
  (("menu", "background"), ["menu-bkg", "test-panel-bkg"]),
  (("menu", "divider"), ["menu-divider", "CREASE"]),
  (("menu", "shadow"), ["menu-shadow", "SHADOW"]),
  (("cursor", "connector"), ["shard-lines-exp", "shard-lines-rul"]),
  (("cursor", "pattern"), ["shard-caret-pat", "shard-pat"]),
  (("cursor", "type"), ["shard-caret-typ", "shard-typ"]),
  (("cursor", "type-pattern"), ["shard-caret-tpat", "shard-tpat"]),
  (("hole", "error"), ["error-hole-fill", "ERRHOLE"]),
  (("menu", "nut"), ["nut-menu"]),
  (("menu", "hover"), ["menu-item-hover-bkg", "light-page-color"]),
  (("chrome", "background"), ["ui-bkg"]),
  (("chrome", "heading"), ["ui-header-text"]),
  (("chrome", "meter"), ["context-meter-fill"]),
  (("chrome", "table-row-hover"), ["df-hover-bg"]),
  (("editor", "cell"), ["cell-active"]),
  (("editor", "scrollbar"), ["main-scroll-thumb"]),
  (("editor", "buffer"), ["token-buffer"]),
  (("editor", "derivation"), ["token-drv"]),
  (("editor", "locked-cell"), ["cell-exercises-border"]),
  (("editor", "backpack-outline"), ["backpack-selection-outline"]),
  (("cursor", "derivation"), ["shard-caret-drv", "shard-drv"]),
  (("cursor", "module"), ["shard-caret-mod", "shard-mod"]),
  (("cursor", "signature"), ["shard-caret-sig", "shard-sig"]),
  (("cursor", "module-pattern"), ["shard-caret-mpat", "shard-mpat"]),
  (("hole", "empty"), ["empty-hole-fill"]),
  (("hole", "empty-edge"), ["empty-hole-stroke"]),
  (("hole", "warning"), ["warning-hole-fill", "ci-status-warning-bkg"]),
  (
    ("hole", "warning-edge"),
    ["warning-hole-stroke", "ci-status-warning-text"],
  ),
  (("problems", "row"), ["hole-fill"]),
  (("problems", "row-edge"), ["hole-stroke"]),
  (("problems", "row-active"), ["hole-active"]),
  (("results", "divider"), ["cell-result-border"]),
  (("results", "reused"), ["incremental-frozen"]),
  (("results", "reused-edge"), ["incremental-frozen-edge"]),
  (("results", "sweep"), ["incremental-active-sweep"]),
  (("inspector", "badge"), ["ci-icon-bkg"]),
  (("inspector", "text"), ["ci-status-text"]),
  (("inspector", "separator"), ["context-inspector-colon"]),
  (("probe", "value"), ["exp-base"]),
  (("probe", "value-edge"), ["exp-shadow"]),
  (("probe", "pattern"), ["pat-base"]),
  (("probe", "pattern-edge"), ["pat-shadow"]),
  (("probe", "application"), ["exp-ap"]),
  (("probe", "timeline"), ["pat-cell"]),
  (("probe", "depth"), ["depth-shadow"]),
  (("probe", "caller"), ["sample-above-bg"]),
  (("probe", "caller-text"), ["sample-above-text"]),
  (("probe", "caller-edge"), ["sample-above-shadow"]),
  (("probe", "callee"), ["sample-below-bg"]),
  (("probe", "callee-text"), ["sample-below-text"]),
  (("probe", "callee-edge"), ["sample-below-shadow"]),
  (("probe", "other"), ["sample-neutral-bg"]),
  (("probe", "other-text"), ["sample-neutral-text"]),
  (("probe", "focus-text"), ["sample-focus-text"]),
  (("projector", "statics-background"), ["main-base"]),
  (("projector", "statics-edge"), ["main-shadow"]),
  (("projector", "fold-background"), ["fold-bkg"]),
  (("projector", "textarea-margin"), ["textarea-v-stripe"]),
  (("projector", "textarea-rule"), ["textarea-h-stripe"]),
  (("projector", "textarea-rule-selected"), ["textarea-h-strip-selected"]),
  (("editor", "string"), ["token-string-lit", "token-incomplete"]),
  (("editor", "comment"), ["token-comment"]),
  (
    ("editor", "selection"),
    [
      "shard-selected",
      "backpack-selection",
      "backpack-joiner",
      "backpack-genie",
    ],
  ),
  (
    ("palette", "attention-1"),
    ["attention-1", "surface-highlight-soft", "shard-selected-expanded"],
  ),
  (
    ("palette", "attention-2"),
    [
      "attention-2",
      "border-highlight",
      "shadow-highlight",
      "surface-highlight",
      "num-samples",
    ],
  ),
  (
    ("palette", "attention-4"),
    ["attention-4", "border-warning", "surface-warning", "text-warning"],
  ),
  (("palette", "doc-1"), ["doc-1", "surface-highlight-a", "text-doc-1"]),
  (("palette", "doc-2"), ["doc-2", "surface-highlight-b", "text-doc-2"]),
  (("palette", "doc-3"), ["doc-3", "surface-highlight-c", "text-doc-3"]),
  (
    ("palette", "frame-3"),
    [
      "frame-3",
      "border-strong",
      "shadow-strong",
      "surface-accent",
      "text-accent",
      "explain-this-expander",
    ],
  ),
  (("palette", "info"), ["info", "text-info"]),
  (
    ("palette", "info-strong"),
    ["info-strong", "border-info", "text-info-strong"],
  ),
  (
    ("palette", "success-muted"),
    [
      "success-muted",
      "border-success-muted",
      "shadow-success-muted",
      "text-success-muted",
    ],
  ),
  (
    ("palette", "surface-1"),
    ["surface-1", "border-surface", "surface-default", "text-surface"],
  ),
  (("palette", "surface-4"), ["surface-4", "surface-deep"]),
];

/* Every field a themer can set, as (group, name). The pair is the key: short
   names are the point -- `menu.background` and `chrome.background` are two
   different colours and should not have to be spelled apart. */
let field_names: list((string, string)) =
  List.map(n => ("palette", n), palette)
  @ List.concat_map(
      ((group, members)) => List.map(n => (group, n), members),
      role_groups,
    );

let targets_of = (group: string, name: string): list(string) =>
  switch (List.assoc_opt((group, name), aliases)) {
  | Some(targets) => targets
  | None => [name]
  };

/* Not a colour. The theme DECLARES whether it is dark rather than leaving it
   to be inferred from the seeds, and the app forwards it to CSS so native
   controls -- scrollbars, <select> popups, the caret in text inputs -- invert
   with the theme instead of staying light on a dark editor. */
let polarity_target = "hazel-color-scheme";

/* Every CSS custom property the slide is responsible for. This, not
   `field_names`, is the output contract: it is what the stylesheets consume,
   what the tests check against, and what `theme_key` must be salted with. */
let all_targets: list(string) = [
  polarity_target,
  ...List.concat_map(((g, n)) => targets_of(g, n), field_names),
];

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
      tup_label(label("is-dark"), bool()),
    ])
  );

let entries_of = (v: Exp.t): list(Exp.t) =>
  switch (v.term) {
  | Tuple(es) => es
  | _ => []
  };

/* One pair per CSS property a labeled field is responsible for -- usually
   one, but a field standing in for a family sets several. */
let colors_of_group =
    (group_name: string, group: Exp.t): list((string, string)) =>
  List.concat_map(
    (entry: Exp.t) =>
      switch (entry.term) {
      | TupLabel(l, v) =>
        switch (l.term, C.of_exp(v)) {
        | (Label(name), Some(c)) =>
          let css = C.to_css(c);
          List.map(t => (t, css), targets_of(group_name, name));
        | _ => []
        }
      | _ => []
      },
    entries_of(group),
  );

/* Read the evaluated slide back out as the CSS custom properties to write.
   Both layers are emitted: stylesheets consume the role names, and the
   palette is published too, because it is what a saved user theme writes. */
let decoded_vars = (value: Exp.t): list((string, string)) =>
  List.concat_map(
    (section: Exp.t) =>
      switch (section.term) {
      | TupLabel(l, body) =>
        switch (l.term) {
        | Label("palette") => colors_of_group("palette", body)
        | Label("is-dark") =>
          switch (Unboxing.unbox(Atom(Bool), body)) {
          | Matches(b) => [(polarity_target, b ? "dark" : "light")]
          | _ => []
          }
        /* roles nest one level deeper: group -> entries */
        | Label("roles") =>
          List.concat_map(
            (g: Exp.t) =>
              switch (g.term) {
              | TupLabel({term: Label(gname), _}, members) =>
                colors_of_group(gname, members)
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
