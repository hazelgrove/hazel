module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;

/* The colour-scheme contract, as Hazel types.

   Two layers, and the difference is REUSE, not how the colour was arrived at.
   A PALETTE entry is a colour the theme is built out of: reused widely, named
   for what it is (`frame-1`, `attention-3`), and fanned out to several CSS
   properties at once. A ROLE is one specific job a colour is put to, named for
   what it is for (`cursor.pattern`, `hole.warning-edge`) and usually carrying
   one property. So the roles are the layer a themer can move independently;
   a palette entry moves all of its properties together.

   NOT the difference: whether the value is stated or computed. Every role in
   the committed slide happens to come off the shared axis functions, but that
   is the slide's style and nothing here requires it -- a user may replace any
   of them with a literal and the contract is still satisfied. What the
   contract fixes is only which fields exist and what type each holds.

   The Colors config slide (hazel-programs/config/colors.hz) is a Hazel
   program whose VALUE is the editor's theme, so the shape of that value is an
   interface: Web.ColorConfiguration fans it out to CSS custom properties, and
   the slide is analyzed against it so a slide that stops filling it goes red
   in the buffer instead of quietly half-painting the editor.

   It is written here, decomposed and named, because the names are registered
   in the builtin type context -- so the slide can annotate with them.
   `palette_of` takes `(seed: ColorSeeds, step: ColorRamp)`, `theme_of` a
   `role: ColorOverrides`, and its body is ascribed `: ColorScheme`. That puts
   each check on the record the mistake would be in, rather than leaving the
   whole-program analysis to report it as one inconsistency between two
   hundred-field products at the final `case`.

   Only the CONTRACT is here. Which CSS property carries which field is a web
   concern and stays in Web.ColorConfiguration. */

/* Every leaf is a ColorValue (BuiltinsADT.Color) unless stated otherwise. */
let color = () => var("ColorValue");

/* Every record in the contract is a labeled tuple. */
let record = (fields: list((string, Typ.t))): Typ.t =>
  prod(List.map(((n, t)) => tup_label(label(n), t), fields));

let colors = (names: list(string)): Typ.t =>
  record(List.map(n => (n, color()), names));

/* ── What a scheme states ──────────────────────────────────────────────── */

/* The colours a scheme gives outright, before anything is derived from them. */
let seeds: list(string) = [
  "none",
  "code-background",
  "ink",
  "black",
  "frame-1",
  "surface-1",
  "attention-1",
  "attention-2",
  "attention-3",
  "attention-4",
  "error-1",
  "error-2",
  "error-3",
  "type",
  "label",
  "doc-1",
  "success",
  "success-soft",
  "success-muted",
  "info",
  "info-strong",
  "textarea-margin",
  "textarea-rule",
  "textarea-rule-selected",
  "overlay-shadow",
  "error-hole",
  "divider",
  "probe-application",
  "probe-value",
  "probe-pattern",
  "probe-value-edge",
  "probe-pattern-edge",
  "probe-timeline",
  "statics-background",
  "statics-edge",
];

/* Where each ramp step sits on the L axis. Lightnesses, so Float -- and they
   invert between light and dark, which is the whole reason they are given per
   scheme rather than derived. */
let ramp: list(string) = [
  "frame-2",
  "frame-3",
  "frame-4",
  "surface-2",
  "surface-3",
  "surface-4",
];

/* The roles a scheme points somewhere other than where the shared derivation
   would put them, plus the two flags it declares and the two numbers the
   cursor plate is pinned with. */
let overrides: list((string, Typ.t)) = [
  ("dark", bool()),
  ("contrast", bool()),
  /* One ramp step, read by every role that wants that line weight -- these
     were seven fields carrying three decisions, and the seven always agreed. */
  ("frame-mark", color()),
  ("frame-seam", color()),
  ("frame-border", color()),
  ("menu-nut", color()),
  ("menu-background", color()),
  ("menu-hover", color()),
  ("chrome-background", color()),
  ("chrome-heading", color()),
  ("editor-cell", color()),
  ("cursor-tint", float()),
  ("cursor-level", float()),
  ("hole-empty", color()),
  ("editor-backpack-outline", color()),
  ("projector-textarea-text", color()),
  ("inspector-badge", color()),
  ("inspector-text", color()),
];

/* What the slide derives from the seeds: the ramps step off frame-1 and
   surface-1, pattern and type-pattern are rotations of type, doc-2..6 of
   doc-1. Listed in the order `palette_of` appends them. */
let derived: list(string) = [
  "frame-2",
  "frame-3",
  "frame-4",
  "surface-2",
  "surface-3",
  "surface-4",
  "pattern",
  "type-pattern",
  "doc-2",
  "doc-3",
  "doc-4",
  "doc-5",
  "doc-6",
];

/* ── What the app reads back ───────────────────────────────────────────── */

/* The palette layer: every colour a role can draw on, which is exactly what
   `palette_of` produces. Stated, then derived -- the order `palette_of` builds
   them in, so the slide's `palette = p` needs no rearranging.

   These are FIELD names, not CSS names. A palette colour reaches CSS only
   under the semantic names ColorConfiguration.aliases gives it; the bare
   `--ink` and its 33 siblings are not published, because nothing read them. */
let palette: list(string) = seeds @ derived;

/* Semantic roles, grouped by the part of the UI they dress. One field per
   decision a themer can make independently, named for what the colour is for
   rather than where it happens to be plumbed. A role that only forwarded a
   stated colour was doing no work -- eleven of those are palette entries
   instead. */
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
  (
    "results",
    ["divider", "reused", "reused-edge", "sweep", "pending", "pending-edge"],
  ),
  ("inspector", ["badge", "text", "separator"]),
  (
    "probe",
    [
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
  ("projector", ["textarea-text", "fold-background"]),
];

/* Declared, not inferred: low-contrast seeds could otherwise flip polarity
   partway through a derivation, and no amount of looking at the colours tells
   you whether high contrast was ASKED for. */
let polarity_field = "is-dark";
let contrast_field = "is-high-contrast";

let scheme: list((string, Typ.t)) = [
  ("palette", var("ColorPalette")),
  ("roles", var("ColorRoles")),
  (polarity_field, bool()),
  (contrast_field, bool()),
];

/* ── Registered in the builtin type context ───────────────────────────── */

/* Six names, no more: every one of these is registered in the builtin type
   context, which means it also turns up in type completion in every Hazel
   program. A type per role group was the first cut and cost more than it was
   worth -- `InspectorColors` started winning the completion for `In` ahead of
   `Int`. The groups stay anonymous inside ColorRoles; a mismatch still lands
   on the field, it just does not have a name to print. */
let type_aliases: list((string, Typ.t)) = [
  ("ColorSeeds", colors(seeds)),
  ("ColorRamp", record(List.map(n => (n, float()), ramp))),
  ("ColorOverrides", record(overrides)),
  ("ColorPalette", colors(palette)),
  (
    "ColorRoles",
    record(
      List.map(((g, members)) => (g, colors(members)), role_groups),
    ),
  ),
  ("ColorScheme", record(scheme)),
];

/* What the Colors slide is analyzed against. The alias rather than its
   expansion, so the buffer's error message names the type. */
let typ: Typ.t = var("ColorScheme");
