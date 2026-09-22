# Colors in Hazel's stylesheets

Hazel's colors are not written in CSS. They are computed by a **Hazel
program** that the user can edit inside the editor, and pushed onto the
document as CSS custom properties at startup. A stylesheet's job is to say
*which role* an element takes, never what color it is.

If you are about to write `color: #4a90d9` in a stylesheet here, stop — see
[Adding a color](#adding-a-color).

## The dataflow

```
hazel-programs/config/colors.hz            the Colors configuration slide.
  │                                        A Hazel program: two checkboxes,
  │                                        seed colors, and derivations.
  │  analysed against ColorScheme, then evaluated
  ▼
src/language/builtins/BuiltinsColorScheme  the SHAPE of a theme, as named
  │                                        Hazel types: ColorSeeds,
  │                                        ColorPalette, ColorRoles,
  │                                        ColorScheme.
  ▼
src/web/util/ColorConfiguration.re         the CSS side: a fan-out table
  │                                        mapping each field to the
  │                                        properties that carry it.
  │  decoded to (name, value) pairs
  ▼
src/web/view/ConfigurationMode.re          applies them with
  │                                        JsUtil.set_css_variable, and caches
  │                                        the result in localStorage.
  ▼
:root { --editor-cell: oklch(…); … }       236 custom properties, inline on
                                           the document element.
```

Two things run before that pipeline can:

- **`style/theme-generated.css`** holds a default for every one of those
  properties. It is generated from the slide's light scheme, so a default is
  byte-for-byte what the theme will set and nothing shifts when the theme
  lands. It covers the first frame, and a slide that fails to evaluate.
- **An inline script in `index.html`** replays the cached theme from
  `localStorage` before first paint, so the loading screen is already themed.

## Who owns what

| | |
|---|---|
| `theme-generated.css` | **Generated.** Every color the theme owns. Do not edit; run `make update-css-defaults`. |
| `variables.css` | Hand-written, and deliberately color-free: type, timing, the z-index ladder. Plus `color-scheme`, which is a real property and so must be *used* somewhere. |
| everything else | Component stylesheets. They consume role names and define no colors. |
| `palette.html` | A standalone swatch page with its own hardcoded `:root`. It does not read the theme, so it drifts; regenerate it by hand if you care. |

## The two layers

The slide's value has two sections, and the difference between them is
**reuse, not how the color was arrived at**.

- a **palette** of 48 colors — the colors the theme is built *out of*. Named
  for what they are (`frame-1`, `attention-3`, `error-2`), reused widely, and
  each one fanned out to several CSS properties at once. `palette.ink` alone
  drives `--text-default`, `--border-inverse`, `--code-text`, `--token-exp` and
  five more.
- **roles** in ten groups — `menu`, `chrome`, `editor`, `cursor`, `hole`,
  `problems`, `results`, `inspector`, `probe`, `projector`. One field per
  decision, named for what the color is *for* (`cursor.pattern`,
  `hole.warning-edge`, `chrome.table-row-hover`), and usually carrying a single
  property.

Between them they write 236 properties: 163 off the palette, 71 off the roles,
plus two flags.

**What the split is not:** stated versus computed. Every role in the committed
slide happens to be derived — `wash(p.pattern, 0.11, 0.2)` and friends — but
that is how this slide is written, not what the contract requires. A user is
free to delete the axis functions and give every role a literal; it will still
typecheck and still evaluate to a theme. The contract fixes only which fields
exist and what type each holds.

**What the split does mean, practically:** the roles are the layer you can move
independently. A user editing the slide can set any single role and affect
exactly the properties that role carries. A palette entry is a bundle — set
`palette.ink` and all nine of its properties move together, and there is no way
from the slide to make `--code-text` differ from `--text-default`. Splitting one
out is a two-file change (a new field in `BuiltinsColorScheme`, a row in
`ColorConfiguration.aliases`) and a recompile, not a slide edit.

That bundling is a deliberate readability compromise, and it is the least
principled part of the design: `palette.ink`'s nine properties are body text,
an inverted border, code text, three token colors, an inverted surface and a
menu item — several purposes that happen to share a color today, which is
exactly the accidental grouping the role layer exists to avoid. One field per
property would be ~240 fields; the ~100 we have was judged easier for a themer
to read. The constraint has not bitten yet, and the fix when it does is to
promote the property you need into a role field of its own rather than to
split every bundle pre-emptively.

**There is no `--ink`.** A palette color reaches CSS only under the semantic
names the fan-out gives it. The 34 bare palette names used to be published too
and were read by nothing — no stylesheet, no OCaml, no script, and the only
references left in the tree were commented out. Dropping them took the output
from 270 properties to 236 and means component stylesheets *cannot* consume a
palette name rather than merely being told not to. The palette is still a
first-class layer in the slide and a type in `BuiltinsColorScheme`; it is just
not a CSS namespace.

Two smaller notes on what lives where. `ColorOverrides` — declared in the
slide, not the builtins — is the record a scheme uses to point a role somewhere
itself, and it is deliberately small: 18 fields,
of which two are the flags and two are the numbers the cursor plate is pinned
with. Most of what is left is genuinely per-polarity (`menu.nut` is
`info-strong` in light and `success-muted` in dark, and no axis expression
reproduces both), so shrinking it further means moving a color rather than
rewriting one; three of its fields (`frame-mark`, `frame-seam`, `frame-border`)
are a single ramp step each, read by every role that wants that line weight.
And a role that only forwarded a stated color was doing no work: eleven of
those — the probe fills, the projector island and text-area colors — are
palette entries instead, named for what they are for (`probe-value`,
`statics-background`, `textarea-margin`), with the fan-out still writing the
legacy CSS names they always wrote.

## The types are named, and the slide annotates with them

`BuiltinsColorScheme` registers its types in the builtin type context, so
`colors.hz` can write them in its own signatures:

```
let palette_of(seed: ColorSeeds, step: ColorRamp): ColorPalette = …
let theme_of(p: ColorPalette, role: ColorOverrides): ColorScheme = …
let light_roles(p: ColorPalette): ColorOverrides = …
```

Only four of those are builtin — `ColorSeeds`, `ColorPalette`, `ColorRoles`,
`ColorScheme` — because only those describe the theme, which the app has to
agree with. `ColorRamp` and `ColorOverrides` are seams between the slide's own
two functions and never leave the file, so the slide declares them itself with
`type ColorRamp = (…) in`. They check exactly as well either way; it is about
where the definition belongs, and it keeps two records the app has no interest
in out of the builtin type namespace (where every name also shows up in type
completion in every Hazel program).

`ColorPalette` is one type doing one job: `palette_of` produces it, the theme
publishes it unchanged (`palette = p`), and `ColorConfiguration.palette` is
literally `seeds @ derived`, so the published list cannot drift from what the
slide builds.

That moves the check to where the mistake is. Without them the only analysis is
the whole-program one, which lands on the final `case` — one inconsistency
between two hundred-field products, which is how a rename once produced
thirteen errors that named nothing. Ascribed, a scheme's seed and override
records are checked at the record, and the theme at the record that builds it.

Every one is load-bearing: point any at the wrong type and the slide reports
errors; as committed it reports none. `./hazel analyze hazel-programs/config/
colors.hz` is the quickest way to check that — it names the offending term, and
it is much less work than driving statics from a test.

`p: ColorPalette` is the annotation that matters most. Without it `p` is an
unannotated parameter, every `p.\`field\`` in `theme_of` reads off an unknown
type, and a misspelled palette name is not a static error at all — it silently
yields a slide that evaluates to no theme, which looks exactly like the
stylesheet defaults. It is nearly free, because `palette_of` declares the same
type on the way out, so the check at each call site compares two identical
names rather than two 48-field records.

The slide also leans on tuple extension (`...`) to avoid restating records:
`palette_of` is `seed ... (the derivations)` rather than 35 lines of
`x = seed.x`, and each polarity has one role map that its high-contrast variant
extends with only what it changes. Together with dropping the nested folds that
is worth ~630 segment pieces, a ninth of the slide, and about a fifth off the
time it takes to parse.

## Four schemes from two booleans

The slide ends in a single `case` over `(dark_mode, high_contrast)`, so the two
checkboxes **compose**: there is a high-contrast dark scheme, not a dark scheme
and a separate high-contrast one. All four run through the same derivations, so
a rule like "this fill sits a third of the way from page to ink" is written
once and cannot drift between them.

Two of the emitted properties are flags rather than colors, because some
things cannot be read back off the palette:

| property | values | why |
|---|---|---|
| `--hazel-color-scheme` | `light` \| `dark` | Feeds the standard `color-scheme` property, so native scrollbars, `<select>` popups and text carets invert with the theme. |
| `--hazel-contrast` | `normal` \| `high` | Nothing in the colors tells you high contrast was *asked for*. Declared so a stylesheet can respond to it. |

Both are declared by the theme rather than inferred — low-contrast seeds could
otherwise flip polarity partway through a derivation.

To branch on either from CSS, use a style query:

```css
@container style(--hazel-contrast: high) {
  .thing { border-width: 2px; }
}
```

## Adding a color

1. Add a field to the appropriate role group in `colors.hz`. Deriving it from
   the palette is the convention rather than a rule — `at`/`wash` place a
   color on the page→ink axis, which is what makes one definition correct in
   all four schemes, where a per-scheme literal has to be got right four
   times.
2. Add it to `role_groups` in `BuiltinsColorScheme.re`, and a row to
   `ColorConfiguration.aliases` if it should write CSS properties under
   different names.
3. `make update-css-defaults` to regenerate the stylesheet.
4. `UPDATE_COLOR_GOLDEN=1 ./run_tests test 'ColorConfiguration'` and read the
   diff — an unexplained line in it is a bug.

## What is enforced

`make lint-css` (`scripts/lint_css_roles.py`):

- component stylesheets may not consume palette names directly (belt and
  braces now that those names are not published — a `var(--ink)` would also
  trip the dangling check below, but with a worse message);
- only `theme-generated.css` may declare a theme-owned color on `:root` —
  two `:root` blocks setting one name is a race decided by `@import` order,
  which is how defaults once drifted into projector stylesheets;
- no new dangling `var()` references (there is a ratchet list of inherited
  ones, which may shrink and never grow).

`./run_tests test 'ColorConfiguration'` additionally pins that the slide
type-checks against the contract, that every scheme defines every property and
renders as valid CSS, that the four schemes are pairwise distinct, that
`theme-generated.css` is current, and — via `test/goldens/colors.tsv`, every
property × every scheme — that no color has changed value.

**Not** enforced: nothing stops a new stylesheet rule hardcoding a literal
color. That is a gap; the lint above catches the `:root` case only.

## Gotchas

- The slide is committed Hazel text, parsed on load. It must take the fast
  parse path; the symptom of losing it is not an error but a test suite that
  runs for minutes. Float literals longer than about six significant digits do
  not survive the print/parse round trip and will cost you that.
- A user's edited slide is persisted, and shadows the built-in one. When
  testing theme changes in the browser, clear `HAZEL_THEME` from localStorage
  and the `hazel` IndexedDB first, or you are looking at their copy. To avoid
  destroying someone's saved slide, serve the same build on a second port —
  storage is per origin, so a fresh port loads the built-in slide and leaves
  theirs alone.
- `make test-quick` and `./run_tests` disagree about which
  `theme-generated.css` they check: `theme_css_path()` walks up from the cwd,
  and dune's runtest rule finds the `_build` copy. After
  `make update-css-defaults`, build before trusting that check.
- Colors are OKLCH. Lightness runs 0–100, chroma is unbounded in principle,
  and the palette is deliberately not gamut-limited, so some colors are
  outside sRGB and clamp when converted.
