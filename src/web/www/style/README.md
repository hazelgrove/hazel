# Colours in Hazel's stylesheets

Hazel's colours are not written in CSS. They are computed by a **Hazel
program** that the user can edit inside the editor, and pushed onto the
document as CSS custom properties at startup. A stylesheet's job is to say
*which role* an element takes, never what colour it is.

If you are about to write `color: #4a90d9` in a stylesheet here, stop — see
[Adding a colour](#adding-a-colour).

## The dataflow

```
hazel-programs/config/colors.hz            the Colours configuration slide.
  │                                        A Hazel program: two checkboxes,
  │                                        seed colours, and derivations.
  │  analysed against ColorScheme, then evaluated
  ▼
src/language/builtins/BuiltinsColorScheme  the SHAPE of a theme, as named
  │                                        Hazel types: ColorSeeds, ColorRamp,
  │                                        ColorOverrides, ColorPalette,
  │                                        ColorRoles, ColorScheme.
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
:root { --editor-cell: oklch(…); … }       270 custom properties, inline on
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
| `theme-generated.css` | **Generated.** Every colour the theme owns. Do not edit; run `make update-css-defaults`. |
| `variables.css` | Hand-written, and deliberately colour-free: type, timing, the z-index ladder. Plus `color-scheme`, which is a real property and so must be *used* somewhere. |
| everything else | Component stylesheets. They consume role names and define no colours. |

## The role vocabulary

The slide defines two layers, and both are published as CSS properties:

- a **palette** of 48 colours: what a scheme states outright, plus what the
  slide derives mechanically from that (`frame-2..4` off `frame-1`, `doc-2..6`
  as rotations of `doc-1`). `--ink`, `--frame-1`, `--surface-2`, `--doc-1` …
- **roles** in ten groups — `menu`, `chrome`, `editor`, `cursor`, `hole`,
  `problems`, `results`, `inspector`, `probe`, `projector` — each *derived*
  from the palette by the shared axis functions, or pointed at a palette entry
  per scheme.

A colour belongs in the palette if a scheme states or mechanically derives it,
and in a role if the shared derivation decides it. `ColorOverrides` is what is
left: the roles a scheme points somewhere itself, and it is deliberately small
(18 fields, of which two are the flags and two are the numbers the cursor plate
is pinned with). Most of what is left is genuinely per-polarity — `menu.nut` is
`info-strong` in light and `success-muted` in dark, and no axis expression
reproduces both — so shrinking it further means moving a colour, not just
rewriting it. Three fields (`frame-mark`, `frame-seam`, `frame-border`) are one
ramp step each, read by every role that wants that line weight. A role that only forwarded a
stated colour was doing no work: eleven of those (the probe fills, the
projector island and text-area colours) are palette entries instead, named for
what they are for — `probe-value`, `statics-background`, `textarea-margin` —
with the fan-out table still writing the legacy CSS names they always wrote.

Component stylesheets read roles, not the palette. The distinction matters:
change `--frame-1` and everything that means "the faintest rule" moves
together, which is not the same set as everything that happens to be that
colour today.

One role can carry several CSS properties. `ColorConfiguration.aliases` is the
fan-out table, and its rows exist because several properties often carry a
single *decision* — the slide states the decision once. Splitting one later is
a one-line change: give it its own field.

## The types are named, and the slide annotates with them

`BuiltinsColorScheme` registers its types in the builtin type context, so
`colors.hz` can write them in its own signatures:

```
let palette_of(seed: ColorSeeds, step: ColorRamp): ColorPalette = …
let theme_of(p: ColorPalette, role: ColorOverrides): ColorScheme = …
let light_roles(p: ColorPalette): ColorOverrides = …
```

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
is worth ~630 segment pieces, an ninth of the slide, and about a fifth off the
time it takes to parse.

## Four schemes from two booleans

The slide ends in a single `case` over `(dark_mode, high_contrast)`, so the two
checkboxes **compose**: there is a high-contrast dark scheme, not a dark scheme
and a separate high-contrast one. All four run through the same derivations, so
a rule like "this fill sits a third of the way from page to ink" is written
once and cannot drift between them.

Two of the emitted properties are flags rather than colours, because some
things cannot be read back off the palette:

| property | values | why |
|---|---|---|
| `--hazel-color-scheme` | `light` \| `dark` | Feeds the standard `color-scheme` property, so native scrollbars, `<select>` popups and text carets invert with the theme. |
| `--hazel-contrast` | `normal` \| `high` | Nothing in the colours tells you high contrast was *asked for*. Declared so a stylesheet can respond to it. |

Both are declared by the theme rather than inferred — low-contrast seeds could
otherwise flip polarity partway through a derivation.

To branch on either from CSS, use a style query:

```css
@container style(--hazel-contrast: high) {
  .thing { border-width: 2px; }
}
```

## Adding a colour

1. Add a field to the appropriate role group in `colors.hz`, derived from the
   palette (`at`/`wash` place a colour on the page→ink axis, which is what
   makes one definition correct in all four schemes).
2. Add it to `role_groups` in `BuiltinsColorScheme.re`, and a row to
   `ColorConfiguration.aliases` if it should write CSS properties under
   different names.
3. `make update-css-defaults` to regenerate the stylesheet.
4. `UPDATE_COLOR_GOLDEN=1 ./run_tests test 'ColorConfiguration'` and read the
   diff — an unexplained line in it is a bug.

## What is enforced

`make lint-css` (`scripts/lint_css_roles.py`):

- component stylesheets may not consume palette names directly;
- only `theme-generated.css` may declare a theme-owned colour on `:root` —
  two `:root` blocks setting one name is a race decided by `@import` order,
  which is how defaults once drifted into projector stylesheets;
- no new dangling `var()` references (there is a ratchet list of inherited
  ones, which may shrink and never grow).

`./run_tests test 'ColorConfiguration'` additionally pins that the slide
type-checks against the contract, that every scheme defines every property and
renders as valid CSS, that the four schemes are pairwise distinct, that
`theme-generated.css` is current, and — via `test/goldens/colors.tsv`, every
property × every scheme — that no colour has changed value.

**Not** enforced: nothing stops a new stylesheet rule hardcoding a literal
colour. That is a gap; the lint above catches the `:root` case only.

## Gotchas

- The slide is committed Hazel text, parsed on load. It must take the fast
  parse path; the symptom of losing it is not an error but a test suite that
  runs for minutes. Float literals longer than about six significant digits do
  not survive the print/parse round trip and will cost you that.
- A user's edited slide is persisted, and shadows the built-in one. When
  testing theme changes in the browser, clear `HAZEL_THEME` from localStorage
  and the `hazel` IndexedDB first, or you are looking at their copy.
- Colours are OKLCH. Lightness runs 0–100, chroma is unbounded in principle,
  and the palette is deliberately not gamut-limited, so some colours are
  outside sRGB and clamp when converted.
