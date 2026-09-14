# Illustration Style

Last updated 2026-09-13

**Status: a draft reading, not a ruling.** This is an attempt to write down the
house style that Hazel's artwork already has, so that new artwork can be made
to match it instead of each piece re-inventing a look. The style was not
designed in this document — it was inferred from work that already exists,
most of it Andrew's. It is posted here to be corrected by the people whose
style it is.

## Where this reading comes from

Three sources, in descending order of how much they shaped it:

1. **[Week in Hazel](https://andrewblinn.com/hazel/weekly/)** — the covers for
   issues 001, −060 and −100, and the interior plates. This is the fullest
   statement of the style that exists.
2. **The icon set** — `src/web/app/common/Icons.re`, especially the dense
   Noun-Project silhouettes (`microscope2` on the probes tab, `new_buffer`,
   `rename`).
3. **The app palette** — `src/web/www/style/variables.css`.

The happy accident that makes a single style possible: Hazel's UI ground is
already the right colour. `--SAND` is `oklch(99% 0.012 90)` and `--ui-bkg`
resolves to `--T1`, `oklch(97% 0.025 90)` — a warm, pale cream within a hair
of the paper the Weekly covers are printed on. Artwork in the Weekly idiom
does not have to be fitted to the app; it already sits on the same paper.

## The style in one sentence

A nineteenth-century engraved plate: ink line and hatching on warm paper,
drawn with a naturalist's patience, with colour used sparingly enough that
each appearance of it means something.

## Palette

Spot colour is the discipline here. The Weekly covers are essentially
monochrome plates with two or three colours admitted per image — never a
full spectrum, and never a colour that is only decorative.

| Role | Value | Notes |
| --- | --- | --- |
| Paper | `#f2e8d4` | Sits just under `--T1`, so a plate reads as a figure on the panel rather than a hole in it |
| Ink | `#2b2117` | Warm near-black. Never pure `#000` |
| Leather / wood | `#8a6740`, shadow `#33230f` | The Weekly's furniture and bark |
| Steel / stone | `#cbc2ae`, shadow `#3f3a2e` | Instruments, machinery |
| Ochre | `#dba828`–`#e0aa1e`, shadow `#8a6214` | Brass, lamplight, mustard |
| Oxblood | `#a8392e` | The Weekly's accent red — issue badges, single small marks |
| Forest | `#2f4739` | Foliage, lampshades. Mostly absent from small work |

Two rules that matter more than the exact values:

- **No pure black and no pure white.** Highlights are `#fffaf0` at low
  opacity; the darkest ink is still brown.
- **Red is rationed.** On the covers it appears once or twice per image and
  the eye goes straight to it. It is the most expensive colour in the set;
  spend it on the one thing that should be noticed.

## Technique

**Tone comes from hatching, not from fills.** This is the single largest
difference between work that looks like the Weekly and work that looks like
clip art. A shape with a flat fill and an outline reads as vector art no
matter how good the silhouette is. The same shape with a crescent of
parallel lines along the shaded limb reads as engraving.

In SVG the economical way to get this is a `<pattern>` of parallel lines
plus a `<clipPath>` of the region:

- one pattern at ~2.4 unit spacing for ordinary shade,
- a denser one at ~1.5 for deep shade,
- a crossed one for the darkest passages,
- optionally a light-ink pattern, rotated the other way, for highlights on
  dark material.

**Shade in crescents, never in rectangles.** Filling a clipped rectangle with
hatch leaves a straight edge running through a curved object, and the eye
catches it immediately. Clip to the object and fill a *circle offset toward
the light-away side* instead; the intersection is a crescent with a curved
boundary, which is what an engraver would have cut.

**No gradients, no blurs, no drop shadows.** Cast shadows are a few short
parallel strokes on the ground, thinning as they recede.

**Repetition is cheap and looks expensive.** `stroke-dasharray` with
`pathLength` gives evenly divided detail around a circle for one element —
a knurled bezel, a minute track, a row of rivets. Fine repeated detail is
very characteristic of the source style and costs almost nothing.

## Typography in artwork

- **Captions and labels: small monospace caps, letter-spaced, at ~70%
  opacity.** The Weekly's chrome is built out of this — `A SMALL MAGAZINE
  ABOUT A VERY LIVE LANGUAGE`, `PILOT / ABOUT 15 MINUTES`. In-app, use
  `Source Code Pro`, which the app already ships.
- **Lettering inside the image: serif.** It is a nineteenth-century plate;
  the plate's own writing is set, not lettered.
- Text small enough to be a texture rather than a message at normal size is
  a feature, not a bug — see the next section.

## Composition

The covers frame the illustration and caption it. Smaller work can borrow
the same structure:

- a **plate**: a paper rectangle with a ruled border and a lighter inner
  rule,
- a **ground rule**: a single horizontal line the objects stand on, which
  also gives cast shadows something to fall across,
- a **caption** in small monospace caps along the bottom.

The frame does real work beyond decoration. It makes the artwork
self-grounding: the paper travels with the image, so the piece does not
depend on the panel behind it being any particular colour. That matters for
anything that has to survive a theme change.

## Scale: draw for two distances

The Weekly covers reward being looked at twice, and small in-app artwork
should too. Practically this means designing at two scales at once:

- **At panel size** (roughly 140 px wide for an explanation panel), only the
  silhouette, the largest masses and the spot colour survive. Those have to
  carry the whole idea on their own.
- **On closer inspection**, the fine detail pays the viewer back: hatching,
  a signature on a dial, a joke in a caption, an object that turns out to be
  labelled.

Detail that is illegible at panel size is worth drawing anyway, provided the
image is not *relying* on it. Detail that the image relies on has to be
promoted to the silhouette.

## Technical constraints for in-app artwork

These are properties of Hazel's viewer rather than the style, but they
determine how artwork has to be built:

- **Build SVG as `Virtual_dom` nodes, not as an image file and not as a
  markdown image.** The explanation panel's markdown translator has no
  `Omd.Image` case, so an image in markdown is silently dropped. See
  `MustardWatch.re` for the shape: a small `svg` helper over
  `Node.create_svg`, and the art as a `Node.t`.
- **`dune build @fmt` is CI's first step.** Run `dune build @fmt
  --auto-promote` before pushing; a formatting failure stops the build
  before anything compiles.
- **Namespace every `id`.** `pattern` and `clipPath` ids are global to the
  document. Prefix them per-artwork (`mw-h1`, `mw-ring`, …).
- **Draw glyphs that fonts may not have.** `⅋` (U+214B) and `⊸` (U+22B8) are
  missing from many font stacks and will render as tofu. Either draw them as
  geometry, or — for `⅋` specifically — set an ampersand and rotate it 180°,
  which is what a turned ampersand is.
- **Don't assume the page is cream.** Panels change colour; see the note on
  the plate above.

## Worked example

The `mustardWatch` easter egg in the explanation panel — an engraved plate of
a wristwatch dispensing mustard, its dial marked with linear-logic
connectives — is being redrawn against this reading. It is on the
`experimental-lang-integration` branch rather than in this PR, so that this
document can be reviewed on its own.

## Open questions

For Andrew, and for anyone else with a claim on the style:

1. **Is the frame right for small work?** The covers are framed because they
   are covers. A 140 px figure in a side panel might be better unframed and
   floating on the panel's own ground — at the cost of the theme-independence
   the plate buys.
2. **How far does the green go?** It is unmistakable on the covers and I have
   not found a use for it at small sizes. Is it foliage-only, or is it a
   general third colour?
3. **Is the caption in character?** Small mono caps under an in-app figure
   reads as Weekly to me, but it may read as pastiche when it is not actually
   in the magazine.
4. **Should this converge with the icon set, or stay separate?** The icons are
   flat single-colour silhouettes and look nothing like the plates, which is
   arguably correct — icons are UI, plates are pictures. Worth saying out loud
   either way.
5. **Is the palette above actually the palette?** The values are sampled by
   eye from rendered covers, not taken from a source. If a real palette
   exists, this table should be replaced by it.
