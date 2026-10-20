# Charts

Charts in Hazel are not a language feature. There is no `Chart` type, no chart
projector, and no charting library bundled into the implementation. A chart is
an ordinary HTML value produced by ordinary Hazel functions, and the charting
vocabulary — scales, ticks, axes, marks, legends, palettes — is Hazel source in
`hazel-programs/charts/charts.hz` that anyone can read, fork, or replace.

This doc covers what makes that possible, what the library provides, and where
to look when a chart doesn't draw.

## The three layers

| Layer | Where | Chart-aware? |
|---|---|---|
| HTML/SVG value → real DOM | `src/haz3lcore/projectors/HazelDOM.re` | no |
| evaluated value → rendered view | `src/haz3lcore/projectors/implementations/HtmlRenderer.re` | no |
| scales, axes, marks, legends | `hazel-programs/charts/charts.hz` | **yes — in Hazel** |

Only the third layer knows what a chart is, and it is not written in OCaml.
Adding a stacked bar, a log scale, an axis title or an annotation is a change to
Hazel source, in the editor, by whoever wants it.

## Drawing

`HTML`'s generic escape hatch is `Node(tag, attrs, children)`. Tags in
`HazelDOM.svg_tags` (`svg`, `g`, `path`, `circle`, `rect`, `text`, gradients,
filters, …) are created in the SVG namespace — `createElement` on them would
yield an inert `HTMLUnknownElement`. SVG attributes go through `Attr`'s generic
`Create(name, value)`, since `Attr.create` is `setAttribute` and so is
namespace-safe:

```
Node("svg", [Create("viewBox", "0 0 200 100")], [
  Node("circle", [Create("cx", "50"), Create("cy", "50"),
                  Create("r", "20"), Create("fill", "teal")], [
    Node("title", [], [Text("a tooltip")])
  ])
])
```

Numbers reach SVG as strings, so `to_fixed(f, digits)` does the formatting.
A `<title>` nested in a shape is the browser's native tooltip — the charts use
it rather than building one.

Two rough edges worth knowing: `a`, `script` and `style` are deliberately *not*
SVG-namespaced (they stay HTML), so SVG links are not expressible; and HTML
constructors like `Div` inside an `svg` subtree render in the HTML namespace,
which is only legal inside `foreignObject`.

## Viewing a chart

A chart is a *computed* value, so the two projectors that draw HTML split by
what they are handed:

- **`^^probe_html`** (`HtmlRenderer`, a rich probe) renders the **evaluated
  value** of any expression. HTML is drawn read-only, and an `(init, update, view, subs)` 4-tuple is run
  as a live app you can click, keyed on the probe's id. This is what a computed
  chart wants: `Chart.bar(sales)` is an application, and its HTML only exists
  after evaluation. A probed app restarts from `init` on reload, since a rich
  probe has no quiet channel to checkpoint through.

  The open rich probe can be moved into the **Projectors panel** with the arrow
  beside its close button; the probe itself stays on the code. Placement lives
  in `ProbeProj`, not in the renderer, so every rich probe docks — the table
  renderer included.

### Projector or probe

They render the same values. `HTMLProj`'s `init` accepts a bare `Ap` or `Var`
and its first dispatch arm runs whatever the live value turns out to be, so
`^^html(makeApp(data))` runs the app just as `^^probe_html` does. What differs
is what each one *is*:

|  | `^^html` (projector) | `^^probe_html` (rich probe) |
|---|---|---|
| Relationship to the code | **replaces** it | **additive** — the source stays |
| Values it can show | one: the latest sample by `seq` | **every sample**, chosen with the navigator |
| Editing the source | only by removing the projector | by the rendering *or* by typing |

The second row is the one with no workaround. A probe on an expression inside a
loop or a recursion records a sample per iteration, so the whole run is
inspectable rather than just its answer, navigable with sample focus. A
projector there would show the last sample only, and would have hidden the
expression to do it.

The third row is the probe's other advantage, and `HtmlRenderer` takes it by
**rewriting the source**. Clicking a handler pipes the probed html into it —
`H` becomes `H |> f`, in reverse application over newlines, the way
`TableRenderer`'s column operations commit — and the program's own pipeline
evaluates that, with statics, elaboration, and the scope `H` already sits in.

Evaluating the transform inside the renderer instead does not work, and is
worth recording because it looks like it should. A probe has no elaborated form
of its syntax (`RefractorView.mk_data` passes `~elaborated=None`) and no
environment to resolve a free handler in, so `Evaluator.evaluate` leaves the
application stuck part-way — and a stuck term comes back as `Ok`, not `Error`.
Splicing that writes a half-reduced `case` over the program. Handing the
application back as *syntax* puts the evaluation somewhere that has all three,
which is the same reason `TableRenderer` rewrites syntax rather than evaluating.

Two properties follow from rewriting rather than evaluating:

- **The edit lands on the definition.** If the html sits inside a function, the
  rewrite is to that function's body, so every call renders the transformed
  version — not just the invocation that was clicked.
- **The handler is spliced by name.** The evaluator records the binding name on
  a `Fun`, so `let bump = fun …` commits as `bump(H)` rather than inlining the
  body, and editing `bump` later changes what every committed call does. The
  name is used only when the probe site actually binds it; a handler defined in
  an inner scope and returned outward is inlined instead, since splicing its
  name there would write an unbound variable.

Each click appends another stage rather than nesting, so a run of edits reads as
a pipeline — which is an accurate record of what was asked for, and prunes a line
at a time. **Charts / Calculator** is built on exactly this: its keys are named
`Html -> Html` functions, so a session accumulates as

```
calc(0)
  |> plusOne
  |> double
  |> square
```

Naming matters here: an operation built by partial application has no recorded
name to commit, so it would be inlined at the use site instead.

A *fixed* app also cannot serve more than one schema: a literal 4-tuple's `view`
fixes one row type, where an app built by a polymorphic function does not. That
is a property of computed apps rather than of the probe, so both surfaces show
it equally.

**Known limitation.** The AppStore is keyed by the probe's id, and rebinding
preserves the old model whenever the new `view` still evaluates it. One probe on
an expression that yields *several* app samples therefore carries one sample's
model into the next as focus moves. Give each app its own probe.
- **`^^html`** (`HTMLProj`) renders the projector's **syntax**, which is what
  lets it commit edits back: written-out HTML is editable in place, and an
  `(init, update, view, subs)` 4-tuple runs as an app. See `docs/mvu.md`.

Both are also reachable without typing a token: right-click for the projector
menu, or open a `^^probe` and pick "View as html" from the sample menu.

### In the evaluation output

**Nut menu → HTML** turns on `project_html`, and a result that is an HTML value
renders as DOM in the evaluation output instead of printing as a constructor
tree. It mirrors the existing **Tables** toggle and is off by default.

The gate is the constructor's *type*, not its name (`MvuShape.is_html_typed`):
statics compacts an unshadowed builtin alias to `Var("HTML")`, so a program that
defines its own `type HTML = + Div(Int)` is left alone. And only the outermost
node is projected — every child of an element is itself an HTML constructor
application, so projecting each one would put a projector around every element
in the tree.

## The library

`hazel-programs/charts/charts.hz` is three modules. Hazel has no cross-file
imports, so the module block is the unit of distribution — copy it into a
program, or start from the **Charts** documentation slide, which is this file.

**`Svg`** — thin wrappers over `Node`/`Create`: `root`, `g` (translate),
`rect`, `circle`, `seg`, `path`, `text`, `tip`, plus `f2`/`at` for formatting
and `num` for axis labels (which drops decimals on whole numbers).

**`Scale`** — `linear(d0, d1, r0, r1, x)` maps a domain onto a range and pins a
degenerate domain to the range midpoint instead of dividing by zero;
`band(n, i, r0, r1, pad)` gives the `(start, width)` of one of `n` bands;
`step(span, count)` picks the 1/2/5 × 10ᵏ nearest `span/count`;
`nice(lo, hi, count)` rounds a domain outward to whole steps and returns
`(lo, hi, step)`; `ticks(lo, hi, step)` enumerates them inclusively.

**`Chart`** — the five kinds, each taking the labeled-tuple data that the table
projector already recognizes:

| Function | Data |
|---|---|
| `Chart.bar` | `[(label=String, value=Float)]` |
| `Chart.groupedBar` | `[(name=String, data=[(label=String, value=Float)])]` |
| `Chart.line` | `[(x=Float, y=Float)]` |
| `Chart.scatter` | `[(x=Float, y=Float)]` |
| `Chart.pie` | `[(label=String, value=Float)]` |

Series colors are `var(--chart-1)` … `var(--chart-8)`, defined in
`src/web/www/style/variables.css`, so a chart follows the editor theme with no
color logic in the library. The `chart-*` classes it puts on its elements are
styled in `src/web/www/style/projectors/proj-html-probe.css`; a program that
wants a different look can use its own class names, or inline `Style(...)`.

### Behaviours worth preserving

- **Grouped series align by label, not position.** The category axis is the
  union of every series' labels in first-seen order, and each series looks its
  own values up by label. A series with no value at a category leaves a gap
  rather than shifting its remaining bars into the next category's slot.
- **Empty data draws "no data"**, not an empty pair of axes.
- **A pie slice covering the whole circle is drawn as a circle**, because an arc
  with coincident endpoints collapses to nothing.
- **Negative pie values are dropped**, not drawn as zero-width wedges.
- **Bar domains always include zero**, so bar length stays proportional to value.

`test/Test_Charts.re` pins all of these against the value tree the library
produces. It reads the library out of the shipped slide and cuts it at the
gallery header, so the tests cannot drift from what users read.

## Charts as inputs

An HTML value can carry event handlers, so a chart can be an input as well as an
output. The **Charts / Linked Views** slide (`hazel-programs/charts/linked.hz`)
is an app whose bars are dragged to set their values, with a pie chart derived
from the same model beside them. It uses the `...At` handlers (`OnMouseDownAt`,
`OnMouseMoveAt`, `OnMouseUpAt`), which report the pointer relative to the
element they are attached to; keeping `Width`/`Height` equal to the `viewBox`
size makes those pixels the viewBox coordinates directly.

**Derive the second chart inside `view`.** An app's model lives in the runtime,
not in the syntax, so destructuring the 4-tuple to get at it —
`let (model, _, _, _) = ^^html((init, update, view, subs))` — binds the `init`
component, which is the *starting* data and never moves. A chart built from that
is static. Deriving inside `view` is what makes a linked chart track the model.

Shape helpers like `Svg.rect` take a fixed attribute list, so marks that carry
event handlers are built from raw `Node` instead. That is the seam to widen if
interactive charts become common.

## Troubleshooting

- **The chart renders as code.** `HazelDOM` matches constructor names as
  strings; a typo in an element or attribute name falls back to a read-only
  syntax view of that subterm rather than failing loudly. Look for the part of
  the drawing that came out as text.
- **An SVG element renders as nothing.** Its tag is probably not in
  `HazelDOM.svg_tags`, so it was created in the HTML namespace.
- **`^^probe_html` says nothing is there.** The expression has no probe sample
  yet — it hasn't evaluated, or dynamics are off.
- **Numbers come out as `0.10000000000001`.** Use `to_fixed`; SVG attributes are
  strings and nothing rounds them for you.
