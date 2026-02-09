# Self-Modifying HTML: Concept, Examples, and the RNA World

## The Model

The inline HTML projector's "legacy" mode: event handlers are `Html -> Html` functions. The handler receives the current HTML tree and returns the replacement. No separate model, no message type, no update function. **The view IS the state.**

Every interaction is a tree rewrite. Each handler is a rewrite rule applied to the UI. The program is its own data.


## Examples

### Working with the current projector

Place the projector on the outermost expression (must be an HTML constructor).

#### 1. Static HTML

Baseline: renders, no interactivity.

```
Div(
  [Style([
    ("padding", "20px"),
    ("background", "#e8f5e9"),
    ("border-radius", "8px")
  ])],
  [
    H2([], [Text("Hello from Hazel!")]),
    P([], [Text("This is rendered HTML.")])
  ]
)
```

#### 2. One-shot

The handler ignores the current tree and returns static HTML. After one click, it becomes `Text("Clicked!")` with no further handler. The interaction is consumed.

```
Button(
  [
    OnClick(fun _ -> Text("Clicked!")),
    Style([
      ("padding", "12px 24px"),
      ("font-size", "16px"),
      ("cursor", "pointer")
    ])
  ],
  [Text("Click me")]
)
```

#### 3. Two-shot

Each handler manually embeds the next state. Finite by construction.

```
Button(
  [
    OnClick(fun _ ->
      Button(
        [
          OnClick(fun _ -> Text("Done!")),
          Style([
            ("padding", "12px 24px"),
            ("background", "#4CAF50"),
            ("color", "white"),
            ("border", "none"),
            ("cursor", "pointer")
          ])
        ],
        [Text("Click again!")]
      )
    ),
    Style([
      ("padding", "12px 24px"),
      ("cursor", "pointer")
    ])
  ],
  [Text("Click me")]
)
```

#### 4. Toggle via pattern matching (infinite loop)

The handler pattern-matches on the current tree to determine state. `attrs` captures the handler itself, so the cycle repeats forever.

**Note:** Depends on Hazel supporting list patterns like `[Text("OFF")]` in case expressions. Needs testing.

```
Button(
  [
    OnClick(fun html ->
      case html
      | Button(attrs, [Text("OFF")]) =>
          Button(attrs, [Text("ON")])
      | Button(attrs, _) =>
          Button(attrs, [Text("OFF")])
      end
    ),
    Style([
      ("padding", "12px 24px"),
      ("font-size", "16px"),
      ("cursor", "pointer")
    ])
  ],
  [Text("OFF")]
)
```

Key insight: the handler function doesn't change between states. It persists because `attrs` captures it along with the styles.


### Broken: external variable capture

This example exposes a current implementation limitation:

```
let x = "ON" in
Button(
  [
    OnClick(fun html ->
      case html
      | Button(attrs, [Text("OFF")]) =>
          Button(attrs, [Text(x)])
      | Button(attrs, _) =>
          Button(attrs, [Text("OFF")])
      end
    ),
    Style([
      ("padding", "12px 24px"),
      ("font-size", "16px"),
      ("cursor", "pointer")
    ])
  ],
  [Text("OFF")]
)
```

**What happens:** Clicking the button displays the raw syntax `x` instead of `"ON"`.

**Why:** The inline projector operates on *syntax*, not on the evaluated result from the full Hazel pipeline. HTMLProj.re extracts the handler from `info.syntax` — the raw AST — rather than from `info.dynamics` (which contains the fully-evaluated expression with Closures that capture the lexical environment). So the handler is a bare `Fun` node with no captured environment. When it fires and `evaluate` runs with `Builtins.env_init`, `x` is unbound.

The evaluated result with proper Closures exists and is available via `info.dynamics` — the projector just never looks at it. This is fixable: HTMLProj could use the evaluated expression (where the handler would be `Closure({x: "ON", ...}, fun html -> ...)`) instead of the raw syntax.

This matters for the broader conceptual picture: the boundary between the Hazel environment and the projector's "independent world" is currently hard. Closures should bridge it — carrying the external environment into the projector — but the inline projector bypasses the evaluation that creates them.


### Not yet possible: pass-self counter

The pass-self pattern: the view function takes itself as a parameter, so handlers can reconstruct the view with new state. State lives in closures, not in the tree.

```
let view = fun (self, n) ->
  Div(
    [Style([
      ("display", "flex"),
      ("align-items", "center"),
      ("gap", "10px")
    ])],
    [
      Button(
        [
          OnClick(fun _ -> self(self, n - 1)),
          Style([("padding", "8px 16px")])
        ],
        [Text("-")]
      ),
      Text(string_of_int(n)),
      Button(
        [
          OnClick(fun _ -> self(self, n + 1)),
          Style([("padding", "8px 16px")])
        ],
        [Text("+")]
      )
    ]
  )
in
view(view, 0)
```

**Can't work yet** for two reasons:

1. `init` only accepts top-level HTML constructors. `view(view, 0)` is a function application that *evaluates to* HTML but isn't syntactically an HTML constructor.
2. Even if `init` were relaxed, the environment limitation above would prevent `self` and `n` from being captured in the handler closures.

Both are fixable. See "Implementation Issues" below.


## Where State Lives

### Structural state (examples 2–4)

The state is encoded in the HTML tree itself. The handler inspects the tree via pattern matching to determine the current state, then reconstructs with the new state. Everything is visible — there's no hidden information.

### Closure state (pass-self)

The state lives in handler closures. The tree doesn't encode the state. The view function is parameterized over abstract state (`n`), and each handler closure captures the current value.

### The real difference

At first glance, this looks like an expressivity gap: structural state requires the model to be "serializable" into the HTML tree, while closure state can carry anything.

But this gap is softer than it appears. Closures are data too — an environment (name → value mappings) and a body (an AST). Both are tree-structured. Both *could* be represented as HTML structure. There's no principled barrier to encoding a closure as:

```
Div([Class("closure")], [
  Div([Class("env")], [
    Div([Class("binding")], [Text("n"), Text("42")])
  ]),
  Div([Class("body")], [... code as HTML ...])
])
```

So the structural approach isn't less expressive in principle — it's less *ergonomic*. You can encode anything as tree structure if you're willing to go deep enough. The question is whether you'd want to. (More on this in "The RNA World" below.)


## Relationship to MVU

### Definitions

An MVU app specifies:

```
M      : type
init   : M
view   : M → Html(Msg)
update : (Msg, M) → M
```

A self-modifying app specifies:

```
h : Html(Html)
```

That's it. Just a value.

### MVU → structural self-modifying

Requires `encode : M → Html` and `decode : Html → M`:

```
translate(init, view, update) =
  let rec build(m) =
    view(m)
    |> replace_each_handler(fun msg →
         fun html →
           let m = decode(html) in
           build(update(msg, m))
       )
    |> inject_encoding(encode(m))
  in
  build(init)
```

Every handler decodes the model from the tree, applies the MVU update, and rebuilds. Works if encode/decode exist — which they do for any type in principle, but are unwieldy for complex types.

### MVU → pass-self

No encode/decode needed:

```
translate(init, view, update) =
  let rec build(self, m) =
    view(m)
    |> replace_each_handler(fun msg →
         fun _ → self(self, update(msg, m))
       )
  in
  build(build, init)
```

State lives in closures. Works for any model type.

### Self-modifying → MVU

Trivial: `M = Html(Html)`, `view = id`, `init = h`, `Msg = event routing`, `update(msg, html) = dispatch to the relevant handler`.

### The hierarchy

```
structural self-mod  →  pass-self  →  MVU  →  MVU + Msg
       (most direct)                          (most structured)
```

Each arrow adds a layer of indirection and buys something (separation of concerns, testability, named transitions, replayability) while losing immediacy. Self-modifying HTML is the *free* or *universal* MVU — it's what you get when you refuse to commit to a separate model type, because the model IS the view.


## What's Interesting

### Handlers as rewrite rules

Each event handler is a structural rewrite rule: "when the tree looks like THIS, replace it with THAT." The UI is a term rewriting system. Clicking a button applies a rewrite rule. The system evolves by successive rewrites. You could study confluence, termination, etc.

### Direct manipulation taken literally

Hazel's philosophy is direct manipulation of the program artifact. Self-modifying HTML takes this *literally*: you interact with the artifact, and the artifact rewrites itself. No intermediary. The handler talks directly about the thing you see.

And there's a deeper connection: in Hazel, the inline HTML projector renders a Hazel expression as DOM. When the handler fires, it *rewrites the underlying Hazel expression*. That's what the Hazel editor does — lets you directly manipulate Hazel expressions. So the self-modifying HTML projector is, in a very literal sense, a domain-specific Hazel editor. The user is editing Hazel ASTs through a custom visual interface that happens to look like a web app.

This is the most Hazel-native thing possible. Projectors exist to provide alternative interfaces for manipulating program structure. The HTML projector is a projector whose alternative interface is an interactive web application.

### What the message layer buys you

MVU's message type is an *indirection*. Instead of handlers directly computing the next state, they produce a *name* for the transition, and a separate function interprets that name. This indirection has costs (boilerplate, harder to trace) and benefits (transitions are enumerated, testable, replayable, centralized).

The self-modifying model eliminates this indirection. Each handler is a concrete transition, not a reference to one. This is the source of both its simplicity and its limitations.

### Collapse of representation layers

Normally you have distinct layers: source code, AST, runtime state, UI. Self-modifying HTML in Hazel collapses them. The source IS a Hazel expression that IS an HTML AST that IS the runtime state that IS rendered as the view. The handler is a function from this unified representation to itself. Everything is the same kind of thing.

This is homoiconicity — code is data, data is code — but typed, visual, and interactive.

It's like the difference between a notebook and a compiled program. In a notebook, the visible document IS the computation. In a compiled program, the visible output is a projection of hidden internal state. Self-modifying HTML is the notebook approach to UI: no hidden state, no compilation step, the thing you see is the thing that runs.


## The RNA World

### The metaphor

In the RNA world hypothesis, RNA served as both genetic material (information storage) AND enzyme (computation). Before the DNA/protein split, one molecule did everything.

The DOM as RNA world: HTML serves as both the visible interface AND the program state AND the code. Before the usual split into source code / runtime state / UI rendering, one substrate does everything.

The DNA/protein split happened because specialization is more efficient. Proteins fold into shapes good for catalysis; DNA is good for copying. Similarly, source code is optimized for editing, runtime state for execution, UI for perception. Unifying them means none is individually optimized. But for simple enough organisms, the RNA world works. And it has a unique advantage: *everything is the same kind of thing*, so you get uniform tooling.

### Can closures be HTML? Yes.

A closure is an environment plus a body. Both are tree-structured data. Both can be represented as HTML. There's no deep reason why "the state that can live in the DOM" is limited to ints and strings. If you're willing to put closure environments in the DOM, the structural/pass-self distinction dissolves — closures ARE structure, just at a deeper level.

This reframes the three levels of embedding:

**Level 1 — Value serialization:** Model data in the DOM, handlers are native. Ints become `Text("42")`, records become nested Divs. This is the structural approach from the examples above, and it's what web apps do with `data-*` attributes.

**Level 2 — Full reification:** Model AND closures in the DOM, but a native interpreter executes the DOM-encoded closures. Everything is inspectable and styleable, but you still need something outside the DOM to run it.

**Level 3 — Metacircular:** Model, closures, AND the interpreter in the DOM. The interpreter is itself HTML that gets interpreted by itself. This requires a fixed point — a bootstrapping interpreter.

Level 2 is probably the sweet spot. Full inspection without infinite regress.

### Hazel syntax as HTML custom elements

HTML custom elements let you use any tag name. If we defined Hazel syntax forms as custom elements, Hazel code becomes part of the DOM. A Hazel expression like:

```
let x = 5 in x + 1
```

As the Hazel ADT (a reflected Hazel-level type for Hazel syntax):

```
HzLet("x", HzInt(5), HzAp(HzAp(HzVar("+"), HzVar("x")), HzInt(1)))
```

As HTML custom elements in the DOM:

```html
<hz-let>
  <hz-bind name="x"><hz-int>5</hz-int></hz-bind>
  <hz-body>
    <hz-ap>
      <hz-ap><hz-var>+</hz-var><hz-var>x</hz-var></hz-ap>
      <hz-int>1</hz-int>
    </hz-ap>
  </hz-body>
</hz-let>
```

A closure (the thing that currently has no surface syntax in Hazel — `x2seg` strips Closure wrappers):

```html
<hz-closure>
  <hz-env>
    <hz-bind name="n"><hz-int>42</hz-int></hz-bind>
    <hz-bind name="self"><hz-closure>...</hz-closure></hz-bind>
  </hz-env>
  <hz-body>
    <hz-fun>
      <hz-pat><hz-pat-var>n</hz-pat-var></hz-pat>
      <hz-ap>
        <hz-var>self</hz-var>
        <hz-tuple>
          <hz-var>self</hz-var>
          <hz-ap><hz-var>+</hz-var><hz-var>n</hz-var><hz-int>1</hz-int></hz-ap>
        </hz-tuple>
      </hz-ap>
    </hz-fun>
  </hz-body>
</hz-closure>
```

### CSS as debugger

If program state is in the DOM, CSS becomes a debugging tool:

```css
hz-closure { border: 1px dashed #888; padding: 4px; }
hz-env { background: #f0f0f0; font-size: 0.8em; }
hz-var { color: #2196F3; }
hz-int { color: #FF9800; }
hz-bind[name="self"] { opacity: 0.5; }
hz-closure hz-closure { border-color: red; }  /* nested closures */
```

Syntax highlighting via CSS selectors. The browser inspector becomes a Hazel AST inspector for free. Complex CSS selectors can query program structure: "highlight all closures that capture a variable named `n`."

### Two mixing directions

**Hazel syntax in HTML:** The HTML tree contains `<hz-*>` elements representing code. Handlers could traverse the DOM, find `<hz-bind name="x">`, change its content. Code as content.

**HTML in Hazel syntax:** Hazel's AST contains HTML constructors as expression forms. `Div([], [Text("hi")])` is a Hazel expression that's also an HTML tree. This is what we already have.

If you unify them, the distinction dissolves. `<div>` is just another constructor in the same namespace as `<hz-let>`. `<hz-fun>` is just another element in the same namespace as `<button>`. The DOM is a Hazel AST is an HTML document. One tree, two readings.

Note that Hazel already represents closures as syntax internally — the runtime uses `Closure(env, body)` AST nodes, and the stepper infrastructure could in principle show explicit substitution steps. The custom element encoding just makes this visible. The DOM serves as surface syntax for things that don't have text syntax.

### The self-modifying tool

A DOM editor written as self-modifying HTML would be a tool that can modify itself:

- A visual tool for editing HTML trees
- The tool IS an HTML tree
- Using the tool modifies the tree
- The tool can modify itself

This is the HyperCard / Smalltalk image move. The tool and the artifact are the same kind of thing. But typed (Hazel catches structural errors), functional (each transition is pure), and transparent (the state IS what you see).

The limitation: self-modification is handler-mediated. You can only change what existing handlers expose. The program controls what modifications are available. This is a feature — it's self-modification with defined entry points, not arbitrary mutation.


## Implementation Issues

### Environment isolation (current bug)

The inline projector uses the *syntax* of the underlying expression, not the evaluated result. Handlers are raw `Fun` nodes, not `Closure(env, body)` nodes. External variable references in handlers are unbound at handler execution time. See the broken `let x = "ON"` example above.

**Fix:** HTMLProj.re should use `info.dynamics` (the evaluated result from the full pipeline) rather than `info.syntax`. The evaluated expression contains Closures that properly capture the lexical environment. The comment in HazelDOM.re already says "Event handlers are already-evaluated Closures" — the infrastructure assumes this, but the inline projector doesn't deliver it.

### `init` restriction

HTMLProj.re's `init` only accepts top-level HTML constructor applications and 4-tuple Elm-style apps. `let`-expressions, function applications, and other forms that *evaluate to* HTML are rejected.

**Fix:** Extend `init` to accept any expression whose evaluated result is an HTML constructor. The conceptual shift: render the *value* of an expression, not just expressions that syntactically look like HTML.

This would unlock the pass-self pattern, let-bound helpers, and arbitrary computation that produces HTML.

### Open questions

- When a legacy handler produces a new HTML tree, the underlying expression is replaced. If the source was `view(view, 0)` and the handler returns `Div(...)`, do we lose the original source? Should the projector preserve it and re-evaluate?
- How does source replacement interact with the environment fix? If the projector uses evaluated results (with Closures), and the handler produces new Closures, those Closures carry the environment forward. But if the source gets replaced with a concrete tree, subsequent code edits might break the captured environment.
- Composition: How do you compose two self-modifying components? In MVU, you compose models and messages. In self-modifying HTML, each handler needs to reconstruct its containing tree — or you need some notion of local rewriting that only touches a subtree.
- How far does the RNA world go before the performance and ergonomic costs make the DNA/protein split necessary? Is Level 2 (full reification, native interpreter) the right equilibrium?
