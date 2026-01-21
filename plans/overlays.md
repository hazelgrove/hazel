# Overlays: Implementation Plan

## Concept

Overlays are visual elements and interactions attached to terms that render over the code without modifying the AST. Unlike projectors (which replace syntax) or refractors (which render offside), overlays render directly over the code tokens.

Examples:
- Error/warning indicators (badge at term corner)
- Test pass/fail checkmarks
- Number scrubbing (modifier+drag to change values)
- Type hints on hover

## Core Insight

Visual appearance and interaction capability are orthogonal:
- An overlay might be always visible but not interactive (error badge)
- An overlay might be invisible until modifier held, then both visible AND interactive (number scrubber)
- An overlay might be visible on hover and interactive on click (type hint)

So we have ONE system where each overlay specifies its own visibility and interaction logic.

## Architecture

### Core Types (`src/haz3lcore/overlays/Overlay.re`)

```reason
// UI state passed to overlay handlers
type ui_state = {
  modifier_held: bool,      // Alt/Option key
  hover_id: option(Id.t),   // Currently hovered term
  active_interaction: option(interaction), // Ongoing drag, etc.
};

type interaction =
  | Scrubbing({
      target_id: Id.t,
      start_value: float,
      start_mouse: Point.t,
      current_mouse: Point.t,
    });

// Events overlays can respond to
type event =
  | MouseDown(Point.t)
  | MouseMove(Point.t)
  | MouseUp(Point.t)
  | Click
  | ModifierDown
  | ModifierUp;

// Actions overlays can trigger
type action =
  | EditToken(Id.t, string)        // Replace token text
  | StartInteraction(interaction)
  | UpdateInteraction(interaction)
  | EndInteraction
  | EditorAction(Action.t);        // Delegate to editor

// An overlay handler for a specific term
type handler = {
  // Render overlay (if any) given measurement and current state
  view: (Measured.measurement, ui_state) => option(Node.t),

  // Handle events, return action (if any)
  on_event: option((event, Measured.measurement, ui_state) => option(action)),
};

// All overlays, keyed by target term ID
type t = Id.Map.t(list(handler));

// Merge multiple overlay sources
let merge: list(t) => t;
```

### Overlay Computation (`src/haz3lcore/overlays/OverlayCompute.re`)

Functions that generate overlays from editor state:

```reason
// Error indicators from statics
let errors: (list(Id.t)) => Overlay.t;

// Warning indicators (if we have warnings)
let warnings: (list(Id.t)) => Overlay.t;

// Test result indicators from dynamics
let tests: (TestResults.t) => Overlay.t;

// Number scrubbing for all numeric literals
let number_scrubbers: (Segment.t, Measured.t) => Overlay.t;
```

### Rendering (`src/web/app/editors/decoration/OverlayView.re`)

```reason
let view:
  (
    ~font_metrics: FontMetrics.t,
    ~measured: Measured.t,
    ~term_data: TermData.t,
    ~ui_state: Overlay.ui_state,
    overlays: Overlay.t,
  ) => Node.t;

// Renders all overlays, sorted by z-index
// Handles event delegation to appropriate handlers
```

### Event Handling

In the editor's event handling:
1. Track modifier key state (Alt/Option)
2. On mouse events, check if any overlay wants to handle it
3. Route to overlay handler, apply resulting action

## Implementation Phases

### Phase 1: Minimal Infrastructure

1. Create `Overlay.re` with core types
2. Create `OverlayView.re` with basic rendering
3. Wire into editor view (add overlay layer to DOM)
4. Track modifier key state in editor

### Phase 2: Error Overlays (Proof of Concept)

```reason
// OverlayCompute.re
let error_overlay = (id: Id.t): Overlay.handler => {
  view: (measurement, _ui_state) => {
    // Red dot positioned at end of term, slightly above
    let pos = measurement.last;
    Some(Node.div(
      ~attrs=[
        Attr.classes(["overlay-error"]),
        DecUtil.abs_position(~font_metrics, pos),
      ],
      [Node.text({js|●|js})],  // Or an icon
    ));
  },
  on_event: None,  // Not interactive for now
};

let errors = (error_ids: list(Id.t)): Overlay.t =>
  error_ids
  |> List.map(id => (id, [error_overlay(id)]))
  |> Id.Map.of_list;
```

### Phase 3: Number Scrubbing

```reason
// OverlayCompute.re
let number_scrubber = (id: Id.t, current_value: float): Overlay.handler => {
  view: (measurement, ui_state) => {
    // Only show when modifier held or actively scrubbing
    let dominated_by_overlay = ...figure out overlay logic below
    let dominated = ui_state.modifier_held && dominated_by_overlay
    let dominated = switch ui_state.active_interaction
      Scrubbing({target_id, _}) when target_id == id => true
      _ -> false
    };
    if (!visible) { return None; }

    // Render scrub indicator
    switch (ui_state.active_interaction) {
    | Some(Scrubbing({current_mouse, ...})) when target_id == id =>
      // Draw line from token to cursor, show value
      Some(scrub_visual(measurement, current_mouse, current_value))
    | _ =>
      // Just highlight that this is scrubbable
      Some(scrub_indicator(measurement))
    };
  },

  on_event: Some((event, measurement, ui_state) => {
    switch (event, ui_state.active_interaction) {
    | (MouseDown(pos), None) when ui_state.modifier_held =>
      Some(StartInteraction(Scrubbing({
        target_id: id,
        start_value: current_value,
        start_mouse: pos,
        current_mouse: pos,
      })))
    | (MouseMove(pos), Some(Scrubbing(s))) when s.target_id == id =>
      let new_value = compute_scrub_value(s.start_value, s.start_mouse, pos);
      Some(UpdateInteraction(Scrubbing({...s, current_mouse: pos})))
      // Also emit EditToken to update the code
    | (MouseUp(_), Some(Scrubbing(s))) when s.target_id == id =>
      Some(EndInteraction)
    | _ => None
    };
  }),
};
```

### Phase 4: Polish

1. CSS styling for overlays
2. Smooth scrubbing UX (value display, line rendering)
3. Keyboard escape to cancel scrub
4. Consider: snap to nice values, shift for fine control

## CSS

```css
/* proj-overlays.css */

.overlay-layer {
  position: absolute;
  top: 0;
  left: 0;
  pointer-events: none;  /* Pass through by default */
  z-index: 100;
}

.overlay-error {
  color: var(--error-color);
  font-size: 0.7em;
  transform: translate(2px, -0.5em);  /* Offset from token end */
  pointer-events: auto;  /* If we want click-to-jump */
}

.overlay-scrub-indicator {
  /* Subtle highlight when modifier held */
  background: rgba(var(--accent-rgb), 0.1);
  border-radius: 2px;
}

.overlay-scrub-active {
  /* Visual during active scrub */
}

.overlay-scrub-line {
  stroke: var(--accent-color);
  stroke-width: 2;
}

.overlay-scrub-value {
  background: var(--bg-color);
  border: 1px solid var(--accent-color);
  padding: 2px 6px;
  border-radius: 4px;
  font-size: 0.9em;
}
```

## Integration Points

### Editor State

Add to editor model:
```reason
type overlay_state = {
  modifier_held: bool,
  active_interaction: option(Overlay.interaction),
};
```

### View Pipeline

In `CodeWithStatics` or similar:
```reason
let overlays = Overlay.merge([
  OverlayCompute.errors(statics.error_ids),
  OverlayCompute.number_scrubbers(syntax.segment, syntax.measured),
  // ... other overlay sources
]);

// In view:
OverlayView.view(~font_metrics, ~measured, ~term_data, ~ui_state, overlays)
```

### Event Handling

In editor update:
```reason
| KeyDown(key) when key == "Alt" =>
  {...model, overlay_state: {...model.overlay_state, modifier_held: true}}
| KeyUp(key) when key == "Alt" =>
  {...model, overlay_state: {...model.overlay_state, modifier_held: false}}
| MouseDown(pos) when model.overlay_state.modifier_held =>
  // Check if any overlay wants this event
  // Route to handler, apply action
```

## Appendix: Test Migration Path

Current test rendering (`EvalResult.re:806-835`) uses:
- `TestResults.t` from dynamics
- Positions checkmark at `last` point of test token's measured shards
- Simple div with CSS class for pass/fail/indet

To migrate:
1. Create `OverlayCompute.tests(test_results)` that generates overlay handlers
2. Each handler renders checkmark at appropriate position
3. Remove `test_result_layer` from EvalResult
4. (Optional) Make tests probes for richer info

Benefits:
- Unified rendering system
- Could add interaction (click to see test details)
- Consistent styling/positioning with other overlays

## Appendix: Relationship to Projectors/Refractors

| Aspect | Projectors | Refractors | Overlays |
|--------|------------|------------|----------|
| Modifies AST | Yes (placeholder) | No | No |
| Position | Replaces syntax | Offside (right margin) | Over code |
| Reserves space | Yes (Shape) | Yes (rows) | No |
| Persisted | Yes | Partially (manuals) | No (computed) |
| Interactive | Yes (focus model) | Yes (via projector) | Yes (event handlers) |

Long-term, these could share more infrastructure:
- Measurement lookup
- Event routing
- View layering

For now, overlays are a parallel system that can evolve independently.

## Open Questions

1. **Z-ordering**: When overlays overlap, which is on top? Probably: active interaction > hover > static
2. **Hit testing**: How to determine which overlay "owns" a mouse event? Probably by checking bounding boxes
3. **Performance**: With many number literals, is creating handlers expensive? Probably fine, but could lazy-init
4. **Modifier key**: Alt? Cmd? Configurable? Start with Alt, can change later
