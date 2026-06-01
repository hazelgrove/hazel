# Surfaces & Placements

Every container in the Hazel UI is described by two independent choices:

- **Surface** — interior identity: background, typography, headings, buttons, list rows. "What kind of place is this?"
- **Placement** — relationship to the parent: border, radius, outer padding, shadow. "How does this sit in its container?"

Surface owns *internal layout* (how the things inside space themselves). Placement owns the *edge*.

## Surfaces

There are three.

### Code
Monospace font, syntax-colored tokens, cursor, code-specific background, statics decorations.

Variants (within the surface):
- **editable** — the main editor
- **read-only** — code that can't be edited
- **evaluator** — code with live evaluation output

### Workspace
UI font, normal text, workspace headings, buttons, list rows, toggles. The general-purpose container interior — sidebars, exercise bodies, dialogs, tooltips.

### Menu
Yellow-tinted background, tight padding, hover-to-select item rows, grouped sections with labeled headings. Used for menus, dropdowns, context menus. Always raised with shadow + outline.

A menu either floats (all four corners rounded) or is anchored to one of its corners — `.tl / .tr / .bl / .br` mark the anchor; that corner is sharp and only the opposite corner is rounded.

Menus have a default `max-height: 70vh` and scroll their contents when they exceed it.

## Placements

Placements apply only to the **code** and **workspace** surfaces. Menus have no placement variants.

### inset — *code only*
Recessed border, no shadow. The code editor sits visually below its parent.

### inline — *workspace only*
Flush with parent at the top level. When nested inside another workspace, the container gets an outline + radius + padding so it reads as a contained section.

### raised — *workspace only*
Same chrome as a nested inline, plus a shadow. Used for popovers, and (composed with a backdrop and centered positioning) modal-style dialogs.

### tooltip — *workspace only*
Tight padding, small text scale, anchored, info-only chrome.

When a placement needs to scale typography (e.g. tooltip's smaller text), it overrides the surface's defaults directly rather than picking from a layered scale.

## Valid combinations

| Combination | Examples |
|---|---|
| code × inset | main editor, mini editors, read-only code, stepper output |
| workspace × inline | sidebars, exercise body, tutorial body, projector UIs |
| workspace × raised | assistant suggestions, explain-this, settings dialog (with backdrop + centered positioning) |
| workspace × tooltip | icon tooltips, hover info |
| menu (no placement) | nut menu, context menu, dropdown menus, slash menu |

Modal-style dialogs (settings, confirmations) are not their own placement. They are composed: `raised` provides the chrome, a backdrop element dims the page, and the caller positions the surface centered on the page.
