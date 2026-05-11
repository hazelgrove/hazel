# Elements

Every UI inside a surface is built from a small set of **content primitives** composed inside **layout containers**.

- **Content primitives** — generic appearance, default styling, no parent assumptions.
- **Layout containers** — provide CSS context that restyles their children.

A primitive's look depends on its parent. A `Button` on a workspace surface looks like a button; the same `Button` inside `Tabs` looks like a tab. Only layout containers may restyle their children.

The workspace and menu surfaces have separate primitive sets. The workspace primitives are listed first; menu primitives at the bottom.

## Workspace primitives

### Button
Icon button. Wobble + scale on hover.

Variants: `~kind` (link / file-upload), `~disabled`, `~active`, `~subtle`, `~tooltip`.

### Toggle
Binary on/off switch. No label — wrap in a `ListRow` if you need text alongside.

### TextInput
Text field. Variants: `~multiline`.

### Select
Dropdown.

### Heading
Three levels, each ported from an existing pattern:

- **h1** — exercise title style (`.title-cell .title-text`): large, bold, BR4. For page and section titles.
- **h2** — all-caps cell caption (`.cell-caption strong`): small, bold, uppercase, BR3. For section labels and panel titles.
- **h3** — probarium subtle (`#probe-sidebar .legend .title`): small, regular, BR2. For subtle subsection labels.

### CodeInline
Code or type content embedded in a workspace. The bridge between workspace surface and code surface.

### Badge
Small labeled indicator. Variants: `~tone` (status / count / kbd).

### Divider
Visual separator. Variants: `~orientation` (horizontal / vertical).

### ProgressBar
Two flavours:

- **continuous** (default) — track + fill whose width is the progress. Ports the agent context meter.
- **segmented** — track containing a row of clickable status-coded segments (`pass` / `fail` / `indet`). Ports the test-panel `.test-bar`.

Both support an optional `.label` above the track.

### ListRow
A row in a list. Slots: *leading* (icon / control / none), *label*, *trailing* (control / badge / none). State variants: `~active`, `~expanded`, `~selected`. Status variants: `~error`, `~syntax`, `~warning`, `~hole` (each adds a left accent + tinted bg, ported from `.problem-row` in sidebar.css).

The highest-leverage primitive — replaces problem rows, legend items, settings rows, history rows. (Menu items may end up on a separate menu surface and are not absorbed here.)

## Layout containers

A container restyles its children via CSS descendant selectors. The children stay generic.

```css
.tabs .button         { /* tab look */ }
.tabs .button.active  { /* active tab */ }
```

```reason
tabs([
  Button.button(~active=true,  Icons.problems,   switch_to_problems),
  Button.button(~active=false, Icons.assistant,  switch_to_assistant),
])
```

### Tabs
A row of buttons styled as tabs. Each child `Button` owns its own `~active` state. Three forms:

- default (vertical) — sidebar tab strip with a right-border active indicator
- `.horizontal` — bordered segmented control (probarium sample-color-scheme)
- `.title` — bold uppercase mode-labels with `/` separator, used as a switchable panel title (probarium / printarium)

## Composition rules

- Only layout containers restyle their children. Regular `<div>`s do not.
- Layout containers should stay few to keep the implicit-styling cost low. Looking at a primitive in isolation should usually be enough to predict how it will render.

## Menu surface primitives

Distinct from the workspace primitives — the menu surface has its own typography and item-row patterns.

### MenuItem
A single clickable row inside a menu. Slots: leading icon, label, optional trailing keyboard shortcut. State variants: `~selected`.

### MenuGroup
A labeled section: a name heading and a list of menu items. Adjacent groups get a hairline divider automatically.

### MenuDivider
Thin separator between items within a group. Distinct from the workspace `Divider` — tighter and tuned for menu density.

## Open

- **Toolbar** as a second workspace layout container — a row of small flat buttons. Skip until a third use site appears.
- **Segmented control** is already covered by `Tabs.horizontal`.
