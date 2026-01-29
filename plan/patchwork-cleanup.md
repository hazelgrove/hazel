# Patchwork Integration Cleanup Plan

This plan covers cleanup of the Hazel-Patchwork integration on the `patchwork` branch.

## Phase 1: Type Conversion and Dead Code Cleanup ✓ COMPLETE

Goal: Remove unused types and dead code specific to this branch.

### Tasks

- [x] **Remove nested `Tile` module from `Delta.mli`** (was lines 147-190)
  - Only `FlatTile` is used; the nested `Tile` with `children: t list` is never referenced in `Iframe.re`
  - The flattening happens via `AutoSeg`, not via nested Tiles

- [x] **Remove nested `Tile` interface from `embed/src/types/delta.d.ts`** (was lines 33-40)
  - Same as above - only `FlatTile` is used in `FlatPiece` and `HazelDoc`

- [x] **Remove commented-out EditOp types from `embed/src/types/delta.d.ts`** (was lines 58-77)
  - `DeleteOp`, `InsertOp`, `ReplaceOp`, `EditScript` were an earlier diff-based sync approach
  - We now use full-state sync, so these are dead

- [x] **Remove `embed/src/App.tsx` and related test harness files**
  - Removed: `App.tsx`, `main.tsx`, `index.html`, `index.css`
  - Removed: `MessageDisplay.tsx`, `DocStateManager.tsx`, `DocComponents.css`
  - These were for local development testing; real consumer is `patchwork-extra/hazel/src/tool.tsx`

- [x] **Add documentation to type files**
  - Added comprehensive comments to `delta.d.ts` explaining flat vs nested representation
  - Added protocol documentation to `messages.d.ts`

- [x] **Verify build still works after removals**
  - `make dev` builds Hazel OCaml code successfully
  - `pnpm build` in embed/ builds the package successfully

### Files Modified in Phase 1

- `src/haz3lcore/Delta.mli` - removed Tile module
- `embed/src/types/delta.d.ts` - removed Tile interface, commented code, added docs
- `embed/src/types/messages.d.ts` - added protocol documentation
- Deleted: `embed/src/App.tsx`, `embed/src/main.tsx`, `embed/index.html`, `embed/src/index.css`
- Deleted: `embed/src/components/MessageDisplay.tsx`, `DocStateManager.tsx`, `DocComponents.css`

---

## Phase 2: Embed UI Cleanup ✓ COMPLETE

Goal: Minimize the embed wrapper UI in `patchwork-extra/hazel/src/tool.tsx`.

### Tasks

- [x] **Replace sidebar with thin top bar**
  - Was: 60/40 split with HazelEmbed on left, large sidebar on right
  - Now: Full-width HazelEmbed with thin bar above containing:
    - URL text field (monospace, compact)
    - "Local" button → sets URL to `http://localhost:8001`
    - "Remote" button → sets URL to `https://hazel.org/build/patchwork/`
    - Tile count display
    - "Graph" toggle button
  - Removed: Ping button, large title display, 60/40 layout

- [x] **Make DocGraph toggleable instead of removing it**
  - DocGraph is hidden by default
  - "Graph" button in top bar toggles a collapsible panel
  - Keeps the debug visualization available when needed

### Files Modified in Phase 2

- `patchwork-extra/hazel/src/tool.tsx` - replaced sidebar layout with thin top bar

### Not Done (kept for potential future cleanup)

- DocGraph.tsx still exists in embed package (used via toggle)
- react-d3-tree dependency still present (needed for DocGraph)

---

## Not In Scope (for now)

- Console.log debug statements in `Iframe.re` - leave for debugging
- Renaming `RedundantCoverterIGuess` module - cosmetic, low priority
- Security hardening (PostMessage `"*"` origin) - future work

---

## Type Conversion Reference

### TypeScript → OCaml Generation

The TypeScript types in `embed/src/types/` are the source of truth. To regenerate OCaml bindings:

```bash
cd embed
pnpm type:delta     # Generates Delta.mli from delta.d.ts
pnpm type:messages  # Generates MessageTypes.mli from messages.d.ts
pnpm type           # Both at once
```

### Runtime Conversion (Iframe.re)

The `RedundantCoverterIGuess` module in `Iframe.re` handles bidirectional conversion:

| Direction | Function | Purpose |
|-----------|----------|---------|
| OCaml → JS | `of_*` functions | Convert OCaml types to JS for PostMessage |
| JS → OCaml | `to_*` functions | Convert received JS messages to OCaml |

Key entry points:
- `js_of_autoseg`: AutoSeg.Doc.t → Ojs.t (for sending state)
- `autoseg_of_hazeldoc`: HazelDoc.t_0 → AutoSeg.Doc.t (for receiving state)

### Tree ↔ Flat Conversion (AutoSeg.re)

| Function | Purpose |
|----------|---------|
| `seg_to_doc` | Segment (nested tree) → Doc (flat map) |
| `doc_to_seg` | Doc (flat map) → Segment (nested tree) |

The flat representation uses UUID references instead of nested children, which is more compatible with Automerge's CRDT data structures.
