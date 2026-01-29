# Patchwork Integration Cleanup Plan

This plan covers cleanup of the Hazel-Patchwork integration on the `patchwork` branch.

## Phase 1: Type Conversion and Dead Code Cleanup

Goal: Remove unused types and dead code specific to this branch.

### Tasks

- [ ] **Remove nested `Tile` module from `Delta.mli`** (lines 147-190)
  - Only `FlatTile` is used; the nested `Tile` with `children: t list` is never referenced in `Iframe.re`
  - The flattening happens via `AutoSeg`, not via nested Tiles

- [ ] **Remove nested `Tile` interface from `embed/src/types/delta.d.ts`** (lines 33-40)
  - Same as above - only `FlatTile` is used in `FlatPiece` and `HazelDoc`

- [ ] **Remove commented-out EditOp types from `embed/src/types/delta.d.ts`** (lines 58-77)
  - `DeleteOp`, `InsertOp`, `ReplaceOp`, `EditScript` were an earlier diff-based sync approach
  - We now use full-state sync, so these are dead

- [ ] **Remove `embed/src/App.tsx`**
  - This is a test harness; the real consumer is `patchwork-extra/hazel/src/tool.tsx`
  - May need to update `embed/src/index.ts` if it exports App

- [ ] **Verify build still works after removals**

### Verification

After Phase 1, run:
```bash
make deps
make dev
```
And verify Hazel builds and the patchwork embed package builds.

---

## Phase 2: Embed UI Cleanup

Goal: Minimize the embed wrapper UI in `patchwork-extra/hazel/src/tool.tsx`.

### Tasks

- [ ] **Remove `DocGraph` component and import**
  - Remove from `patchwork-extra/hazel/src/tool.tsx`
  - It's debug visualization, not needed for production

- [ ] **Remove `DocGraph.tsx` from `embed/src/components/`**
  - And remove its export from `embed/src/index.ts`
  - This also removes the `react-d3-tree` dependency

- [ ] **Replace sidebar with thin top bar**
  - Current: 60/40 split with HazelEmbed on left, sidebar on right
  - New: Full-width HazelEmbed with thin bar above containing:
    - URL input (for switching between localhost and build server)
    - Tile count (optional, for debugging)
  - Remove: Ping button, title display, DocGraph

- [ ] **Update `patchwork-extra/hazel/package.json`**
  - Remove `react-d3-tree` dependency if no longer needed

### Verification

After Phase 2:
1. Build the embed package
2. Build the patchwork-extra/hazel tool
3. Test in Patchwork that Hazel loads and syncs correctly
4. Verify URL switching between localhost and build server works

---

## Not In Scope (for now)

- Console.log debug statements in `Iframe.re` - leave for debugging
- Renaming `RedundantCoverterIGuess` module - cosmetic, low priority
- Security hardening (PostMessage `"*"` origin) - future work

---

## Files Modified

### Phase 1
- `src/haz3lcore/Delta.mli` - remove Tile module
- `embed/src/types/delta.d.ts` - remove Tile interface and commented code
- `embed/src/App.tsx` - delete
- `embed/src/index.ts` - possibly update exports

### Phase 2
- `embed/src/components/DocGraph.tsx` - delete
- `embed/src/index.ts` - remove DocGraph export
- `patchwork-extra/hazel/src/tool.tsx` - replace sidebar with thin bar
- `patchwork-extra/hazel/package.json` - remove react-d3-tree
