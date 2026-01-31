# Patchwork Automerge Patches Integration - Implementation Plan

## Executive Summary

**Goal:** Use Automerge's native patch events to send deltas (not full state) from Patchwork to Hazel iframe.

**Key Discovery:** Automerge patches are granular (field-level), not piece-level. A single piece update generates 20-30 patches. We must extract piece IDs from patches and fetch complete pieces from `handle.doc()`.

---

## Design Philosophy

### Interface Boundaries

The goal is to get Patchwork (TypeScript) out of the business of managing full state. The interface between Patchwork and Hazel should be **delta-based in both directions**:

- **Sender side (Hazel → Patchwork):** Already working. Hazel computes a delta (changed/added/deleted pieces) and sends only affected pieces. This is currently expensive (full tree diff) but could be optimized later on the Hazel side.

- **Receiver side (Patchwork → Hazel):** Currently sends full state (~443 KB). This plan changes it to send only affected pieces (~7 KB). Hazel does the work of merging the delta with its current state.

### Responsibilities

- **Patchwork (tool.tsx):** Thin pass-through layer
  - Receives Automerge patches
  - Extracts affected piece IDs from patch paths
  - Fetches complete pieces from `handle.doc()`
  - Sends affected pieces to Hazel (no diffing, no full state management)

- **Hazel:** Owns state management
  - Receives partial piece map
  - Merges with current state (flatten, override, unflatten)
  - Applies result via sync_replace

### No New Message Types

We use the existing `{ t: "state", state: { title, pieces } }` message format. The only difference is that `pieces` may contain a subset of all pieces rather than the full map. Hazel handles both cases with the same merge algorithm:

- **Full state:** All pieces override current → result is incoming state
- **Partial state:** Some pieces override → unchanged pieces remain

Deletions work implicitly: when a piece is deleted, its parent's `children` array changes. We send the updated parent. The deleted piece becomes orphaned and is excluded during unflatten.

---

## Repository Structure

**This plan involves TWO separate repositories:**

1. **hazel** - Main Hazel editor (OCaml/ReasonML)
   - Location: `/Users/andrewblinn/Dropbox/projects/hazel`
   - Branch: `patchwork`
   - Relevant files:
     - `src/haz3lcore/zipper/action/Action.re` - Action type definitions
     - `src/haz3lcore/zipper/action/Perform.re` - Action dispatch
     - `src/haz3lcore/patchwork/SyncReplace.re` - Sync and merge logic
     - `src/haz3lcore/patchwork/PatchworkComm.re` - Communication with parent
     - `src/haz3lcore/patchwork/FlatConvert.re` - Segment ↔ Doc conversion

2. **patchwork-extra** - Patchwork tool extensions
   - Location: `/Users/andrewblinn/Dropbox/projects/patchwork-extra/hazel`
   - Branch: `hazel`
   - Relevant files:
     - `src/tool.tsx` - React component that embeds Hazel iframe
   - Build/deploy: `pnpm run build` + `patchwork push`

---

## Current State

**Sender side (Hazel → Patchwork):** ✅ Working
- Commit: `25d24c26e "Implement delta-based sync protocol in OCaml"`
- Computes delta, sends only changed pieces
- Performance: 13ms, ~7 KB payload for typical edit

**Receiver side (Patchwork → Hazel):** ❌ Sends full state
- Commit: `93df914 "Update tool.tsx for map-based schema and echo loop prevention"`
- Sends full state (~443 KB) on every remote edit
- Echo prevention working

---

## Data Flow: Receiver Side (Patchwork → Hazel)

### Step-by-step flow after implementation:

1. Remote user makes edit in their Hazel iframe
2. Their edit syncs via Automerge to our tool.tsx
3. `handle.on("change", changeListener)` fires with patches
4. Check `isUpdatingFromIframe.current` - if true, skip (echo prevention)
5. Extract affected piece IDs from patch paths:
   ```typescript
   if (path[0] === 'pieces') affectedIds.add(path[1])
   ```
6. Call `handle.doc()` INSIDE the listener to get fresh state
7. Build partial pieces map from affected IDs:
   ```typescript
   const pieces: { [id: string]: any } = {};
   for (const id of affectedIds) {
     const piece = freshDoc.pieces[id];
     if (piece) pieces[id] = piece;
   }
   ```
8. Send to Hazel: `{ t: "state", state: { title, pieces } }`
9. PatchworkComm receives message, converts to OCaml `Doc.t`
10. Dispatch `SyncReplace(doc)` action
11. In `sync_replace`:
    - `current_seg = Zipper.zip(z)`
    - `current_doc = seg_to_doc(current_seg)`
    - `merged_doc = Doc.union(current_doc, delta_doc)` (delta overrides)
    - `new_seg = doc_to_seg(merged_doc)`
    - Reposition cursor in new segment
12. Editor re-renders with updated state

---

## How Automerge Actually Works

### Why Patches Are Granular

**User's perspective:**
```typescript
handle.change(d => {
  d.pieces[id] = { id: "uuid", label: "foo", children: [...] };
});
```
"I'm setting one object" → Expect 1 patch

**Automerge's CRDT perspective:**
```
1. PUT object container at pieces[id]
2. PUT string "uuid" at pieces[id].id
3. PUT string "foo" at pieces[id].label
4. PUT array container at pieces[id].children
5. INSERT ... at pieces[id].children[0]
...
```
Result: **20-30 patches** for one piece update

**Why?** Each field is tracked separately for conflict-free merging. This enables concurrent edits to different fields of the same piece.

### Critical Rule

`patch.value` contains ONLY the value at that exact path. For container operations (objects, arrays), `patch.value = {}` - an empty container. The fields are set in subsequent patches.

**Wrong approach:** Try to use `patch.value` directly → get `{}`
**Correct approach:** Extract piece ID from path, fetch complete piece from `handle.doc()`

### Performance Note

Calling `handle.doc()` inside the change listener is cheap (~0.01ms). Automerge maintains a materialized JavaScript view that's already computed. This is the intended usage pattern.

---

## The Echo Loop Bug

### Symptoms

- Rapid typing/backspacing causes edits out of order
- Cursor jumps to unexpected positions
- `sync_replace` called on SENDER (should only happen on receiver)

### Root Cause

```
1. User types in local Hazel iframe
2. Iframe sends state to Parent (tool.tsx)
3. Parent applies to Automerge
4. Automerge triggers change event
5. Change event sends state BACK to local iframe (ECHO!)
6. Iframe processes as "remote edit" → sync_replace
7. Loop continues...
```

### The Fix: Two-Part Echo Prevention

**Part 1: Reset flag inside changeListener when skipping**
```typescript
const changeListener = ({ patches }) => {
  if (isUpdatingFromIframe.current) {
    console.log(`[TOOL] Skipping echo`);
    isUpdatingFromIframe.current = false;  // Reset IMMEDIATELY
    return;
  }
  // Process patches...
};
```

**Part 2: Use queueMicrotask instead of setTimeout**
```typescript
case "state": {
  isUpdatingFromIframe.current = true;

  handle.change((d) => { /* apply changes */ });

  queueMicrotask(() => {
    isUpdatingFromIframe.current = false;
  });

  break;
}
```

**Why both?**
- Synchronous change event: Part 1 resets flag
- Asynchronous change event: Part 2 resets flag after microtask
- No timing window where echo can escape

### Diagnostic Checklist

If echo symptoms return:
1. Is `isUpdatingFromIframe` ref defined?
2. Is flag set TRUE before `handle.change()`?
3. Is flag checked at start of changeListener?
4. Is flag reset to FALSE inside changeListener when skipping? (CRITICAL)
5. Is `queueMicrotask` used instead of `setTimeout`?

Test: Type "aaaa" quickly then backspace rapidly. Check for "Skipping echo" logs and no `sync_replace` on sender.

---

## Implementation Changes

### TypeScript (tool.tsx)

Replace `useEffect([doc])` with `handle.on("change", changeListener)`:

```typescript
const hasInitialized = React.useRef(false);

useEffect(() => {
  const changeListener = ({ patches }: { patches: any[] }) => {
    // Echo prevention
    if (isUpdatingFromIframe.current) {
      console.log(`[TOOL] Skipping echo`);
      isUpdatingFromIframe.current = false;
      return;
    }

    // Extract affected piece IDs
    const affectedIds = new Set<string>();
    let titleChanged = false;

    for (const patch of patches) {
      if (patch.path[0] === 'pieces' && patch.path.length >= 2) {
        affectedIds.add(patch.path[1]);
      } else if (patch.path[0] === 'title') {
        titleChanged = true;
      }
    }

    // Fetch complete pieces from fresh state
    const freshDoc = handle.doc();
    const pieces: { [id: string]: any } = {};

    for (const id of affectedIds) {
      const piece = freshDoc.pieces[id];
      if (piece) pieces[id] = piece;
      // If piece doesn't exist, it was deleted - parent's children changed,
      // orphan will be excluded during unflatten
    }

    // Send to Hazel using existing message format
    if (Object.keys(pieces).length > 0 || titleChanged) {
      sendToHazel.current({
        t: "state",
        state: {
          title: freshDoc.title,
          pieces
        }
      });
    }
  };

  // Initial state on mount
  if (!hasInitialized.current) {
    const doc = handle.doc();
    sendToHazel.current({
      t: "state",
      state: { title: doc.title, pieces: doc.pieces }
    });
    hasInitialized.current = true;
  }

  (handle as any).on("change", changeListener);
  return () => (handle as any).off("change", changeListener);
}, [handle]);  // Only depend on handle, NOT doc!
```

### OCaml Changes

**1. Action.re** - Change SyncReplace payload type:
```ocaml
(* Before *)
| SyncReplace(Segment.t)

(* After *)
| SyncReplace(FlatConvert.Doc.t)
```

**2. PatchworkComm.re** - Don't convert to segment, pass doc directly:
```ocaml
(* Before *)
| `U_s8_state(state) =>
    let js_state = EditorState.get_state(state);
    let flatdoc = JsConvert.flatdoc_of_hazeldoc(js_state);
    let seg = FlatConvert.doc_to_seg(flatdoc);
    schedule_action(SyncReplace(seg));

(* After *)
| `U_s8_state(state) =>
    let js_state = EditorState.get_state(state);
    let delta_doc = JsConvert.flatdoc_of_hazeldoc(js_state);
    schedule_action(SyncReplace(delta_doc));
```

**3. Perform.re** - Pass doc to sync_replace:
```ocaml
(* Before *)
| SyncReplace(segment) =>
    SyncReplace.sync_replace(z, segment)

(* After *)
| SyncReplace(delta_doc) =>
    SyncReplace.sync_replace(z, delta_doc)
```

**4. SyncReplace.re** - Add merge logic:
```ocaml
(* Before *)
let sync_replace = (z: Zipper.t, segment: Segment.t): option(Zipper.t) => {
  (* ... save cursor info ... *)
  let z = Zipper.unzip(segment);
  (* ... restore cursor ... *)
};

(* After *)
let sync_replace = (z: Zipper.t, delta_doc: FlatConvert.Doc.t): option(Zipper.t) => {
  (* ... save cursor info ... *)

  (* Merge delta with current state *)
  let current_seg = Zipper.zip(z);
  let current_doc = FlatConvert.seg_to_doc(current_seg);
  let merged_doc = FlatConvert.Doc.union(
    (_, _, b) => Some(b),  (* delta overrides current *)
    current_doc,
    delta_doc
  );
  let new_seg = FlatConvert.doc_to_seg(merged_doc);

  let z = Zipper.unzip(new_seg);
  (* ... restore cursor ... *)
};
```

---

## Implementation Checklist

### Phase 1: OCaml Changes (hazel repo)

- [ ] **Action.re:** Change `SyncReplace(Segment.t)` to `SyncReplace(FlatConvert.Doc.t)`
- [ ] **Perform.re:** Update pattern match to pass doc to sync_replace
- [ ] **SyncReplace.re:**
  - [ ] Change signature to take `Doc.t`
  - [ ] Add merge logic: flatten current, merge, unflatten
- [ ] **PatchworkComm.re:** Remove `doc_to_seg` call, dispatch `SyncReplace(doc)`
- [ ] Build and verify no compile errors

### Phase 2: TypeScript Changes (patchwork-extra repo)

- [ ] Replace `useEffect([doc])` with `handle.on("change", changeListener)`
- [ ] Add `hasInitialized` ref
- [ ] In changeListener:
  - [ ] Check echo flag, reset immediately when skipping
  - [ ] Extract affected piece IDs from patch paths
  - [ ] Call `handle.doc()` inside listener
  - [ ] Build partial pieces map
  - [ ] Send `{ t: "state", state: { title, pieces } }`
- [ ] Send initial full state once on mount
- [ ] useEffect depends only on `[handle]`
- [ ] Update echo flag timing: use `queueMicrotask`

### Phase 3: Testing

**Single client:**
- [ ] Type character - no crashes
- [ ] Type rapidly - no echo loops
- [ ] Console shows patch count and affected pieces

**Two clients:**
- [ ] Type in one - other updates correctly
- [ ] No cursor jumping
- [ ] Rapid typing in both - stable

**Edge cases:**
- [ ] Delete pieces - works (parent changes, orphan excluded)
- [ ] Large program (1800 tiles) - smooth

### Phase 4: Deploy

- [ ] Build patchwork-extra: `pnpm run build`
- [ ] Deploy: `patchwork push`
- [ ] Verify in production

---

## Performance Expectations

**Payload size:** 443 KB → ~7 KB (for typical 1-3 piece edits)

**Hazel-side cost:** The merge step (flatten, merge, unflatten) adds overhead, but this is acceptable for now. Future optimization could make Hazel's state management more incremental.

**Unchanged:** Cursor repositioning (~24ms) remains the bottleneck.

---

---

## Future Optimization: Atomic Pieces

**Not part of this plan - for later consideration.**

### The Issue

Automerge decomposes nested objects into field-level CRDT operations. Setting one piece generates 20-30 patches (one per field). This may cause overhead:
- On write: decomposing piece into many operations
- On read: collecting piece IDs from granular patches, then re-fetching complete pieces

### Potential Solution

Store pieces as JSON strings instead of nested objects:

```typescript
// Current (field-level CRDT):
d.pieces[id] = { id, label, mold, children, ... };  // 20-30 ops

// Alternative (atomic):
d.pieces[id] = JSON.stringify({ id, label, mold, children, ... });  // 1 op
```

Automerge's simple string type does NOT perform field-level merging - it uses last-writer-wins for concurrent modifications.

### Trade-offs

**Pros:**
- Fewer CRDT operations per piece update
- Simpler patch handling (patch.value is complete piece)
- Aligns Automerge granularity with Hazel's piece-as-atomic-unit model

**Cons:**
- Lose field-level conflict resolution (if two users edit same piece concurrently, one wins entirely)
- Requires schema migration

### Recommendation

For Hazel's use case, last-writer-wins at piece level is probably acceptable. The probability of two users editing the *same piece* at the *same time* is low. Worth investigating if performance becomes an issue.

See: [CloudKitchens blog on CRDT performance](https://techblog.cloudkitchens.com/p/protocol-buffer-crdts-outperforming) - achieved 4-5x memory reduction using atomic "REPLACE" semantics.

---

## References

- [Automerge Repo blog](https://automerge.org/blog/automerge-repo/)
- [Automerge Patches Rust Docs](https://automerge.org/automerge/automerge/patches/enum.PatchAction.html)
