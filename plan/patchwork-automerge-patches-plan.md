# Patchwork Automerge Patches Integration - Plan & Analysis

## Executive Summary

**Goal:** Eliminate manual TypeScript diffing by using Automerge's native patch events to send deltas to Hazel iframe.

**Status:** Implementation attempted but introduced regressions (echo loops, crashes, full state being sent repeatedly).

**Core Issue:** Hybrid approach using patches to identify changes but reading from stale `doc` closure caused timing issues.

---

## Original Working State (Before Automerge Patches Work)

### Sender Side (OCaml → Patchwork) ✅ WORKING
```
User types "1" → Hazel updates tree → Flattens to flat_doc → Computes delta → Sends to Patchwork
```

**Performance (1800-tile program, single character):**
- old_seg_to_doc: 3ms
- seg_to_doc: 5ms
- compute_delta: 3ms
- Delta: 1 changed, 2 added (6.83 KB payload)
- **Total: 13ms** ✅

**Code Location:** `src/haz3lcore/patchwork/SyncReplace.re`, `PatchworkComm.re`

### Receiver Side (Patchwork → Hazel) ❌ PROBLEMATIC
```
Remote edit → Automerge merges → tool.tsx sends FULL state → Hazel rebuilds tree
```

**Performance:**
- Sending: 1768 pieces, 443.92 KB (full state, not delta!)
- doc_to_seg: 1.4ms
- cursor_repositioning: 23.9ms (bottleneck)
- **Total: ~32ms**

**Code Location:** `patchwork-extra/hazel/src/tool.tsx`

**Original Implementation:**
```typescript
useEffect(() => {
  if (isUpdatingFromIframe.current) {
    console.log(`[TOOL] Skipping echo - change originated from iframe`);
    return;
  }

  // Send FULL state every time doc changes
  const hazelDoc = { title: doc.title, pieces: doc.pieces };
  sendToHazel.current({ t: "state", state: hazelDoc });
}, [doc]);
```

---

## The Problem Identified

**Question from user:** "But like, do we need to do the TypeScript diff? Like, does it make sense that the only thing Patchwork can do for a client is provide it with the full state?"

**Key Insight:** We were sending full state (443 KB) when only 3 pieces changed (6.83 KB delta). Automerge must already know what changed - can we use that?

---

## Research Findings: How Automerge Actually Works

### 1. DocHandle Change Events ✅

From [Automerge Repo blog](https://automerge.org/blog/automerge-repo/):
```javascript
handle.on("change", ({handle, patches, patchInfo}) => {
  // patches tell us EXACTLY what changed!
});
```

**Patch Structure:**
```typescript
{
  action: 'put' | 'insert' | 'del' | 'delete',
  path: ['pieces', 'uuid-123', ...],  // Path to changed value
  value: { ... }  // New value (for put/insert)
}
```

### 2. React Hooks Difference

**useDocument** (hides patches):
```typescript
const [doc, changeDoc] = useDocument(docUrl);
// Auto re-renders, but no access to patches
```

**useDocHandle** (exposes patches via events):
```typescript
const handle = useDocHandle(docUrl);
handle.on("change", listener);  // Get patches!
```

### 3. The Right Architecture

**Conceptual Model:**
- **Patchwork's job:** CRDT merging, provide deltas
- **Hazel's job:** Tree reconstruction, cursor positioning

**Proper flow:**
```
Automerge change event (remote edit)
  → Patches from Automerge
  → Convert to our delta format
  → Send delta to Hazel iframe
  → Hazel applies delta + rebuilds tree
```

---

## The Agreed Implementation Plan

**From conversation before compaction:**

```typescript
useEffect(() => {
  const listener = ({ patches }) => {
    // Automerge TELLS us what changed via patches!
    // No manual diffing needed

    // Convert Automerge patches to our delta format
    const changed = {};
    const added = {};
    const deleted = [];

    for (const patch of patches) {
      if (patch.path[0] === 'pieces') {
        const pieceId = patch.path[1];
        if (patch.action === 'put') {
          changed[pieceId] = patch.value;
        } else if (patch.action === 'insert') {
          added[pieceId] = patch.value;
        } else if (patch.action === 'del') {
          deleted.push(pieceId);
        }
      }
    }

    sendToHazel.current({
      t: "delta",
      changed,
      added,
      deleted
    });
  };

  handle.on("change", listener);
  return () => handle.off("change", listener);
}, [handle]);  // Only depend on handle, NOT doc!
```

**User Approval:** "Yes, let's try this out please."

---

## What We Actually Implemented (The Divergence)

### Critical Mistakes

**1. Added `doc` to dependencies:**
```typescript
}, [handle, doc, sendToHazel]);  // ❌ This causes re-run on every change!
```

**2. Read from closure instead of patches:**
```typescript
const currentPiece = doc.pieces[pieceId];  // ❌ `doc` from closure is stale!
if (currentPiece) {
  changed[pieceId] = currentPiece;
}
```

**3. Local `isInitialized` variable:**
```typescript
let isInitialized = false;  // ❌ Resets on every effect re-run!
if (!isInitialized) {
  sendToHazel.current({ t: "state", state: hazelDoc });
  isInitialized = true;
}
```

**Result:**
- Effect re-runs on every `doc` change
- Sends full initial state repeatedly
- `doc` in closure becomes stale
- Timing issues with `isUpdatingFromIframe` flag

### Symptoms Observed

1. **Echo loops:** Changes bouncing between clients
2. **Crashes:** `Cannot read properties of undefined (reading 'children')`
3. **Full state spam:** Initial state sent on every change instead of once
4. **Weird jumping:** Cursor repositioning with stale data

---

## Why The Hybrid Approach Failed

**The plan:** Use patches as source of truth

**What we did:** Use patches to know WHICH pieces changed, then read from `doc` for content

**Why it failed:**
- Patches arrive asynchronously
- `doc` in closure may be stale when patches fire
- Effect dependency on `doc` creates loop
- Patch `value` field already has the data we need!

---

## The Correct Implementation

### Key Principles

1. **Use patch `value` field directly** - don't read from `doc`
2. **Depend only on `handle`** - not `doc`
3. **Use ref for initialization** - not local variable
4. **Let Automerge be source of truth** - patches contain everything

### Proposed Fix

```typescript
// Outside component or as ref
const hasInitialized = React.useRef(false);

useEffect(() => {
  // Send initial state ONCE
  if (!hasInitialized.current) {
    const doc = handle.doc();
    sendToHazel.current({
      t: "state",
      state: { title: doc.title, pieces: doc.pieces }
    });
    hasInitialized.current = true;
  }

  const changeListener = ({ patches }) => {
    // Skip if change came from our iframe
    if (isUpdatingFromIframe.current) {
      isUpdatingFromIframe.current = false;  // Reset here
      return;
    }

    const changed = {};
    const added = {};
    const deleted = [];
    let titleChanged = false;

    for (const patch of patches) {
      const { action, path, value } = patch;

      if (path[0] === 'pieces' && path.length === 2) {
        // Only care about piece-level changes, not nested properties
        const pieceId = path[1];

        if (action === 'del' || action === 'delete') {
          deleted.push(pieceId);
        } else if (action === 'insert') {
          added[pieceId] = value;  // ✅ Use value from patch!
        } else if (action === 'put') {
          changed[pieceId] = value;  // ✅ Use value from patch!
        }
      } else if (path[0] === 'title' && path.length === 1) {
        titleChanged = true;
      }
    }

    // Only send if something changed
    if (Object.keys(changed).length > 0 ||
        Object.keys(added).length > 0 ||
        deleted.length > 0 ||
        titleChanged) {
      sendToHazel.current({
        t: "delta",
        changed,
        added,
        deleted,
        title: titleChanged ? handle.doc().title : undefined
      });
    }
  };

  handle.on("change", changeListener);

  return () => {
    handle.off("change", changeListener);
  };
}, [handle]);  // ✅ Only depend on handle!
```

### For echo prevention

```typescript
case "delta": {
  isUpdatingFromIframe.current = true;

  handle.change((d) => {
    // Apply changes...
  });

  // Reset flag in next microtask to ensure it happens after
  // the change event fires (if it's synchronous)
  queueMicrotask(() => {
    isUpdatingFromIframe.current = false;
  });

  break;
}
```

---

## Open Questions

### 1. Do patches contain full piece values?

**Need to verify:** When Automerge fires a patch with `action: 'put'`, does `patch.value` contain the complete piece object, or just the changed field?

**If patches are granular** (e.g., `path: ['pieces', 'id-123', 'label']`):
- We'd need to read from `handle.doc().pieces[pieceId]` after all
- But we should call `handle.doc()` inside the listener, not use closure

**If patches are piece-level** (e.g., `path: ['pieces', 'id-123']`):
- `patch.value` has the full piece
- We can use it directly ✅

### 2. Nested property updates

Current code checks `path.length === 2` to only handle whole-piece updates. But what if Automerge sends:
- `path: ['pieces', 'id-123', 'label']` for label change?
- `path: ['pieces', 'id-123', 'children', 0]` for child array update?

We need to track which piece IDs appear in patches, then fetch the complete piece.

**Revised approach:**
```typescript
const affectedPieceIds = new Set();

for (const patch of patches) {
  if (patch.path[0] === 'pieces' && patch.path.length >= 2) {
    affectedPieceIds.add(patch.path[1]);
  }
}

const doc = handle.doc();  // Get fresh doc INSIDE listener
for (const pieceId of affectedPieceIds) {
  if (doc.pieces[pieceId]) {
    changed[pieceId] = doc.pieces[pieceId];
  } else {
    deleted.push(pieceId);
  }
}
```

This handles nested updates correctly while avoiding stale closure.

---

## Performance Expectations

**If implemented correctly:**

Sender side: (already working)
- 13ms total ✅

Receiver side: (target)
- Automerge patches event: ~0ms (event listener)
- Convert patches to delta: ~1ms
- Send delta to iframe: ~1ms
- Hazel receives delta: ~1ms
- Flatten existing tree: ~3ms (currently in send_state)
- Apply delta to flat doc: ~1ms
- Rebuild tree: ~1-2ms
- Cursor repositioning: ~24ms (bottleneck, unchanged)
- **Total: ~32ms** (no improvement in total time, but cleaner architecture)

**Key insight:** The bottleneck is cursor_repositioning (24ms), not the data transfer. This implementation won't speed things up much, but it will:
- ✅ Reduce payload size (443 KB → ~7 KB)
- ✅ Use Automerge properly
- ✅ Clean architectural separation

---

## Rollback Plan

**Current unstaged changes:**
- `src/haz3lcore/patchwork/PatchworkComm.re` (OCaml side - keep this, it's working)
- `patchwork-extra/hazel/src/tool.tsx` (TypeScript side - revert this)

**To rollback:**
```bash
cd /Users/andrewblinn/Dropbox/projects/patchwork-extra/hazel
git restore src/tool.tsx
```

**Then re-implement properly using the "Correct Implementation" above.**

---

## Testing Checklist

When implementing the fix:

- [ ] Single client: Type rapidly, verify no echo loops
- [ ] Two clients: Type in one, verify other updates correctly
- [ ] Two clients: Type rapidly in both, verify no crashes
- [ ] Console logs: Verify "Received N patches" matches expected changes
- [ ] Console logs: Verify "Initial state sent" appears only ONCE per page load
- [ ] Console logs: No "Cannot read properties of undefined" errors
- [ ] Payload size: Verify deltas are small (< 10 KB for typical edits)
- [ ] Performance: Cursor repositioning still ~24ms (bottleneck)

---

## References

- [Automerge Repo blog](https://automerge.org/blog/automerge-repo/)
- [React Integration Tutorial](https://automerge.org/docs/tutorial/react/)
- [Automerge Repo 2.0](https://automerge.org/blog/automerge-repo-2/)
- Conversation transcript: `/Users/andrewblinn/.claude/projects/-Users-andrewblinn-Dropbox-projects-hazel/4ea5e03a-f8a4-45f8-8311-fc4e77774f5a.jsonl`

---

## Next Steps

1. **Decide:** Attempt fix or rollback completely?
2. **If fixing:** Verify patch structure (granular vs piece-level)
3. **Implement:** Use correct approach from "Proposed Fix" section
4. **Test:** Follow testing checklist
5. **Profile:** Verify performance is as expected

**Key Question:** Do we feel confident the patch structure will give us what we need, or should we test Automerge behavior first in a minimal reproduction?
