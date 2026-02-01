# Hazel-Patchwork Integration

This document describes how Hazel integrates with Patchwork as an embeddable collaborative editor.

## Architecture Overview

The integration uses an **iframe-based architecture** with three parts:

```
┌─────────────────────────────────────────────┐
│  Patchwork (patchwork-extra/hazel)          │
│  - tool.tsx wraps HazelEmbed                │
│  - Automerge handles multi-user sync        │
│  - Stores flattened HazelDoc in Automerge   │
└─────────────────────────────────────────────┘
              ↓ PostMessage ↓
┌─────────────────────────────────────────────┐
│  Hazel iframe                               │
│  (loaded from hazel.org/build/patchwork/)   │
│  - PatchworkComm.re handles messages        │
│  - SyncReplace.re applies remote changes    │
│  - FlatConvert.re converts tree↔flat        │
└─────────────────────────────────────────────┘
```

### The Three Parts

1. **`embed/` directory (this repo)** - npm package `@hazelgrove/hazel-embed`
   - `HazelEmbed.tsx` - React component that creates the iframe and handles PostMessage
   - Verifies message source to only accept messages from its own iframe
   - `flatdoc.d.ts` / `patchworkmessages.d.ts` - TypeScript types (source of truth for OCaml types)
   - Used by any app that wants to embed Hazel with sync support

2. **`src/haz3lcore/patchwork/` (this repo)** - OCaml code running inside the iframe
   - `PatchworkComm.re` - PostMessage communication and type conversion
   - `FlatConvert.re` - Converts between Hazel's tree AST and flat sync format
   - `SyncReplace.re` - Applies remote state while preserving caret position

3. **`patchwork-extra/hazel` (separate repo)** - The Patchwork "tool"
   - `tool.tsx` - Wraps HazelEmbed, connects to Automerge
   - Handles multi-user sync via Automerge CRDT

## Key Concept: Tree vs Flat Representation

Hazel's internal AST is a **nested tree** (`Segment`), but Automerge works best with **flatter structures**. The `FlatConvert.re` module converts between:

- **Segment** - Hazel's internal nested tree with tiles containing child tiles
- **HazelDoc** - Flat **map** of pieces (keyed by UUID) with UUID-based children references

The map-based schema (changed from array in Jan 2026) enables O(1) Automerge updates instead of O(n) array diffing.

## Message Protocol

Communication uses PostMessage with these message types:

| Message | Direction        | Purpose                                      |
| ------- | ---------------- | -------------------------------------------- |
| `init`  | Iframe → Parent  | Iframe signals readiness, triggers state send |
| `ping`  | Both             | Connection testing                           |
| `pong`  | Both             | Ping response                                |
| `state` | Both             | Document sync (full state or delta)          |

### Initialization Sequence

When the Hazel iframe loads:

```
1. Hazel iframe loads and JavaScript executes
2. init_iframe() sets up message listener
3. init_iframe() sends "init" to parent
4. tool.tsx receives "init" → sends full document state to iframe
5. Hazel receives state → applies via SyncReplace action
```

This one-way handshake ensures the iframe only receives state after its message listener is ready. The parent waits for the iframe to signal readiness rather than guessing when to send.

**Key design point:** The `init` message is only sent from iframe to parent. The parent does NOT send `init` to the iframe—it responds with `state` instead.

## Caret Position Sync

Remote users' caret positions are synced in real-time using Automerge's ephemeral broadcast mechanism. This is **view-layer state only** - caret positions are NOT persisted to the document.

### Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│  Hazel iframe (User A)                                              │
│  - User moves caret                                                 │
│  - SyncReplace.send_caret() extracts (pieceId, caret) from zipper   │
│  - PatchworkComm.send_caret() posts message to parent               │
└─────────────────────────────────────────────────────────────────────┘
                    ↓ PostMessage {t: "caret", pieceId, caret, shape}
┌─────────────────────────────────────────────────────────────────────┐
│  tool.tsx (Patchwork)                                               │
│  - Receives caret message from iframe                               │
│  - Broadcasts via handle.broadcast([userId, caretState])            │
│  - Listens for "ephemeral-message" events from other peers          │
│  - Forwards remote carets to iframe                                 │
└─────────────────────────────────────────────────────────────────────┘
                    ↓ handle.broadcast() / ephemeral-message
┌─────────────────────────────────────────────────────────────────────┐
│  tool.tsx (Other peer - User B)                                     │
│  - Receives ephemeral message with User A's caret                   │
│  - Assigns color based on senderId hash                             │
│  - Posts {t: "remote-caret", ...} to iframe                         │
└─────────────────────────────────────────────────────────────────────┘
                    ↓ PostMessage
┌─────────────────────────────────────────────────────────────────────┐
│  Hazel iframe (User B)                                              │
│  - PatchworkComm.re stores remote caret in StringMap                │
│  - Triggers UpdateRemoteCarets action                               │
│  - RemoteCaretDec.re renders colored caret at piece position        │
└─────────────────────────────────────────────────────────────────────┘
```

### Ephemeral Broadcast Protocol

The Automerge `DocHandle` provides ephemeral messaging:

- `handle.broadcast(message)` - sends to all peers, not persisted
- `handle.on("ephemeral-message", callback)` - receives from peers

Messages use awareness protocol format: `[userId, state]`

Each peer is identified by `senderId` (unique per browser session, e.g., `frontend-817`), which is used to distinguish different users even if they share the same Patchwork account.

### Message Types

| Message               | Direction      | Purpose                       |
| --------------------- | -------------- | ----------------------------- |
| `caret`               | Hazel → Parent | Local caret position changed  |
| `remote-caret`        | Parent → Hazel | Another user's caret position |
| `remote-caret-remove` | Parent → Hazel | User disconnected             |

### Key Files

| File                                               | Purpose                                                               |
| -------------------------------------------------- | --------------------------------------------------------------------- |
| `patchwork-extra/hazel/src/tool.tsx`               | Broadcasts local caret, receives remote carets via ephemeral messages |
| `src/haz3lcore/patchwork/PatchworkComm.re`         | Sends caret to parent, stores received remote carets                  |
| `src/haz3lcore/patchwork/SyncReplace.re`           | Extracts caret position from zipper, decides when to send             |
| `src/web/app/editors/decoration/RemoteCaretDec.re` | Renders remote carets with custom colors                              |
| `src/web/app/editors/code/CodeEditable.re`         | Includes remote carets in editor decorations                          |

### Caret Position Model

The caret position sent over the wire uses this model:

1. **Piece selection**: The caret is always "on" the **first piece of right siblings** in the zipper. At the end of a segment, it's on the last piece of left siblings.

2. **Offset encoding**: `caretOffset` is 0 for `Outer` (at piece's left edge), or `n+1` for `Inner(n)` (position n inside the piece).

3. **Shape**: At `Outer` positions, the caret shape (convex/concave) is derived from `Zipper.Caret.direction`. For `Inner` positions, shape is always `null`.

4. **Side**: Which edge of the piece the caret is on:
   - `"left"` = caret is at the left edge of the piece (normal case, piece is to the right of caret)
   - `"right"` = caret is at the right edge of the piece (end-of-segment case, piece is to the left of caret)
   - `null` = caret is inside the piece (`Inner` position)

**Example** (whitespace shown as `·`):

```
foo · 333 · bar
```

Segment: `[foo, ·, 333, ·, bar]`

| Visual        | Piece you're on  | caretOffset | side    |
| ------------- | ---------------- | ----------- | ------- |
| `│foo · 333`  | `foo`            | 0           | `left`  |
| `foo│· 333`   | `·` (whitespace) | 0           | `left`  |
| `foo ·│333`   | `333`            | 0           | `left`  |
| `foo ·3│33`   | `333`            | 1           | `null`  |
| `foo ·33│3`   | `333`            | 2           | `null`  |
| `foo ·333│·`  | `·` (whitespace) | 0           | `left`  |
| `foo · 333│`  | `333` (last)     | 0           | `right` |

## Data Flow

### Initialization (on iframe load):

1. Hazel iframe loads, `init_iframe()` executes
2. Sends `init` message to parent
3. `tool.tsx` receives `init`, sends **full document state**
4. Hazel applies state via `SyncReplace` action

### Local edit → sync to others:

1. User types in Hazel iframe
2. `SyncReplace.send_state()` called after action completes
3. `FlatConvert.seg_to_doc()` converts both old and new Segment → flat Doc
4. `PatchworkComm.compute_delta()` finds changed, added, and **deleted** pieces
5. Delta sent via PostMessage: affected pieces + deleted IDs
6. Parent's `tool.tsx` updates Automerge: adds/changes pieces, **deletes removed pieces**
7. Automerge syncs to other clients

### Remote edit → apply locally:

1. Automerge receives update from another client
2. `tool.tsx` `useEffect[doc]` detects change, computes delta
3. Sends only affected pieces to iframe via PostMessage
4. `PatchworkComm.listen()` receives delta message
5. `SyncReplace.sync_replace()` merges delta with current state (delta overrides)
6. `FlatConvert.doc_to_seg()` converts merged Doc → Segment
7. Editor re-renders with preserved caret position

**Note:** The `useEffect[doc]` in tool.tsx only handles changes AFTER initialization. Initial state is sent exclusively through the `init` handler to ensure the iframe is ready to receive it.

## Explicit Deletion Sync

### The Problem: Tree vs Flat Map Mismatch

Hazel internally uses a **tree** structure where pieces are nested inside parent tiles. When a piece is deleted (e.g., cut), it simply disappears from the tree—there's no trace of it.

Automerge stores a **flat map** of all pieces, keyed by UUID. If we only send changed/added pieces, deleted pieces remain in Automerge as "orphans"—still present but unreferenced by any parent.

This mismatch causes problems for undo/redo sync:

1. User A cuts a piece → piece becomes orphan in Automerge (still exists, just unreferenced)
2. User A undoes → piece is "restored" in Hazel's tree
3. When syncing to User B: the parent tile changed (its children array now references the piece again), but the piece itself is unchanged in Automerge
4. tool.tsx compares with previous Automerge state, sees no change to the piece, doesn't forward it
5. User B's Hazel crashes: parent references a piece that wasn't included in the delta

### The Solution: Explicit Deletion

The `state` message includes a `deleted` field listing piece IDs to remove from Automerge:

```typescript
interface EditorState {
  t: "state";
  state: HazelDoc;      // Changed/added pieces
  deleted?: string[];   // IDs of pieces to remove
}
```

This keeps Automerge in sync with Hazel's tree: when pieces are deleted in Hazel, they're also deleted from Automerge. Undo then "re-adds" them as genuinely new pieces, triggering proper delta detection.

### How Deletion is Computed

In `PatchworkComm.re`, `compute_delta` compares old and new flat docs:

```reason
// Find deleted pieces
old_doc
|> Id.Map.iter((id, _) => {
     switch (Id.Map.find_opt(id, new_doc)) {
     | None => deleted := [id, ...deleted^]  // In old, not in new → deleted
     | Some(_) => ()
     }
   });
```

The key insight: `FlatConvert.seg_to_doc` only includes pieces **reachable** from the tree root. When you cut something, it's removed from the segment, so it's not in the new flat doc, so it appears in `deleted`.

## Echo Loop Prevention

When the local user edits, Hazel sends state to Patchwork, which updates Automerge, which triggers a change event. Without protection, this change event would send state back to the originating iframe, causing an echo loop with symptoms like:
- Rapid typing causes edits to appear out of order
- Cursor jumps unexpectedly
- `sync_replace` called on the sender (should only happen on receiver)

The fix uses `isUpdatingFromIframe` ref in tool.tsx:
1. Set flag `true` before `handle.change()`
2. Check flag at start of change listener - if true, skip and reset flag
3. Use `queueMicrotask` to reset flag after async change events

If echo symptoms recur, check that all three parts are in place in tool.tsx.

## Directory Structure

### Hazel repo (this repo)

```
src/haz3lcore/patchwork/
├── PatchworkComm.re       # PostMessage communication, type conversion
├── SyncReplace.re         # Apply remote state while preserving caret
├── FlatConvert.re         # Convert Segment ↔ flat Doc
├── FlatTypes.re           # Flat piece types and Doc module (breaks dependency cycle)
├── FlatDoc.mli            # OCaml types for flat document (generated)
├── PatchworkMessages.mli  # OCaml types for messages (generated)
└── dune                   # Build rules

embed/
├── src/
│   ├── components/
│   │   └── HazelEmbed.tsx # React component for embedding
│   ├── types/
│   │   ├── flatdoc.d.ts   # TypeScript types (source of truth)
│   │   └── patchworkmessages.d.ts
│   └── index.ts           # Package exports
└── package.json           # npm package config
```

### patchwork-extra/hazel

```
src/
├── tool.tsx      # Patchwork tool UI, wraps HazelEmbed
├── datatype.ts   # Automerge schema for HazelDoc
└── index.ts      # Plugin registration
```

## Type Generation

TypeScript types in `embed/src/types/` are the **source of truth**. OCaml types are generated from them.

### Regenerating OCaml Types

After modifying `.d.ts` files:

```bash
cd embed
pnpm type:flatdoc           # flatdoc.d.ts → patchwork/FlatDoc.mli
pnpm type:patchworkmessages # patchworkmessages.d.ts → patchwork/PatchworkMessages.mli
pnpm type                   # Both at once
```

After regenerating, update `PatchworkComm.re` if you added/removed/renamed fields (the `JsConvert` module has manual conversion code).

### Variant Tag Maintenance

**Known issue:** ts2ocaml generates variant names alphabetically (e.g., `U_s5_Grout`, `U_s7_Projector`). Adding a new type can shift all the tags, requiring updates throughout `PatchworkComm.re`.

**Alternative approach:** Instead of pattern matching on generated variants, match on the JS discriminator string directly:

```reason
let to_flat_piece = (js_obj: Ojs.t): FlatConvert.Flat.piece => {
  let t = Ojs.get_prop_ascii(js_obj, "t") |> Ojs.string_of_js;
  switch (t) {
  | "Tile" => Tile(to_tile(FlatDoc.FlatTile.t_of_js(js_obj)))
  | "Grout" => Grout(to_grout(FlatDoc.Grout.t_of_js(js_obj)))
  // ... etc
  };
};
```

**Tradeoff:** String matching is stable (no cascading renames) but loses compile-time exhaustiveness checking - missing cases become runtime errors instead of compile errors.

## Patchwork-Specific Behavior

When Hazel runs inside Patchwork (detected via `PatchworkComm.is_in_iframe()`), certain behaviors change:

- **localStorage disabled**: Editor content is NOT saved to localStorage (Automerge handles persistence)
- **Settings still saved**: User settings (theme, etc.) still use localStorage
- **Mode switcher hidden**: Only Scratch mode is available (other modes don't sync via Automerge)
- **Sync enabled**: Caret and state sync only operate when in iframe mode

## URL Configuration

The Hazel iframe can load from:

- **Local**: `http://localhost:8001/` (local dev server)
- **Remote**: `https://hazel.org/build/patchwork/` (hosted build)

The tool.tsx UI includes "Local" and "Remote" buttons to switch between these.

## Building

### Hazel (this repo)

```bash
make deps
make dev  # or make release for production
```

### embed/ package (this repo)

```bash
cd embed
pnpm install
pnpm build
```

### patchwork-extra/hazel

**Important:** Requires pnpm 9.x (lockfile is v9.0 format).

```bash
cd /path/to/patchwork-extra/hazel

# Install (use pnpm 9)
npx pnpm@9 install --frozen-lockfile

# Build and deploy
npx pnpm@9 build
patchwork push
```

**Do NOT modify `package.json` dependencies** - the GitHub subdirectory syntax requires pnpm 9.x and the lockfile pins working vite versions.

## Current Limitations

- Caret position can be disrupted when subterm containing caret is deleted
- No divergence recovery mechanism (if clients diverge, delta sync won't reconcile)
- Anonymous users may briefly see duplicate carets on refresh (until 3-minute timeout cleanup)

### patchwork-extra/hazel Dependency

The `side` field in caret messages must be forwarded by `tool.tsx` in `patchwork-extra/hazel`. When updating the Hazel embed package, ensure that `tool.tsx`:

1. Forwards the `side` field when broadcasting caret updates via `handle.broadcast()`
2. Includes the `side` field when sending `remote-caret` messages to iframes

If the `side` field is not forwarded, remote carets at end-of-segment positions will render incorrectly (at the left edge of the last piece instead of the right edge).

## Projector Sync

Projectors are synced between collaborators, including their wrapped syntax and model state.

### What's Synced

- Projector placement (id, kind, wrapped syntax)
- Projector model state (as opaque string)
- All projector kinds are synced (Fold, Checkbox, Slider, etc.)
- Refractors (Probe, Statics) are NOT synced - they're per-user debugging tools

### Model Sync Note

The projector `model` field is synced as an opaque string. To disable model sync (keep models local-only while still syncing projector placements):

1. In `FlatConvert.re`, `seg_to_doc`: change `model` to `model: ""`
2. In `FlatConvert.re`, `doc_to_seg`: preserve local model instead of using remote

This would prevent remote state from overwriting local projector state, but the projector placement itself would still sync.

## Future Work

### Caret Sync Improvements

- **Debounce outgoing caret messages** (50ms threshold)
- **Sync selection ranges** (highlight what others have selected)
- ~~**Clean up stale remote carets on peer disconnect**~~: DONE. Remote carets are now keyed by persistent `userId` (from the broadcast message) instead of ephemeral `senderId`. This ensures logged-in users who refresh replace their old caret rather than creating a duplicate. Additionally, carets not updated within 3 minutes are automatically removed via timeout cleanup. Note: Anonymous users (who get a new random ID each session) may still briefly see duplicate carets until the stale one times out.
- ~~**Optimize remote caret forwarding in tool.tsx**~~: DONE. Carets are now forwarded directly to the iframe in the ephemeral message handler, avoiding the previous pattern of storing all carets in state and re-forwarding all on every change. State is still maintained for the iframe init/reload case.

### Performance

- **Cache old flat doc**: Currently `old_zipper → flat_doc` conversion happens on every send. Store last sent flat_doc in syntax cache to eliminate one `seg_to_doc` call per edit.
- **Profile and optimize `FlatConvert`** for large documents
- **Dirty-tracking instead of full diff**: Track which pieces changed during edit, send O(k) delta where k = changed pieces
- **Cursor repositioning O(n) → O(log n)**: After receiving remote edit, `move_to_id` does linear scan from document start. Build ID→path index during `doc_to_seg`, then construct zipper directly at target position.

### Sync Lifecycle & Recovery

- **Preserve local selection during sync**: Currently, incoming remote edits wipe out the local user's selection. The `sync_replace` routine restores cursor position but not selection state. We should save the selection content/range before applying remote changes and restore it afterward (adjusting for any structural changes that may have affected the selected region).
- **Divergence recovery**: If clients diverge (network partition, bugs), delta sync won't reconcile them. Options: periodic full sync, checksum comparison, manual resync button.
- **Full-replace mode for initial state**: Current SyncReplace always merges; initial load might benefit from full replacement.

### Security

- **Restrict PostMessage origin** (currently uses `"*"`)
- **Validate incoming messages more strictly**

### Atomic Pieces (Automerge Optimization)

Automerge decomposes nested objects into field-level CRDT operations. Setting one piece generates 20-30 patches (one per field). A potential optimization is to store pieces as JSON strings:

```typescript
// Current (field-level CRDT):
d.pieces[id] = { id, label, mold, children, ... };  // 20-30 ops

// Alternative (atomic):
d.pieces[id] = JSON.stringify({ id, label, mold, children, ... });  // 1 op
```

Trade-off: Lose field-level conflict resolution (concurrent edits to same piece → last-writer-wins), but this is likely acceptable since the probability of two users editing the same piece simultaneously is low. See [CloudKitchens blog](https://techblog.cloudkitchens.com/p/protocol-buffer-crdts-outperforming) for a similar approach.
