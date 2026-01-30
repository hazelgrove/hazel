# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and usage documentation, see `docs/patchwork-integration.md`.

## Caret Sync

Show collaborators' cursor positions in real-time.

### Current Status: Basic Infrastructure Complete ✓

The basic caret sync infrastructure is **implemented and working**:
- [x] Ephemeral broadcast via `handle.broadcast()` / `ephemeral-message` events
- [x] Message types defined (CaretUpdate, RemoteCaret, RemoteCaretRemove)
- [x] tool.tsx broadcasts local caret, receives and forwards remote carets
- [x] PatchworkComm.re sends caret updates, stores received remote carets
- [x] RemoteCaretDec.re renders remote carets with user-specific colors
- [x] User identification via senderId (unique per browser session)

**However, caret positioning is buggy.** See "Caret Position Fix Plan" below.

### Overview

Multiple users viewing the same Hazel document should see each other's caret positions rendered in different colors. This is purely view-layer state and should NOT be stored in the persistent document.

**Scope**: Caret position only (pieceId + offset). Selection ranges are out of scope for now.

### Recommended Approach: Automerge Ephemeral Broadcast

Patchwork/Automerge has a built-in side channel for exactly this use case:

- `DocHandle.broadcast(message)` sends ephemeral data to all peers
- Peers receive via `handle.on("ephemeral-message", callback)`
- Messages are **not persisted** to the document
- Patchwork provides React hooks: `useLocalAwareness` and `useRemoteAwareness`

This was confirmed by pvh and Alex Good on the IncanSwitch Discord:
> "try .broadcast()" / "DocHandle.broadcast to send data, and DocHandle.on('ephemeral-message', (msg) => { .. })"

**Why not store carets in the document?**
- Pollutes document history with transient cursor movements
- Creates unnecessary sync overhead
- Goes against Automerge best practices for ephemeral state

### Architecture

```
Hazel iframe (OCaml/JS)
    ↓ PostMessage {t: "caret", pieceId, caretOffset}
tool.tsx (Patchwork tool wrapper)
    ↓ useLocalAwareness → handle.broadcast({userId, caret})
────── network (ephemeral, not persisted) ──────
    ↓ useRemoteAwareness ← handle.on("ephemeral-message")
tool.tsx (other peer)
    ↓ PostMessage {t: "remote-caret", userId, color, pieceId, caretOffset}
Hazel iframe (other peer)
    ↓ render remote caret via RemoteCaretDec
```

### User Identification

Patchwork provides user identity via `useCurrentAccount()` hook (from `@patchwork/sdk`):
- Returns an `Account` with `contactHandle.url` as a stable user ID
- `ContactDoc` has optional `name` field for registered users

**Implementation approach:**
```typescript
const account = useCurrentAccount();
const userId = account ? account.contactHandle.url : `anon-${crypto.randomUUID()}`;
```

**Color assignment:**
- Generate deterministic color from userId hash (e.g., HSL with hue = hash % 360)
- Or use a predefined palette cycling through users
- Example palette: `["#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1"]`

### Debouncing Strategy

Caret updates should be debounced to avoid flooding the network:
- **Threshold**: 50ms debounce (responsive but not excessive)
- **Leading edge**: Send immediately on first movement, then debounce subsequent
- Alternatively, throttle to max 20 updates/second

### Protocol Changes

#### New message types in `embed/src/types/patchworkmessages.d.ts`:

```typescript
// Hazel → Parent: local caret moved
interface CaretUpdate {
  t: "caret";
  pieceId: string;
  caretOffset: number;  // 0 = Outer, n = Inner(n-1)
}

// Parent → Hazel: remote user's caret position
interface RemoteCaret {
  t: "remote-caret";
  userId: string;
  color: string;        // CSS color for rendering
  pieceId: string;
  caretOffset: number;
}

// Parent → Hazel: remote user disconnected
interface RemoteCaretRemove {
  t: "remote-caret-remove";
  userId: string;
}

// Update union types:
type HazelToParent = Init | Ping | Pong | EditorState | CaretUpdate;
type ParentToHazel = Init | Ping | Pong | EditorState | RemoteCaret | RemoteCaretRemove;
```

After updating TypeScript types, regenerate OCaml:
```bash
cd embed && pnpm type:patchworkmessages
```

### Implementation Tasks

#### 1. TypeScript Side (patchwork-extra/hazel/src/tool.tsx) ✓ DONE

- [x] Import `useCurrentAccount` from `@patchwork/sdk`
- [x] Set up user ID from account (falls back to anonymous ID)
- [x] Generate/assign user color (deterministic hash-based)
- [x] Handle incoming "caret" messages from iframe → broadcast via `handle.broadcast()`
- [x] Listen for `ephemeral-message` events → receive remote carets
- [x] Forward remote carets to iframe via PostMessage

**Note**: Uses `handle.broadcast()` / `ephemeral-message` directly instead of `useLocalAwareness`/`useRemoteAwareness` hooks. Uses `senderId` from ephemeral events to distinguish users.

**Actual implementation** in `patchwork-extra/hazel/src/tool.tsx`:
- `broadcastCaret()` calls `handle.broadcast([userId, {pieceId, offset}])`
- `useEffect` sets up `handle.on("ephemeral-message", ...)` listener
- Listener extracts caret from `[userId, state]` format, keys by `event.senderId`
- Another `useEffect` forwards `remoteCarets` state to iframe

#### 2. OCaml Side: Send local caret updates ✓ DONE (but buggy)

**File: `src/haz3lcore/patchwork/PatchworkComm.re`**

- [x] Add `send_caret` function to broadcast caret position
- [x] Call `send_caret` when caret moves (via SyncReplace.send_caret after actions)
- [ ] Debounce outgoing caret messages (50ms) - NOT YET IMPLEMENTED

**Bug**: Currently uses `Indicated.piece()` which filters out whitespace. See "Caret Position Fix Plan" for fix.

**Actual implementation**: `SyncReplace.get_caret_position()` and `SyncReplace.send_caret()` extract caret info and post to parent via `PatchworkComm.send_caret()`.

#### 3. OCaml Side: Receive and store remote carets ✓ DONE

**File: `src/haz3lcore/patchwork/PatchworkComm.re`**

- [x] Add `remote_carets: ref(StringMap.t(remote_caret))` state
- [x] Handle "remote-caret" message: store/update in map
- [x] Handle "remote-caret-remove" message: remove from map
- [x] Expose `get_remote_carets()` getter for view layer
- [x] Trigger `UpdateRemoteCarets` action on receipt

**Actual implementation**: `PatchworkComm.re` defines `remote_caret` type, `remote_carets` ref, handles messages in `listen()`, exposes `get_remote_carets()`.

#### 4. View Layer: Render remote carets ✓ DONE (but buggy positioning)

**New file: `src/web/app/editors/decoration/RemoteCaretDec.re`** - CREATED

- [x] Created `RemoteCaretDec.re` with color parameter and no blink
- [x] Uses same caret path rendering as CaretDec
- [x] CSS styling in `editor.css` (`.remote-caret`, `.remote-caret-path`)

**File: `src/web/app/editors/code/CodeEditable.re`** - UPDATED

- [x] Added `RemoteCaretDec.view_all()` call in `deco` function

**Bug**: Current positioning logic is binary (origin vs last). See "Caret Position Fix Plan" for fix.

### File Summary

| File | Status | Changes |
|------|--------|---------|
| `embed/src/types/patchworkmessages.d.ts` | ✓ Done | Added CaretUpdate, RemoteCaret, RemoteCaretRemove types |
| `src/haz3lcore/patchwork/PatchworkMessages.mli` | ✓ Done | Regenerated from TypeScript |
| `src/haz3lcore/patchwork/PatchworkComm.re` | ✓ Done | send_caret, receive remote carets, state storage |
| `src/haz3lcore/patchwork/SyncReplace.re` | ✓ Done | get_caret_position, send_caret, should_send_caret |
| `src/haz3lcore/zipper/action/Action.re` | ✓ Done | Added UpdateRemoteCarets action |
| `src/haz3lcore/zipper/action/Perform.re` | ✓ Done | Handle UpdateRemoteCarets action |
| `patchwork-extra/hazel/src/tool.tsx` | ✓ Done | Ephemeral broadcast/receive, message bridging |
| `src/web/app/editors/decoration/RemoteCaretDec.re` | ✓ Done | New file for remote caret rendering |
| `src/web/app/editors/code/CodeEditable.re` | ✓ Done | Add remote carets to deco function |
| `src/web/www/style/editor.css` | ✓ Done | CSS for .remote-caret styling |

### Reference: Existing Code Locations

- **CaretDec.view**: `src/web/app/editors/decoration/CaretDec.re:40-66`
- **deco function**: `src/web/app/editors/code/CodeEditable.re:231`
- **Zipper caret type**: `src/haz3lcore/zipper/ZipperBase.re` (`Outer | Inner(int)`)
- **Indicated.piece**: Gets current piece from zipper
- **PatchworkComm.listen**: `src/haz3lcore/patchwork/PatchworkComm.re:204-244`
- **tool.tsx onMessage**: `patchwork-extra/hazel/src/tool.tsx:30-54`
- **TLDraw presence example**: `/patchwork/packages/tldraw/src/vendor/automerge-tldraw/useAutomergeStore.ts:128-208`
- **useLocalAwareness**: `/patchwork/node_modules/@automerge/automerge-repo-react-hooks/`
- **Account/userId**: `/patchwork/sdk/src/account.ts` - `useCurrentAccount()` returns `Account` with `contactHandle.url`

### Testing

1. Open same Hazel doc in two browser windows/tabs
2. Move caret in window A
3. Verify window B shows remote caret at correct position with different color
4. Verify window A's local caret is unaffected
5. Close window A, verify window B removes the remote caret
6. Test rapid caret movement (debouncing works)
7. Test caret on various piece types (tiles, grout, secondary)

---

## Caret Position Fix Plan

**Status: Not yet implemented**

The current implementation has bugs with caret positioning. This section documents the root cause analysis and fix plan.

### Observed Issues

1. **Inner positions not working**: Remote carets only appear at piece boundaries (start/end), not at positions within multi-character tokens. When moving through `333`, the remote caret jumps to the end, then back to the beginning.

2. **Whitespace doesn't show caret**: Moving through secondary/whitespace pieces doesn't update the remote caret at all.

3. **Shape always straight**: Remote carets are always rendered with straight shape, should change to convex/concave at piece boundaries.

### Root Cause Analysis

#### Problem 1: Wrong piece selection on sender

**Current code** (SyncReplace.re) uses `Indicated.piece(z)` which:
- Filters out secondary and grout pieces (`~ign=p => Piece.(is_secondary(p) || is_grout(p))`)
- Returns `None` when in whitespace → no caret message sent

**Fix**: Use the first piece of right siblings directly, which includes all piece types:
```reason
switch (z.relatives.siblings) {
| (_, [piece, ..._]) => Some((Piece.id(piece), z.caret))
| ([_, ..._] as left, []) => Some((Piece.id(ListUtil.last(left)), z.caret))
| _ => None
}
```

#### Problem 2: Binary positioning on receiver

**Current code** (RemoteCaretDec.re) treats offset as binary:
```reason
let origin = caret_offset == 0 ? measurement.origin : measurement.last;
```
This maps ALL non-zero offsets to the piece's right edge.

**Fix**: Always position relative to `origin`, add offset for inner positions:
```reason
let col_offset = switch (caret) {
  | Outer => 0
  | Inner(n) => n + 1
};
let position = {row: origin.row, col: origin.col + col_offset};
```

### Conceptual Model: Zipper Caret Position

Understanding Hazel's caret model is key to the fix:

1. **Segment partitioning**: The zipper partitions a segment into left siblings and right siblings. Movement transfers pieces between them.

2. **"The piece you're on"**: By convention, you're always "on" the **first piece of right siblings**. When you're at the end of a segment, you're on the last piece of left siblings.

3. **Caret type**:
   - `Outer` = at the left edge of the piece (the boundary before it starts)
   - `Inner(n)` = at position n inside the piece (0-indexed)

4. **No "outer-left" vs "outer-right"**: When you're at the right edge of a piece, you're actually `Outer` on the NEXT piece, not "outer-right" on the current piece.

**Example** (whitespace shown as `·`):
```
foo · 333 · bar
```
Segment: `[foo, ·, 333, ·, bar]`

| Caret visual | Left siblings | Right siblings | Piece you're on | Caret |
|--------------|---------------|----------------|-----------------|-------|
| `foo│· 333` | `[foo]` | `[·, 333, ·, bar]` | `·` | Outer |
| `foo ·│333` | `[foo, ·]` | `[333, ·, bar]` | `333` | Outer |
| `foo ·3│33` | `[foo, ·]` | `[333, ·, bar]` | `333` | Inner(0) |
| `foo ·33│3` | `[foo, ·]` | `[333, ·, bar]` | `333` | Inner(1) |
| `foo ·333│·` | `[foo, ·, 333]` | `[·, bar]` | `·` | Outer |

### Revised Message Format

```typescript
interface CaretUpdate {
  t: "caret";
  pieceId: string;           // ID of piece we're "on" (first of right siblings)
  caret: "outer" | number;   // "outer" or inner index n
  shape: null | "left" | "right";  // caret shape for rendering
}
```

The `caret` field:
- `"outer"` means `Outer` (at piece's left edge)
- A number `n` means `Inner(n)` (at position n inside piece)

### Implementation Changes

#### 1. Sender (SyncReplace.re)

```reason
let get_caret_position = (z: Zipper.t): option((Id.t, Caret.t, option(Direction.t))) => {
  // Get the piece we're "on" - first of right siblings, or last of left if at end
  let piece_opt = switch (z.relatives.siblings) {
    | (_, [piece, ..._]) => Some(piece)
    | ([_, ..._] as left, []) => Some(ListUtil.last(left))
    | _ => None
  };

  switch (piece_opt) {
  | Some(piece) =>
    let shape = switch (z.caret) {
      | Inner(_) => None  // Inner positions always have straight shape
      | Outer => Zipper.Caret.direction(z)
    };
    Some((Piece.id(piece), z.caret, shape))
  | None => None
  };
};
```

Serialize caret as:
- `Outer` → send `"outer"`
- `Inner(n)` → send `n`

#### 2. Receiver (RemoteCaretDec.re)

```reason
let view = (
  ~measured: Measured.t,
  ~font_metrics: FontMetrics.t,
  ~color: string,
  ~piece_id: Id.t,
  ~caret: option(int),  // None = Outer, Some(n) = Inner(n)
  ~shape: option(Direction.t),
): option(Node.t) => {
  switch (Measured.find_by_id(piece_id, measured)) {
  | None => None
  | Some(measurement) =>
    let origin = measurement.origin;
    let col_offset = switch (caret) {
      | None => 0           // Outer: at piece's left edge
      | Some(n) => n + 1    // Inner(n): n+1 columns into the piece
    };
    let position = Point.{row: origin.row, col: origin.col + col_offset};
    let side = switch (caret) {
      | None => Direction.Left  // Outer caret is at left edge
      | Some(_) => Direction.Right  // Inner caret... (may need adjustment)
    };
    Some(main(~font_metrics, ~color, ~origin=position, ~side, ~shape))
  };
};
```

#### 3. Message Protocol (patchworkmessages.d.ts)

Update the types to reflect the new format:

```typescript
interface CaretUpdate {
  t: "caret";
  pieceId: string;
  caret: "outer" | number;
  shape: null | "left" | "right";
}

interface RemoteCaret {
  t: "remote-caret";
  userId: string;
  color: string;
  pieceId: string;
  caret: "outer" | number;
  shape: null | "left" | "right";
}
```

#### 4. tool.tsx (patchwork-extra/hazel)

Update message handling to pass through the new caret format.

### Testing Plan

After implementing fixes, verify:

1. **Inner positions**: Move caret through `333`, remote caret should track each position
2. **Whitespace**: Move through spaces, remote caret should appear on whitespace pieces
3. **Shape**: At piece boundaries, caret should show correct convex/concave shape
4. **Edge cases**: Beginning/end of document, empty segments

### Future Enhancements (Out of Scope)

- Selection range sync (highlight what others have selected)
- User name labels next to remote carets
- User avatar indicators
- Cursor trails / animation

## Projector Support

Sync projector/livelit state between collaborators.

- [ ] Extend `FlatTile` or add separate structure for projector placements
- [ ] Include projector state in `HazelDoc`
- [ ] Update `FlatConvert.re` to handle projector serialization
- [ ] Test with various projector types

Currently projectors don't sync - their placements are lost in the flat representation.

## Performance

- [ ] Consider diff-based sync instead of full-state sync
- [ ] Profile and optimize `FlatConvert` for large documents
- [ ] Investigate incremental updates to reduce message size

## Security

- [ ] Restrict PostMessage origin (currently uses `"*"`)
- [ ] Validate incoming messages more strictly
