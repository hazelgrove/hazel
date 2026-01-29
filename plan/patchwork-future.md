# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and usage documentation, see `docs/patchwork-integration.md`.

## Caret Sync

Show collaborators' cursor positions in real-time.

### Overview

Multiple users viewing the same Hazel document should see each other's caret positions rendered in different colors. This is purely view-layer state and should NOT be stored in the persistent document.

**Current state**: Document edits sync via Automerge. Each user has their own local caret that isn't shared.

**Goal**: Broadcast caret positions via Automerge's ephemeral message system (side channel), receive remote carets, render them with distinct colors.

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

#### 1. TypeScript Side (patchwork-extra/hazel/src/tool.tsx)

- [ ] Import `useCurrentAccount` from `@patchwork/sdk`
- [ ] Import `useLocalAwareness`, `useRemoteAwareness` from `@automerge/automerge-repo-react-hooks`
- [ ] Set up user ID from account
- [ ] Generate/assign user color
- [ ] Handle incoming "caret" messages from iframe → call `setLocalCaret()`
- [ ] Watch `remoteCarets` from `useRemoteAwareness` → send "remote-caret" to iframe
- [ ] Handle peer disconnection → send "remote-caret-remove" to iframe

```typescript
// Sketch of tool.tsx additions:
const account = useCurrentAccount();
const userId = account?.contactHandle.url ?? `anon-${Date.now()}`;
const userColor = getUserColor(userId); // deterministic from hash

const [, setLocalCaret] = useLocalAwareness({
  handle,
  userId,
  initialState: null as CaretState | null,
});

const [remoteCarets] = useRemoteAwareness({
  handle,
  localUserId: userId,
});

// In onMessage handler:
case "caret": {
  setLocalCaret({ pieceId: message.pieceId, offset: message.caretOffset });
  break;
}

// Effect to forward remote carets to iframe:
useEffect(() => {
  for (const [remoteUserId, caret] of Object.entries(remoteCarets)) {
    if (caret) {
      sendToHazel.current({
        t: "remote-caret",
        userId: remoteUserId,
        color: getUserColor(remoteUserId),
        pieceId: caret.pieceId,
        caretOffset: caret.offset,
      });
    }
  }
  // Also handle removals for users no longer in remoteCarets
}, [remoteCarets]);
```

#### 2. OCaml Side: Send local caret updates

**File: `src/haz3lcore/patchwork/PatchworkComm.re`**

- [ ] Add `send_caret` function to broadcast caret position
- [ ] Call `send_caret` when caret moves (hook into Update or Zipper changes)
- [ ] Debounce outgoing caret messages (50ms)

```ocaml
(* Sketch - actual implementation will differ *)
let send_caret = (zipper: Zipper.t) => {
  let (piece_id, caret_offset) = get_caret_position(zipper);
  send_to_parent({
    t: "caret",
    pieceId: piece_id,
    caretOffset: caret_offset,
  });
};
```

**Getting caret position from Zipper:**
- `Indicated.piece(z)` returns the current piece
- `z.caret` is `Outer` or `Inner(n)`
- Convert to `(pieceId: string, offset: int)` where offset 0 = Outer, n+1 = Inner(n)

#### 3. OCaml Side: Receive and store remote carets

**File: `src/haz3lcore/patchwork/PatchworkComm.re`**

- [ ] Add `remote_carets: ref(Id.Map.t(remote_caret))` or similar state
- [ ] Handle "remote-caret" message: store/update in map
- [ ] Handle "remote-caret-remove" message: remove from map
- [ ] Expose getter for view layer

```ocaml
type remote_caret = {
  user_id: string,
  color: string,
  piece_id: Id.t,
  caret_offset: int,
};

let remote_carets: ref(StringMap.t(remote_caret)) = ref(StringMap.empty);

(* In listen() switch: *)
| "remote-caret" => {
    let rc = parse_remote_caret(msg);
    remote_carets := StringMap.add(rc.user_id, rc, remote_carets^);
    schedule(UpdateRemoteCarets);
  }
| "remote-caret-remove" => {
    let user_id = parse_user_id(msg);
    remote_carets := StringMap.remove(user_id, remote_carets^);
    schedule(UpdateRemoteCarets);
  }
```

#### 4. View Layer: Render remote carets

**New file: `src/web/app/editors/decoration/RemoteCaretDec.re`**

Create a variant of `CaretDec` for remote cursors:
- Different color (passed in, not hardcoded)
- No blinking animation
- Possibly slightly different width or style to distinguish

```ocaml
(* Similar to CaretDec but with color parameter and no blink *)
let view =
    (
      ~measured: Haz3lcore.Measured.t,
      ~font_metrics: FontMetrics.t,
      ~color: string,
      ~piece_id: Id.t,
      ~caret_offset: int,
    )
    : Node.t => {
  (* Find position of piece_id in measured layout *)
  (* Render caret at that position with given color *)
};
```

**File: `src/web/app/editors/code/CodeEditable.re`**

- [ ] In `deco` function, add remote carets after local caret:

```ocaml
let deco = (~syntax: CachedSyntax.t, ~z: Zipper.t, ~globals: Globals.t) => [
  CaretDec.view(~measured=syntax.measured, ~font_metrics=globals.font_metrics, z),
  ...List.map(
    ((user_id, rc)) => RemoteCaretDec.view(
      ~measured=syntax.measured,
      ~font_metrics=globals.font_metrics,
      ~color=rc.color,
      ~piece_id=rc.piece_id,
      ~caret_offset=rc.caret_offset,
    ),
    StringMap.bindings(PatchworkComm.get_remote_carets()),
  ),
  Arms.Indicated.term(~font_metrics=globals.font_metrics, ~syntax, z),
  ...
];
```

### File Summary

| File | Changes |
|------|---------|
| `embed/src/types/patchworkmessages.d.ts` | Add CaretUpdate, RemoteCaret, RemoteCaretRemove types |
| `src/haz3lcore/patchwork/PatchworkMessages.mli` | Regenerate from TypeScript |
| `src/haz3lcore/patchwork/PatchworkComm.re` | send_caret, receive remote carets, state storage |
| `patchwork-extra/hazel/src/tool.tsx` | useLocalAwareness, useRemoteAwareness, message bridging |
| `src/web/app/editors/decoration/RemoteCaretDec.re` | New file for remote caret rendering |
| `src/web/app/editors/code/CodeEditable.re` | Add remote carets to deco function |

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
