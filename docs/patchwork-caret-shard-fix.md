# Remote Caret Shard Index Fix

## The Problem

When the caret is at an **Inner position inside a multi-shard tile** (like `let...in`, `if...then...else`), the remote caret renders at the wrong position - typically on a different line where the first shard is located.

### Root Cause

Multi-shard tiles share the same tile ID across all their shards:
- `let x = 1 in e` is ONE tile with ID "abc"
- Shards: "let" (index 0), "=" (index 1), "in" (index 2)
- When at the "in" shard, the zipper has a tile with `id=abc, shards=[2]`

**What we currently send:**
- `pieceId` = the tile ID (shared by all shards)
- `caretOffset` = character offset within the current shard

**What the receiver does:**
- `Measured.find_by_id(pieceId)` returns the **combined measurement** of all shards
- This gives `origin` of first shard ("let" on line 0) and `last` of last shard ("in" on line 2)
- We compute position as `origin + caretOffset`, which uses the wrong origin!

**Result:** Caret inside "in" on line 2 renders on line 0 (where "let" is).

### Why Outer positions work

- `side=Left`: Uses `origin` of first shard - correct for "before the tile"
- `side=Right`: Uses `last` of last shard - correct for "after the tile"
- At child segment boundaries (between definition and "in"), you're at the end of that child segment, not at the tile level

### Why Inner positions fail

Inner positions are inside a specific shard, but we only send the tile ID. The receiver can't know which shard's measurement to use.

## The Fix

Add `shardIndex` to the caret protocol so the receiver can look up the specific shard's measurement.

### Files to Modify

#### 1. TypeScript Types (`embed/src/types/patchworkmessages.d.ts`)

Add `shardIndex: number | null` to both `CaretUpdate` and `RemoteCaret`:

```typescript
export interface CaretUpdate {
  t: "caret";
  pieceId: string;
  shardIndex: number | null;  // NEW: which shard of a multi-shard tile (null for non-tiles)
  caretOffset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}

export interface RemoteCaret {
  t: "remote-caret";
  userId: string;
  color: string;
  pieceId: string;
  shardIndex: number | null;  // NEW
  caretOffset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}
```

Then regenerate OCaml types:
```bash
cd embed && pnpm type:patchworkmessages
```

#### 2. SyncReplace.re (`src/haz3lcore/patchwork/SyncReplace.re`)

Update `get_caret_position` to also return the shard index:

```reason
/* Returns (piece_id, shard_index, caret_offset, shape, side) */
let get_caret_position =
    (z: Zipper.t)
    : option((Id.t, option(int), int, option(Direction.t), option(Direction.t))) => {
  let (piece_opt, side) = /* ... existing logic ... */;
  switch (piece_opt) {
  | Some(piece) =>
    let piece_id = Piece.id(piece);
    /* Extract shard index for tiles */
    let shard_index =
      switch (piece) {
      | Tile(t) =>
        switch (t.shards) {
        | [i] => Some(i)  /* Single shard - return its index */
        | _ => None       /* Multiple shards or empty - shouldn't happen at caret */
        }
      | _ => None  /* Grout, Secondary, Projector - no shards */
      };
    let caret_offset = /* ... existing logic ... */;
    let shape = /* ... existing logic ... */;
    Some((piece_id, shard_index, caret_offset, shape, side));
  | None => None
  };
};
```

Update `send_caret` to pass the shard index to `PatchworkComm.send_caret`.

#### 3. PatchworkComm.re (`src/haz3lcore/patchwork/PatchworkComm.re`)

Update `remote_caret` type:
```reason
type remote_caret = {
  user_id: string,
  color: string,
  piece_id: Id.t,
  shard_index: option(int),  /* NEW */
  caret_offset: int,
  shape: option(Direction.t),
  side: option(Direction.t),
};
```

Update `send_caret` signature and implementation to include shard index.

Update the listener to extract `shardIndex` from received messages.

#### 4. RemoteCaretDec.re (`src/web/app/editors/decoration/RemoteCaretDec.re`)

Add a helper to look up a specific shard's measurement:

```reason
let find_shard_measurement =
    (piece_id: Id.t, shard_index: option(int), measured: Measured.t)
    : option(Measured.measurement) => {
  switch (shard_index) {
  | None => Measured.find_by_id(piece_id, measured)  /* Non-tile: use existing lookup */
  | Some(idx) =>
    /* For tiles, look up the specific shard */
    switch (Id.Map.find_opt(piece_id, measured.tiles)) {
    | Some(shards) =>
      switch (ListUtil.assoc_opt(idx, shards)) {
      | Some(m) => Some(m)
      | None => Measured.find_by_id(piece_id, measured)  /* Fallback */
      }
    | None => Measured.find_by_id(piece_id, measured)  /* Fallback */
    }
  };
};
```

Update `view` function:
- Add `~shard_index: option(int)` parameter
- Use `find_shard_measurement` instead of `Measured.find_by_id`

Update `view_all` to pass `shard_index` from `remote_caret`.

#### 5. patchwork-extra/hazel tool.tsx

Update interfaces:
```typescript
interface CaretPosition {
  pieceId: string;
  shardIndex: number | null;  // NEW
  offset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}

interface RemoteCaret {
  userId: string;
  color: string;
  pieceId: string;
  shardIndex: number | null;  // NEW
  offset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}
```

Update `broadcastCaret` to include `shardIndex`.

Update ephemeral message handler to extract and store `shardIndex`.

Update caret message handler to pass `shardIndex` to broadcast.

Update remote caret forwarding to include `shardIndex`.

Then build and deploy:
```bash
cd /path/to/patchwork-extra/hazel
npx pnpm@9 build
patchwork push
```

## Testing

After implementing, test with:
1. `let x = 1 in 2` with "in" on a separate line - move through "in" character by character
2. `if true then 1 else 2` with each keyword on separate lines - move through "then" and "else"
3. Verify remote caret appears at correct position for all Inner positions in multi-line delimiters

## Already Fixed (in current commit)

- End-of-segment positions: Added `side` field to distinguish left vs right edge
- Multi-line pieces: Using `measurement.last.row` when `side=Right`

## Current State

The `side` field fix has been committed. The `shardIndex` fix is the remaining work to handle Inner positions in multi-shard tiles.
