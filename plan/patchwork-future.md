# Patchwork Integration - Future Work

This document tracks planned improvements for the Hazel-Patchwork integration.

For architecture and current implementation documentation, see `docs/patchwork-integration.md`.

---

## Caret Position Fix

**Status: Not yet implemented**

Basic caret sync infrastructure is complete (see docs), but positioning has bugs.

### Observed Issues

1. **Inner positions not working**: Remote carets only appear at piece boundaries, not within multi-character tokens
2. **Whitespace doesn't show caret**: Moving through secondary/whitespace pieces doesn't update the remote caret
3. **Shape always straight**: Should change to convex/concave at piece boundaries

### Root Cause Analysis

#### Problem 1: Wrong piece selection on sender

**Current code** (SyncReplace.re) uses `Indicated.piece(z)` which filters out secondary and grout pieces, returning `None` when in whitespace.

**Fix**: Use first piece of right siblings directly:
```reason
switch (z.relatives.siblings) {
| (_, [piece, ..._]) => Some((Piece.id(piece), z.caret))
| ([_, ..._] as left, []) => Some((Piece.id(ListUtil.last(left)), z.caret))
| _ => None
}
```

#### Problem 2: Binary positioning on receiver

**Current code** (RemoteCaretDec.re) treats offset as binary (`origin` vs `last`), mapping ALL non-zero offsets to the piece's right edge.

**Fix**: Always position relative to `origin`, add offset for inner positions:
```reason
let col_offset = switch (caret) {
  | Outer => 0
  | Inner(n) => n + 1
};
let position = {row: origin.row, col: origin.col + col_offset};
```

### Conceptual Model: Zipper Caret Position

1. **Segment partitioning**: The zipper partitions a segment into left siblings and right siblings. Movement transfers pieces between them.

2. **"The piece you're on"**: By convention, you're always "on" the **first piece of right siblings**. When at the end of a segment, you're on the last piece of left siblings.

3. **Caret type**:
   - `Outer` = at the left edge of the piece (the boundary before it starts)
   - `Inner(n)` = at position n inside the piece (0-indexed)

4. **No "outer-left" vs "outer-right"**: When you're at the right edge of a piece, you're actually `Outer` on the NEXT piece.

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

### Implementation Changes

#### 1. Sender (SyncReplace.re)

```reason
let get_caret_position = (z: Zipper.t): option((Id.t, Caret.t, option(Direction.t))) => {
  let piece_opt = switch (z.relatives.siblings) {
    | (_, [piece, ..._]) => Some(piece)
    | ([_, ..._] as left, []) => Some(ListUtil.last(left))
    | _ => None
  };
  switch (piece_opt) {
  | Some(piece) =>
    let shape = switch (z.caret) {
      | Inner(_) => None
      | Outer => Zipper.Caret.direction(z)
    };
    Some((Piece.id(piece), z.caret, shape))
  | None => None
  };
};
```

#### 2. Receiver (RemoteCaretDec.re)

```reason
let view = (..., ~caret: option(int), ~shape: option(Direction.t)) => {
  switch (Measured.find_by_id(piece_id, measured)) {
  | None => None
  | Some(measurement) =>
    let origin = measurement.origin;
    let col_offset = switch (caret) {
      | None => 0
      | Some(n) => n + 1
    };
    let position = Point.{row: origin.row, col: origin.col + col_offset};
    Some(main(~font_metrics, ~color, ~origin=position, ~side=Direction.Left, ~shape))
  };
};
```

#### 3. Message Protocol (patchworkmessages.d.ts)

Update types to include `caret: "outer" | number` and `shape: null | "left" | "right"`.

#### 4. tool.tsx

Pass through the new caret format fields.

### Testing Plan

1. **Inner positions**: Move through `333`, remote caret tracks each position
2. **Whitespace**: Move through spaces, remote caret appears
3. **Shape**: Correct convex/concave at boundaries
4. **Edge cases**: Beginning/end of document

---

## Other Caret Sync Improvements

- [ ] Debounce outgoing caret messages (50ms threshold)
- [ ] Selection range sync (highlight what others have selected)
- [ ] User name labels next to remote carets

---

## Projector Support

Sync projector/livelit state between collaborators.

- [ ] Extend `FlatTile` or add separate structure for projector placements
- [ ] Include projector state in `HazelDoc`
- [ ] Update `FlatConvert.re` to handle projector serialization
- [ ] Test with various projector types

Currently projectors don't sync - their placements are lost in the flat representation.

---

## Performance

- [ ] Consider diff-based sync instead of full-state sync
- [ ] Profile and optimize `FlatConvert` for large documents
- [ ] Investigate incremental updates to reduce message size

---

## Security

- [ ] Restrict PostMessage origin (currently uses `"*"`)
- [ ] Validate incoming messages more strictly
