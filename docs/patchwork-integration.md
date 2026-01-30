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

Hazel's internal AST is a **nested tree** (`Segment`), but Automerge works best with **flat structures**. The `FlatConvert.re` module converts between:

- **Segment** - Hazel's internal nested tree with tiles containing child tiles
- **HazelDoc** - Flat array of pieces with UUID-based children references

## Message Protocol

Communication uses PostMessage with these message types:

| Message | Direction | Purpose |
|---------|-----------|---------|
| `init` | Both | Handshake on iframe load |
| `ping` | Both | Connection testing |
| `pong` | Both | Ping response |
| `state` | Both | Full document state sync |

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

| Message | Direction | Purpose |
|---------|-----------|---------|
| `caret` | Hazel → Parent | Local caret position changed |
| `remote-caret` | Parent → Hazel | Another user's caret position |
| `remote-caret-remove` | Parent → Hazel | User disconnected |

### Key Files

| File | Purpose |
|------|---------|
| `patchwork-extra/hazel/src/tool.tsx` | Broadcasts local caret, receives remote carets via ephemeral messages |
| `src/haz3lcore/patchwork/PatchworkComm.re` | Sends caret to parent, stores received remote carets |
| `src/haz3lcore/patchwork/SyncReplace.re` | Extracts caret position from zipper, decides when to send |
| `src/web/app/editors/decoration/RemoteCaretDec.re` | Renders remote carets with custom colors |
| `src/web/app/editors/code/CodeEditable.re` | Includes remote carets in editor decorations |

### Caret Position Model

The caret position sent over the wire uses this model:

1. **Piece selection**: The caret is always "on" the **first piece of right siblings** in the zipper. At the end of a segment, it's on the last piece of left siblings.

2. **Offset encoding**: `caretOffset` is 0 for `Outer` (at piece's left edge), or `n+1` for `Inner(n)` (position n inside the piece).

3. **Shape**: At `Outer` positions, the caret shape (convex/concave) is derived from `Zipper.Caret.direction`. For `Inner` positions, shape is always `null`.

**Example** (whitespace shown as `·`):
```
foo · 333 · bar
```
Segment: `[foo, ·, 333, ·, bar]`

| Visual | Piece you're on | caretOffset |
|--------|-----------------|-------------|
| `foo│· 333` | `·` (whitespace) | 0 |
| `foo ·│333` | `333` | 0 |
| `foo ·3│33` | `333` | 1 |
| `foo ·33│3` | `333` | 2 |
| `foo ·333│·` | `·` (whitespace) | 0 |

## Data Flow

### Local edit → sync to others:

1. User types in Hazel iframe
2. `SyncReplace.send_state()` called after action completes
3. `FlatConvert.seg_to_doc()` converts Segment → flat Doc
4. `PatchworkComm.js_of_flatdoc()` converts to JS types
5. PostMessage sends `EditorState` to parent
6. Parent's `tool.tsx` updates Automerge document
7. Automerge syncs to other clients

### Remote edit → apply locally:

1. Automerge receives update from another client
2. `tool.tsx` sends `EditorState` to iframe via PostMessage
3. `PatchworkComm.listen()` receives message
4. `FlatConvert.doc_to_seg()` converts flat Doc → Segment
5. `SyncReplace.sync_replace()` applies new segment while preserving caret
6. Editor re-renders

## Directory Structure

### Hazel repo (this repo)

```
src/haz3lcore/patchwork/
├── PatchworkComm.re       # PostMessage communication, type conversion
├── SyncReplace.re         # Apply remote state while preserving caret
├── FlatConvert.re         # Convert Segment ↔ flat Doc
├── FlatDoc.mli            # OCaml types for flat document (generated)
├── PatchworkMessages.mli  # OCaml types for messages (generated)
└── dune                   # Build rules

embed/
├── src/
│   ├── components/
│   │   ├── HazelEmbed.tsx # React component for embedding
│   │   └── DocGraph.tsx   # Debug visualization
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

- Projectors/livelits not supported (placements won't sync)
- Full-state sync (not diff-based)
- Caret position can be disrupted when:
  - Caret becomes end of focal segment due to other player's actions
  - Subterm containing caret is deleted

## Future Work

See `plan/patchwork-future.md` for planned improvements:
- Debounce caret messages, selection sync, user labels
- Projector support in sync format
- Performance optimizations (diff-based sync)
