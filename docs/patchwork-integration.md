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

See `plan/patchwork-future.md` for planned features:
- Caret sync (show other users' cursor positions)
- Projector support in sync format
