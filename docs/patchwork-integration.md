# Hazel-Patchwork Integration

This document describes how Hazel integrates with Patchwork as an embeddable tool.

## Architecture Overview

The integration uses an **iframe-based architecture** with two codebases:

1. **Hazel repo (`patchwork` branch)** - OCaml/ReasonML code running inside the iframe, plus an `embed/` package with React components for the parent
2. **patchwork-extra/hazel** - The Patchwork "tool" that wraps HazelEmbed and connects to Automerge for multi-user sync

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
│  - Iframe.re listens for messages           │
│  - SyncReplace.re applies remote changes    │
│  - AutoSeg.re converts tree↔flat            │
└─────────────────────────────────────────────┘
```

## Key Insight: Tree vs Flat Representation

Hazel's internal AST is a **nested tree** (`Segment`), but Automerge works best with **flat structures**. The `AutoSeg.re` module converts between:

- **Segment** - Hazel's internal nested tree with tiles containing child tiles
- **HazelDoc** - Flat array of pieces with UUID-based children references

## Message Protocol

Communication uses PostMessage with these message types (defined in `MessageTypes.mli` and `messages.d.ts`):

| Message | Direction | Purpose |
|---------|-----------|---------|
| `init` | Both | Handshake on iframe load |
| `ping` | Both | Connection testing |
| `pong` | Both | Ping response |
| `state` | Both | Full document state sync |

## Data Flow: User Edit → Sync

### Local edit propagates to parent:

1. User types in Hazel iframe
2. Action processed by `Perform.re`
3. `SyncReplace.send_state()` called after action completes
4. `AutoSeg.seg_to_doc()` converts Segment → flat Doc
5. `Iframe.js_of_autoseg()` converts to JS types
6. PostMessage sends `EditorState` to parent
7. Parent's `tool.tsx` receives message
8. `handle.change()` updates Automerge document
9. Automerge syncs to other clients

### Remote edit applies to local:

1. Automerge receives update from another client
2. `useEffect` in `tool.tsx` detects doc change
3. PostMessage sends `EditorState` to iframe
4. `Iframe.listen()` receives message
5. `autoseg_of_hazeldoc()` converts JS → OCaml types
6. `AutoSeg.doc_to_seg()` converts flat Doc → Segment
7. `SyncReplace.sync_replace()` applies new segment while preserving caret position
8. Editor re-renders with new state

## Core Files

### Hazel repo (this repo)

| File | Purpose |
|------|---------|
| `src/haz3lcore/Iframe.re` | PostMessage communication, type conversion |
| `src/haz3lcore/zipper/action/SyncReplace.re` | Apply remote state while preserving caret |
| `src/haz3lcore/statics/AutoSeg.re` | Convert Segment ↔ flat Doc |
| `src/haz3lcore/Delta.mli` | OCaml types for flat document (ts2ocaml generated) |
| `src/haz3lcore/MessageTypes.mli` | OCaml types for messages |
| `embed/src/components/HazelEmbed.tsx` | React component for embedding |
| `embed/src/types/delta.d.ts` | TypeScript types for flat document |
| `embed/src/types/messages.d.ts` | TypeScript message types |

### patchwork-extra/hazel

| File | Purpose |
|------|---------|
| `src/tool.tsx` | Patchwork tool UI, wraps HazelEmbed |
| `src/datatype.ts` | Automerge schema for HazelDoc |
| `src/index.ts` | Plugin registration |

## URL Configuration

The Hazel iframe can load from:
- **Local**: `http://localhost:8001/?name=Patchwork&share=` (local Hazel dev server)
- **Remote**: `https://hazel.org/build/patchwork/?name=Patchwork&share=` (hosted build)

The tool.tsx UI includes "Local" and "Remote" buttons to quickly switch between these, plus a text field for custom URLs. The `?name=Patchwork&share=` query params configure the Hazel UI for the embedded context.

## Type Conversion

Types must match between OCaml and TypeScript. The pattern:

- `Delta.mli` / `delta.d.ts` - Document structure types
- `MessageTypes.mli` / `messages.d.ts` - Message protocol types
- `Iframe.re`'s `RedundantCoverterIGuess` module - Bidirectional converters

Only `FlatTile` (with UUID children references) is used, not nested `Tile`.

## Current Limitations

From the PR notes:
- Projectors/livelits not supported (placements won't sync)
- Entire state is serialized and sent (not diff-based)
- Caret position can be disrupted when:
  - Caret becomes end of focal segment due to other player's actions
  - Subterm containing caret is deleted (falls back to ancestor, imprecisely)

## Building

### Hazel (this repo)
```bash
make deps
make dev  # or make release for production build
```

### patchwork-extra/hazel

The patchwork-extra/hazel package has some specific requirements due to its dependencies.

#### Prerequisites

- **pnpm 9.x required**: The lockfile is v9.0 format. If you have pnpm 8.x installed, use `npx pnpm@9` instead.
- **patchwork CLI**: Must be installed and linked for the `patchwork push` command.

#### Standard Workflow (UI changes only)

For changes that only modify source files (like `tool.tsx`) without touching dependencies:

```bash
cd /path/to/patchwork-extra/hazel

# Build (use pnpm 9)
npx pnpm@9 build

# Push to Patchwork
patchwork push
```

#### After Clean Clone or Dependency Changes

If `node_modules` is missing or you need to reinstall:

```bash
cd /path/to/patchwork-extra/hazel

# Install with frozen lockfile to preserve working versions
npx pnpm@9 install --frozen-lockfile

# Build and push
npx pnpm@9 build
patchwork push
```

#### Critical: Do NOT Modify Dependencies

The `package.json` uses a GitHub subdirectory dependency:
```json
"@hazelgrove/hazel-embed": "github:hazelgrove/hazel#patchwork&path:/embed/"
```

**Important notes:**
- This syntax (`#branch&path:/subdir/`) requires pnpm 9.x to resolve correctly
- Do NOT change this to a `file:` path - it will break the build
- The lockfile pins specific versions of vite (5.x) that work correctly
- Using `rolldown-vite` (7.x) or other vite alternatives breaks the build due to CommonJS handling differences

#### Troubleshooting

**"Lockfile not compatible with current pnpm"**
→ Use `npx pnpm@9` instead of `pnpm`

**"Could not resolve patchwork&path:/embed/ to a commit"**
→ You're using pnpm 8.x. Use `npx pnpm@9 install --frozen-lockfile`

**"Calling 'require' for react in an environment that doesn't expose require"**
→ The wrong vite version was used. Reinstall with `npx pnpm@9 install --frozen-lockfile` to restore vite 5.x

**Build succeeds but tool fails to load in Patchwork**
→ Check the browser console. Usually means a bundler incompatibility. Revert any package.json changes and reinstall with frozen lockfile.

#### Why These Constraints Exist

The embed package uses `react-d3-tree` for the DocGraph visualization, which uses CommonJS internally. Vite 5.x handles this correctly, but rolldown-vite 7.x bundles CommonJS with a `require` shim that doesn't work in Patchwork's ES module environment. The frozen lockfile ensures the working vite version is preserved.
