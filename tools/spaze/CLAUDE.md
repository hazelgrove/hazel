# Space

A tldraw-based spatial folder viewer for Patchwork. Renders folder contents as draggable, resizable windows on an infinite canvas with a Mac OS System 7.5 aesthetic.

## Build & Deploy

```sh
pnpm build        # tsc && vite build
pushwork sync      # sync dist to automerge
```

Always build before syncing. Use `pushwork sync` (not `npx pushwork sync`).

## Architecture

### Source Files

- **`src/index.ts`** — Plugin entry point. Registers "space" tool for "folder" datatype.
- **`src/tool.tsx`** — Main component (`SpaceCanvas`). Manages:
  - Tldraw editor setup and automerge sync (bidirectional)
  - Folder doc list reconciliation (docs array <-> tldraw shapes)
  - Presence/cursors for collaboration
  - Image paste handler (creates UnixFileEntry docs)
  - System 7.5 CSS theming of tldraw UI
- **`src/PatchworkDocShape.tsx`** — Custom tldraw shape for embedded documents. Renders each doc as a System 7.5-style window with titlebar. Image files (`@patchwork.type === "file"` with `mimeType` starting `image/`) render as `<img>` tags; everything else uses `<patchwork-view>`.
- **`src/NewDocTool.tsx`** — Tldraw tool for creating new patchwork documents by drawing a box. Includes datatype selector toolbar.
- **`src/TLStoreToAutomerge.ts`** — Converts tldraw store changes to automerge patches.
- **`src/AutomergeToTLStore.ts`** — Converts automerge patches to tldraw store operations.

### Document Structure

The folder doc (`SpaceDoc`) extends `FolderDoc` with:

```ts
{
  docs: DocLink[],           // { name, type, url } entries
  tldraw: string,            // AutomergeUrl pointing to a dedicated tldraw doc
}
```

The tldraw doc (`TldrawDoc`) is a separate automerge document:

```ts
{
  '@patchwork': { type: 'tldraw' },
  store: { [recordId: string]: any },
  schema: any,
}
```

### Key Patterns

**Bidirectional sync:** tldraw store changes are persisted to the tldraw automerge doc, and remote automerge changes are applied back to the tldraw store. A `preventPatchApplicationsRef` guard prevents feedback loops.

**Folder docs <-> shapes reconciliation:** The `reconcilePatchworkDocShapes` function ensures every entry in `docs` has a corresponding `patchwork-doc` shape on canvas, and removes shapes for deleted docs. It takes a `skipUrls` set to protect special shapes (like the sideboard) from deletion.

**NewDocTool flow:** Creates a shape with empty `docUrl`, then async creates the actual doc and updates the shape. The store listener detects the `docUrl` going from empty to non-empty (`newlyLinked` pattern) and adds the doc to the folder's docs list via the correct per-instance `handle` closure.

**Per-editor context (not module singletons):** The NewDocTool stores its context (element, handle) in a `WeakMap<Editor, ...>` keyed by editor instance. This is critical because the tool can render inside itself (subfolder on canvas), so module-level singletons would be overwritten by the inner instance.

**Stale async guard:** The `filterTldrawDocs` async callback in the reconciliation effect uses a `stale` flag (set by the effect cleanup) to prevent outdated data from deleting valid shapes.

### Sideboard

A special patchwork-doc shape is created at init time showing the folder itself with `toolId: 'chee/sideboard'`. It's positioned top-left and excluded from both the store listener's docs sync and the reconciliation's removal logic (via `skipUrls` and `handle.url` checks).

### Image Paste

Pasted images are stored as `UnixFileEntry` automerge docs with:

```ts
{
  '@patchwork': { type: 'file', suggestedImportUrl: 'automerge:3gH1AZwy53dHX3mqxkdfN875J3HT' },
  content: Uint8Array,
  extension: string,
  mimeType: string,
  name: string,
}
```

The file is also added to the folder's `docs` array with `type: 'file'`. Default name for clipboard images (where `file.name` is `image.png` or starts with `tldrawFile`): `Pasted image at ${date}.${ext}`.

### Sparkle Animation

When a `<patchwork-view>` fires the `patchwork:mounted` event, the containing patchwork-doc shape plays a sparkle animation: gold stars pop sequentially around the border with a glowing box-shadow.

## Style

Visual theme is Mac OS System 7.5:
- Square corners (all `--tl-radius-*` set to 0)
- Geneva font family
- `#c0c0c0` panel backgrounds
- Beveled borders (light top-left, dark bottom-right)
- Navy blue (`#000080`) selection/primary color
- Window shapes have striped titlebars with close/zoom boxes
