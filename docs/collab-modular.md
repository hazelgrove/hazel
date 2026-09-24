# Collaborative Hazel on Automerge (modular editors)

Status: design + in-progress implementation on branch `patchwork-modular`
(based on `modular-editors`). Supersedes the tylr flat-piece sync of the
`patchwork` branch for new documents; the old `hazeldoc` datatype is left alone.

## Goals

- Multi-user editing of a Hazel program in Patchwork, with everyone's caret
  visible.
- Merge at the level of **text**, per definition, using Automerge's text CRDT,
  and let Hazel re-parse. Hazel parses anything (holes, errors), so there is no
  need to merge tile structure.
- Build the sync layer so it runs inside Hazel's own page with Automerge as a
  library. In Patchwork the replica syncs with the host over a MessageChannel;
  a standalone hazel.org deployment would swap in IndexedDB + websocket.

## Architecture

```
Patchwork page                                  Hazel iframe (served from the tool's own module)
┌────────────────────────────────┐              ┌──────────────────────────────────────────────┐
│ tool.ts                        │  MessagePort │ collab.js (Automerge Repo, in-memory)        │
│  - host Repo (window.repo)     │◀────────────▶│  - DocHandle for the program doc             │
│  - iframe src = ./hazel/…      │  (automerge  │  - reconcile loop (heads + echo flag)        │
│  - hands the iframe a port     │   sync +     │  - Presence (carets)                         │
│    + the doc URL               │   ephemeral) │        ▲  per-leaf splices / carets          │
└────────────────────────────────┘              │        ▼  (direct JS calls, same realm)      │
                                                │ Hazel (js_of_ocaml)                          │
                                                │  - Collab module: items ⇄ leaf texts,        │
                                                │    caret ⇄ (item, leaf, offset)              │
                                                │  - modular editors: master + stacked cells   │
                                                └──────────────────────────────────────────────┘
```

- Hazel's release build ships **inside the Patchwork module** (pushwork folder)
  and the iframe points at it via the service-worker URL
  (`new URL("./hazel/index.html", import.meta.url)`), so no foreign server is
  involved. Verified: an iframe at a module URL loads HTML, a 6 MB script and a
  relative Web Worker (first load ~9 s while the files sync, then cached).
- The iframe's repo peers with the host repo exactly like Patchwork's own
  `isolation` package (`MessageChannelNetworkAdapter` on both ends). Because
  Hazel and its Automerge replica share a JS realm, the reconcile loop is
  synchronous, and there's no async OT between frames.
- automerge-repo relays ephemeral messages to all other peers
  (`DocSynchronizer.receiveEphemeralMessage`), so `Presence` inside the iframe
  reaches other users through the host repo.

## Document schema (datatype `hazel`)

```ts
type HazelDoc = {
  "@patchwork": { type: "hazel" };
  title: string;
  version: 1;
  items: { [itemId: string]: Item };  // keyed by id, never addressed by list index
};
type Item = {
  parent: string | null;              // module item id, or null for top level
  order: string;                      // fractional index; sort by (order, id)
  kind: "def" | "type" | "module" | "stmt" | "tail";
  lead: string;                       // Automerge text: code/comments before a def-like item
  header: string;                     // Automerge text: pattern (+ annotation) / type name / module name
  body: string;                       // Automerge text: RHS / statement / trailing expression
};
```

- `itemId` is the Hazel `Id` of the item's `let`/`type`/`module` tile (for
  `stmt`, the `;` tile). All peers therefore agree on item ids, which keeps the
  outline, pins and `DefStatics` keys consistent across users. The `tail` gets
  one id per block, chosen by whoever creates the block.
- Delimiters (`let … = … in`, `type … = … in`, `;`) are **structure**, derived
  from `kind`, never text. A text edit can't split or merge items; only explicit
  structural ops (outline new/duplicate/move/delete) change the item set.
- Fractional `order` keys avoid the duplicate/lost-item problems of concurrent
  moves in an Automerge list (there is no move op).
- Deletion removes the item. A concurrent edit to a deleted item's text is lost.
  That's acceptable for v1; a `deleted` tombstone is the upgrade path.

## Items from a program (every program maps to items)

Items are read off modular-editors' top-level spans, made total, since
half-typed code is the normal state while editing (`ScratchCollab.cspans`):

- only complete `let`/`type`/`module … = … in` tiles are definition
  boundaries; an incomplete one (a stray `in`, a `let` still being typed)
  is ordinary content;
- content between items belongs to the next item: a definition's `lead`
  text, or the start of a statement's / the tail's body.

So typing a new definition between two others first shows up as the next
item's `lead`, then, once its `in` is typed, as a new item.

**Lossless.** Every character of the program belongs to exactly one leaf or
delimiter token. A definition's region ends at its `in` and a statement's
at its `;`; whitespace after them (blank lines between definitions) starts
the next item's `lead` (or statement/tail body). Leaves keep their edge
whitespace: the space before `=`, a newline after it, indentation, a newline
before `in`. So the program's text is exactly the concatenation of leaves
and delimiters, every whitespace edit syncs, and every peer shows the same
layout. This relies on dev's canonical completion (#2374): indentation is
ordinary user-owned whitespace, so no layout is computed. Definition cells
still show a leaf's trimmed core; caret offsets shift by its leading
whitespace, and a remote edit updates the frozen program copy's edge
whitespace too. When splicing
content back in at the top level (lead / statement / tail), the top level
is regrouted: unlike tile children it sits next to its neighbours (a lead
`0` before a `let` needs an operator hole between them).

## Sync protocol (collab/src/session.ts)

Hazel applies changes through its asynchronous action queue, so its view of
the document can lag. Instead of hand-written OT:

- every message to Hazel carries a `seq` naming a document version (heads);
- Hazel keeps a **basis per leaf**: the seq its text for that leaf derives
  from. A local edit is reported as (basis, item, leaf, new full text); the
  session diffs it against the leaf's text *at the basis version* and applies
  it there with `handle.changeAt`, so Automerge merges it with whatever
  happened since, and returns the leaf's new basis;
- whenever the document has moved past what Hazel was told, the session sends
  the full current text of each changed leaf with a fresh seq;
- Hazel ignores an incoming leaf text if its basis for that leaf is newer
  than the message (it typed since; a newer message follows if the merged
  text differs);
- structural edits (insert / remove / move) are echoed back to Hazel as
  ordinary remote changes, since the session chooses order keys; `insert`
  returns the basis for the new item's leaves.

Presence is a small protocol on the handle's ephemeral messages (not
automerge-repo's `Presence`, whose API differs across versions): state
messages, a heartbeat, a TTL and a goodbye. A caret is either

- in a leaf: Automerge cursors into that leaf's text, so it follows
  concurrent edits; or
- on one of an item's delimiters (`let`/`=`/`in`, `;`), which are structure,
  not text: the tile shard index and an offset into that token. That's
  meaningful on every peer whatever the local whitespace, and is drawn on the
  token itself in the whole-program editor (or at the matching edge of a
  definition cell, which doesn't show delimiters).

## Leaf text semantics

- Canonical text = `Printer.of_segment(~holes="", ~concave_holes="",
  ~refractors=[])` of the leaf's segment, whitespace included: exactly what
  the user typed, with grout (holes) derived, never stored.
- Local edit: after each action in a cell, print the leaf and diff against the
  previous print (common prefix/suffix) → one `A.splice` per changed leaf.
- Remote edit to a leaf:
  - **not open locally**: update the text, re-parse that leaf (FastParse, with
    the recovering parser as fallback), splice into the master with
    `splice_pat`/`splice_def` (item tile id survives → only that item's statics
    reruns). Can be debounced/lazy.
  - **open in a local cell**: re-parse, replace the cell content, restore the
    caret by text offset. This is a dedicated, non-historic action that is not
    re-sent.
- Invariant check: after a re-parse, printing must reproduce the text. If it
  doesn't, write the normalized form **once** (print∘parse is idempotent) rather
  than ping-ponging.
- **Convergence.** Every peer shows a leaf as parsed from its text, in a
  canonical form: holes go after the whitespace next to them, at every level
  (`ScratchCollab.canonical_hole_placement`). Holes aren't text, so a local
  edit can leave holes where the text doesn't put them. After a local edit
  that only touched whitespace, if the parsed leaf differs from the editor's
  (compared with holes marked), the editor adopts the parsed form. The text
  doesn't change, so carets keep their offsets
  (`ScratchCollabMode.normalize`).
- **Held-back spaces.** A space typed between two operands becomes an
  operator hole, and Hazel holds the space back until the hole is filled
  (`Grout.suppressed_space`). The space is part of the leaf text
  (`with_owed_space`), and the canonical form makes it a real space before
  the hole. Filling the hole then continues exactly as in plain Hazel.
- Projectors print as trigger syntax (`^^fold(…)`); their models aren't synced
  in v1. Probes/refractors are per-user.

## Carets / presence

- One `Presence<{ hazel: HazelCaret | null }>` per document, `userId` = the
  contact URL. `HazelCaret = { item, leaf: "header"|"body", anchor: A.Cursor,
  head: A.Cursor, lastMoved }`. Automerge cursors keep positions stable across
  concurrent edits.
- Hazel exchanges carets as (item, leaf, UTF-16 offset). New code maps a zipper
  caret to a text offset (walk ancestors/left siblings, print lengths) and back
  (leaf-length walk → `jump_to_shard`/`jump_to_side_of_id` + `Inner(k)`).
- Rendering reuses the patchwork branch's `RemoteCaretDec` (takes a `Point` in
  the editor's `Measured` space), plus per-user badges on outline rows.
- Name/colour come from each peer's contact doc.

## Undo

Hazel's undo restores whole-model snapshots, which would revert remote edits.
v1: a remote edit to a leaf clears that cell's undo history. Later: per-user
undo as inverse text splices mapped through remote edits.

## Status (September 2026)

Working end to end in a two-peer harness (two host repos, each running the
real tool + Hazel iframe with its own replica): typing in the master editor
(including new definitions), leads, remote edits, peer carets in the master
and in stack cells. Tests: `test/Test_Collab.re` (bridge), `collab/` vitest
(protocol, including lagging-Hazel races).

Known gaps:
- module members aren't separate items yet (a module's body is one leaf);
- remote edits to an open *test-run* cell rebuild the cell (caret resets);
- undo isn't collaboration-aware yet (snapshot undo can revert remote work);
- projector models aren't synced (projectors travel as trigger syntax).

## Performance

The bridge keeps to modular-editors' rule: per-keystroke work is
proportional to the edit, not the program. Timings are on mega-2k.

- **Caret ⇄ offset** is structural. The caret's offset is the length of
  what's left of it along the zipper path, and a caret is placed by
  descending through the tree by length. Piece lengths are cached by
  physical identity. The old token walk took ~150 ms per keystroke; this
  takes ~1 ms.
- **Local edits** reuse unchanged items when itemizing: ~1 ms.
- **A remote leaf edit** splices that leaf into the program.
  - In the whole-program editor, the new zipper goes into the existing
    editor, keeping its cached syntax and statics, so recalculation is
    incremental. Rebuilding the editor used to take ~120 ms plus a cold
    re-measure.
  - Edits between definitions (lead, statement, tail) regroup the whole
    top level: ~10 ms.
- **A remote insert, move or delete** rearranges the live program's item
  regions (`restructure`). Untouched items keep their pieces and inner ids.
  Rebuilding from items took ~780 ms and re-minted every id, so statics
  went cold; this takes ~15 ms.

## Phases

1. `collab/` JS package: schema, program ⇄ doc, reconcile loop, presence; tested
   with two in-memory repos.
2. OCaml `Collab` module: items ⇄ leaf texts, caret ⇄ offset, apply remote leaf
   text (open/closed), local edits → splices; tests.
3. Wiring in Hazel: collab boot in the iframe (port + doc URL from the parent),
   remote caret rendering, outline presence.
4. Patchwork tool: `hazel` datatype, iframe from the module, MessagePort bridge,
   build step that copies Hazel's release build into the module.
5. Outline structural ops → doc ops; undo policy; later, editing in the master
   editor via re-itemization.
