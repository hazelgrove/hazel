// A collaborative editing session: binds a Hazel instance to one Automerge
// document. See docs/collab-modular.md.
//
// Hazel updates its model through an asynchronous action queue, so its view
// of the document can lag the document itself. The protocol makes that safe
// without hand-written OT:
//
// - Every message to Hazel carries a `seq`, naming the exact document version
//   (Automerge heads) its contents reflect. Seqs increase in the order they
//   are handed out.
// - Hazel keeps a basis per leaf: the seq its text for that leaf derives
//   from. It reports a local edit as (basis, item, leaf, new full text); the
//   session diffs that against the leaf's text *at the basis version* and
//   applies the splice there with `changeAt`, so Automerge merges it with
//   anything that happened since. It returns the leaf's new basis ("basis +
//   this edit"), which is exactly Hazel's text.
// - Whenever the document has moved past what Hazel has been told (remote
//   changes, a local edit merged into a newer document, or a structural edit
//   to echo), the session sends the full current text of every leaf that
//   changed, tagged with a fresh seq.
// - Hazel ignores an incoming leaf text if its basis for that leaf is newer
//   than the message: it has typed since, and if the merged text differs a
//   newer message is already on its way. Otherwise it takes the text and the
//   message's seq as the leaf's basis.
import * as A from "@automerge/automerge/slim";
import {
  decodeHeads,
  type DocHandle,
  type DocHandleChangePayload,
  type DocHandleEphemeralMessagePayload,
  type UrlHeads,
} from "@automerge/automerge-repo/slim";
import {
  allItems,
  children,
  leafPath,
  LEAVES,
  orderAfter,
  snapshot,
  type HazelDoc,
  type ItemSnapshot,
  type Leaf,
  type NewItem,
} from "./schema";

export type RemoteChange =
  | { t: "leaf"; id: string; leaf: Leaf; text: string }
  | { t: "upsert"; item: ItemSnapshot }
  | { t: "delete"; id: string };

// A caret is either in a leaf's text, or on one of an item's delimiters
// (`let`/`=`/`in`, `;`: structure, not text), as the tile shard index and an
// offset into that token. Delimiter positions are structural, so they mean
// the same thing on every peer whatever the local whitespace.
export type LeafCaret = { id: string; leaf: Leaf; anchor: number; head: number };
export type DelimCaret = { id: string; delim: number; off: number };
export type Caret = LeafCaret | DelimCaret;
// As sent to Hazel: every field present, null where it doesn't apply.
export type PeerCaret = {
  peer: string;
  user: string | null;
  name: string;
  color: string;
  id: string;
  leaf: Leaf | null;
  anchor: number | null;
  head: number | null;
  delim: number | null;
  off: number | null;
};
export type Identity = { user: string | null; name: string; color: string };

// What Hazel implements. Each call is a message Hazel applies in order.
export interface HazelSide {
  load(seq: number, items: ItemSnapshot[]): void;
  remote(seq: number, changes: RemoteChange[]): void;
  peers(carets: PeerCaret[]): void;
}

type Splice = { index: number; delete: number; insert: string };

// The single splice turning `a` into `b` (common prefix/suffix, UTF-16).
export function diffText(a: string, b: string): Splice | null {
  if (a === b) return null;
  let p = 0;
  const max = Math.min(a.length, b.length);
  while (p < max && a.charCodeAt(p) === b.charCodeAt(p)) p++;
  let s = 0;
  while (s < max - p && a.charCodeAt(a.length - 1 - s) === b.charCodeAt(b.length - 1 - s)) s++;
  // don't split surrogate pairs
  const isLow = (str: string, i: number) => {
    const c = str.charCodeAt(i);
    return c >= 0xdc00 && c <= 0xdfff;
  };
  if (p > 0 && p < a.length && isLow(a, p)) p--;
  if (s > 0 && isLow(a, a.length - s)) s--;
  return { index: p, delete: a.length - p - s, insert: b.slice(p, b.length - s) };
}

// ---- presence (carets) ----
// A tiny protocol over the handle's ephemeral messages, independent of
// automerge-repo's Presence class (whose API differs across versions).
const MARKER = "__hazelPresence";
type WireCaret =
  | { id: string; leaf: Leaf; anchor: A.Cursor; head: A.Cursor }
  | { id: string; delim: number; off: number };
type PresenceMsg =
  | { type: "state"; session: string; user: string | null; name: string; color: string; caret: WireCaret | null }
  | { type: "bye"; session: string };
const HEARTBEAT_MS = 2000;
const PEER_TTL_MS = 10000;

const MAX_VERSIONS = 256;

export class CollabSession {
  readonly sessionId = Math.random().toString(36).slice(2, 10);
  #seq = 0;
  #versions = new Map<number, UrlHeads>();
  #sentHeads: UrlHeads; // the version Hazel will reach after its queued messages
  #applyingLocal = false;
  #deliverScheduled = false;
  #caret: WireCaret | null = null;
  #peers = new Map<string, { msg: Extract<PresenceMsg, { type: "state" }>; seen: number }>();
  #timer: ReturnType<typeof setInterval>;
  #peersScheduled = false;

  constructor(
    readonly handle: DocHandle<HazelDoc>,
    readonly hazel: HazelSide,
    public identity: Identity,
  ) {
    this.#sentHeads = handle.heads();
    handle.on("change", this.#onChange);
    handle.on("ephemeral-message", this.#onEphemeral);
    this.#timer = setInterval(this.#tick, HEARTBEAT_MS);
    hazel.load(this.#record(this.#sentHeads), allItems(this.doc));
  }

  get doc(): HazelDoc {
    return this.handle.doc()!;
  }

  // ---- local edits (called by Hazel) ----

  // Hazel's leaf now reads `text`, edited from its state at `basis`.
  // Returns Hazel's new basis.
  edit(basis: number, id: string, leaf: Leaf, text: string): number {
    const basisHeads = this.#versions.get(basis);
    if (!this.doc.items[id]) return basis; // item deleted concurrently
    if (!basisHeads) {
      // unknown/expired basis: treat the current document as the basis
      console.warn("[hazel-collab] unknown basis", basis);
      return this.edit(this.#record(this.handle.heads()), id, leaf, text);
    }
    const before = leafText(A.view(this.doc, decodeHeads(basisHeads)), id, leaf);
    const splice = diffText(before ?? "", text);
    if (!splice) return basis;
    const upToDate = sameHeads(basisHeads, this.handle.heads()) && sameHeads(basisHeads, this.#sentHeads);
    let newHeads: UrlHeads | undefined;
    this.#local(() => {
      const fn = (d: HazelDoc) => {
        if (!d.items[id]) return;
        A.splice(d, leafPath(id, leaf), splice.index, splice.delete, splice.insert);
      };
      if (upToDate) {
        this.handle.change(fn);
        newHeads = this.handle.heads();
      } else {
        newHeads = this.handle.changeAt(basisHeads, fn);
      }
    });
    if (!newHeads) return basis;
    if (upToDate) this.#sentHeads = newHeads;
    else this.#scheduleDeliver(); // Hazel lacks what happened since its basis
    this.#schedulePeers();
    return this.#record(newHeads);
  }

  // Structural edits (insert / remove / move) are echoed back to Hazel as
  // ordinary remote changes, since the order key is chosen here.

  // Insert a new item after sibling `after` (null = first) under `parent`.
  // Returns the basis for the new item's leaves (a version that has it).
  insert(item: NewItem, after: string | null, parent: string | null = null): number {
    this.#structural((d) => {
      d.items[item.id] = {
        parent,
        order: item.kind === "tail" ? "~" : orderAfter(d, parent, after),
        kind: item.kind,
        lead: item.lead ?? "",
        header: item.header ?? "",
        body: item.body ?? "",
      };
    });
    return this.#record(this.handle.heads());
  }

  // Delete an item and everything under it.
  remove(id: string) {
    this.#structural((d) => {
      const doomed = [id];
      for (let i = 0; i < doomed.length; i++)
        for (const c of children(d, doomed[i])) doomed.push(c.id);
      for (const x of doomed) delete d.items[x];
    });
  }

  // Move an item to just after sibling `after` (null = first).
  move(id: string, after: string | null) {
    this.#structural((d) => {
      const it = d.items[id];
      if (!it || it.kind === "tail") return;
      it.order = orderAfter(d, it.parent, after, id);
    });
  }

  // The local caret: offsets into a leaf of Hazel's current text, or a
  // position on one of an item's delimiters.
  caret(c: Caret | null) {
    const doc = this.doc;
    if (c && doc.items[c.id] && "delim" in c) {
      this.#caret = { id: c.id, delim: c.delim, off: c.off };
    } else if (c && doc.items[c.id] && "leaf" in c) {
      try {
        const path = leafPath(c.id, c.leaf);
        this.#caret = {
          id: c.id,
          leaf: c.leaf,
          anchor: A.getCursor(doc, path, c.anchor),
          head: A.getCursor(doc, path, c.head),
        };
      } catch {
        this.#caret = null;
      }
    } else {
      this.#caret = null;
    }
    this.#broadcastState();
  }

  destroy() {
    this.handle.off("change", this.#onChange);
    this.handle.off("ephemeral-message", this.#onEphemeral);
    clearInterval(this.#timer);
    this.#send({ type: "bye", session: this.sessionId });
  }

  // ---- internals ----

  #record(heads: UrlHeads): number {
    const seq = ++this.#seq;
    this.#versions.set(seq, heads);
    if (this.#versions.size > MAX_VERSIONS) {
      const oldest = this.#versions.keys().next().value!;
      this.#versions.delete(oldest);
    }
    return seq;
  }

  #local(fn: () => void) {
    this.#applyingLocal = true;
    try {
      fn();
    } finally {
      this.#applyingLocal = false;
    }
  }

  #structural(fn: (d: HazelDoc) => void) {
    this.handle.change(fn); // not #local: the change listener schedules the echo
  }

  #onChange = (_: DocHandleChangePayload<HazelDoc>) => {
    if (this.#applyingLocal) return;
    this.#scheduleDeliver();
  };

  #scheduleDeliver() {
    if (this.#deliverScheduled) return;
    this.#deliverScheduled = true;
    queueMicrotask(() => {
      this.#deliverScheduled = false;
      this.#deliver();
    });
  }

  // Tell Hazel about everything between what it has been sent and now.
  #deliver() {
    const now = this.handle.heads();
    if (sameHeads(now, this.#sentHeads)) return;
    const doc = this.doc;
    const patches = A.diff(doc, decodeHeads(this.#sentHeads), decodeHeads(now));
    this.#sentHeads = now;
    const changes = changesOf(doc, patches);
    const seq = this.#record(now);
    if (changes === "reload") this.hazel.load(seq, allItems(doc));
    else this.hazel.remote(seq, changes);
    this.#schedulePeers();
  }

  #send(msg: PresenceMsg) {
    try {
      this.handle.broadcast({ [MARKER]: msg });
    } catch {
      // not connected yet
    }
  }

  #broadcastState() {
    const { user, name, color } = this.identity;
    this.#send({ type: "state", session: this.sessionId, user, name, color, caret: this.#caret });
  }

  #tick = () => {
    this.#broadcastState();
    const now = Date.now();
    let changed = false;
    for (const [k, p] of this.#peers)
      if (now - p.seen > PEER_TTL_MS) {
        this.#peers.delete(k);
        changed = true;
      }
    if (changed) this.#schedulePeers();
  };

  #onEphemeral = (e: DocHandleEphemeralMessagePayload<HazelDoc>) => {
    const msg = (e.message as any)?.[MARKER] as PresenceMsg | undefined;
    if (!msg || msg.session === this.sessionId) return;
    if (msg.type === "bye") this.#peers.delete(msg.session);
    else {
      const isNew = !this.#peers.has(msg.session);
      this.#peers.set(msg.session, { msg, seen: Date.now() });
      if (isNew) this.#broadcastState(); // let the newcomer see us right away
    }
    this.#schedulePeers();
  };

  #schedulePeers() {
    if (this.#peersScheduled) return;
    this.#peersScheduled = true;
    queueMicrotask(() => {
      this.#peersScheduled = false;
      this.hazel.peers(this.peerCarets());
    });
  }

  // Peers' carets resolved against the current document.
  peerCarets(): PeerCaret[] {
    const doc = this.doc;
    const out: PeerCaret[] = [];
    for (const [session, { msg }] of this.#peers) {
      const c = msg.caret;
      if (!c || !doc.items[c.id]) continue;
      const who = { peer: session, user: msg.user, name: msg.name, color: msg.color, id: c.id };
      if ("delim" in c) {
        out.push({ ...who, leaf: null, anchor: null, head: null, delim: c.delim, off: c.off });
        continue;
      }
      try {
        const path = leafPath(c.id, c.leaf);
        out.push({
          ...who,
          leaf: c.leaf,
          anchor: A.getCursorPosition(doc, path, c.anchor),
          head: A.getCursorPosition(doc, path, c.head),
          delim: null,
          off: null,
        });
      } catch {
        // the peer's caret refers to text we haven't received yet
      }
    }
    return out;
  }
}

function leafText(doc: HazelDoc, id: string, leaf: Leaf): string | undefined {
  const it = doc.items?.[id];
  return it ? String(it[leaf] ?? "") : undefined;
}

function sameHeads(a: UrlHeads, b: UrlHeads): boolean {
  return a.length === b.length && [...a].sort().join() === [...b].sort().join();
}

// Summarize patches as per-item changes: whole items that appeared,
// disappeared or moved, and leaves whose text changed.
function changesOf(doc: HazelDoc, patches: A.Patch[]): RemoteChange[] | "reload" {
  const upserts = new Set<string>();
  const deletes = new Set<string>();
  const leaves = new Map<string, Set<Leaf>>();
  for (const p of patches) {
    const [root, id, field] = p.path;
    if (root !== "items") {
      if (p.path.length === 0) return "reload";
      continue; // title etc.
    }
    if (id === undefined) return "reload";
    const key = String(id);
    if (field === undefined) {
      (doc.items[key] ? upserts : deletes).add(key);
    } else if (LEAVES.includes(field as Leaf)) {
      if (!leaves.has(key)) leaves.set(key, new Set());
      leaves.get(key)!.add(field as Leaf);
    } else {
      upserts.add(key); // order / parent / kind
    }
  }
  const out: RemoteChange[] = [];
  for (const id of deletes) if (!doc.items[id]) out.push({ t: "delete", id });
  for (const id of upserts) {
    const s = snapshot(doc, id);
    if (s) out.push({ t: "upsert", item: s });
  }
  for (const [id, ls] of leaves) {
    if (upserts.has(id) || !doc.items[id]) continue;
    for (const leaf of ls) out.push({ t: "leaf", id, leaf, text: leafText(doc, id, leaf)! });
  }
  return out;
}
