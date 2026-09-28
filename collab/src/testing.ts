// Test doubles shared by the vitest suites (not a test file itself).
import type { ItemSnapshot, Leaf } from "./schema";
import type { HazelSide, PeerCaret, RemoteChange } from "./session";

// The editing calls Hazel makes; both CollabSession and HazelBinding fit.
export type Editor = {
  edit(basis: number, id: string, leaf: Leaf, text: string): number;
  insert(item: { id: string; kind: "def"; header: string; body: string }, after: string | null): number;
};

// A stand-in for Hazel: holds leaf texts and a basis per leaf, applies
// messages in order. With `lag`, messages queue until flush(), like Hazel's
// action queue. A leaf's incoming text is ignored if the leaf was edited
// locally after the message was made (a newer message will follow if the
// merged text differs) — the same rule ScratchCollabMode implements.
export class FakeHazel<E extends Editor = Editor> implements HazelSide {
  items = new Map<string, ItemSnapshot>();
  bases = new Map<string, number>();
  loadSeq = 0;
  loads = 0;
  readonly = false;
  carets: PeerCaret[] = [];
  queue: (() => void)[] = [];
  session!: E;
  constructor(public lag = false) {}

  basis(id: string, leaf: Leaf) {
    return this.bases.get(id + "/" + leaf) ?? this.loadSeq;
  }
  #setText(seq: number, id: string, leaf: Leaf, text: string) {
    const it = this.items.get(id);
    if (!it || this.basis(id, leaf) > seq) return;
    it[leaf] = text;
    this.bases.set(id + "/" + leaf, seq);
  }
  load(seq: number, items: ItemSnapshot[], readonly: boolean) {
    this.#enqueue(() => {
      this.items = new Map(items.map((i) => [i.id, { ...i }]));
      this.bases.clear();
      this.loadSeq = seq;
      this.loads++;
      this.readonly = readonly;
    });
  }
  remote(seq: number, changes: RemoteChange[]) {
    this.#enqueue(() => {
      for (const c of changes) {
        if (c.t === "delete") this.items.delete(c.id);
        else if (c.t === "upsert") {
          const old = this.items.get(c.item.id);
          if (!old) {
            this.items.set(c.item.id, { ...c.item });
            for (const l of ["lead", "header", "body"] as Leaf[]) this.bases.set(c.item.id + "/" + l, seq);
          } else {
            Object.assign(old, { order: c.item.order, parent: c.item.parent, kind: c.item.kind });
            for (const l of ["lead", "header", "body"] as Leaf[]) this.#setText(seq, c.item.id, l, c.item[l]);
          }
        } else this.#setText(seq, c.id, c.leaf, c.text);
      }
    });
  }
  peers(carets: PeerCaret[]) {
    this.carets = carets;
  }
  #enqueue(f: () => void) {
    if (this.lag) this.queue.push(f);
    else f();
  }
  flush() {
    while (this.queue.length) this.queue.shift()!();
  }

  // a local edit: rewrite a leaf and report it
  type(id: string, leaf: Leaf, f: (s: string) => string) {
    const it = this.items.get(id)!;
    it[leaf] = f(it[leaf]);
    this.bases.set(id + "/" + leaf, this.session.edit(this.basis(id, leaf), id, leaf, it[leaf]));
  }
  // a local structural insert; Hazel shows the item right away
  insertLocal(item: { id: string; kind: "def"; header: string; body: string }, after: string | null) {
    this.items.set(item.id, { ...item, lead: "", parent: null, order: "zzz" });
    const basis = this.session.insert(item, after);
    for (const l of ["lead", "header", "body"] as Leaf[]) this.bases.set(item.id + "/" + l, basis);
  }
  text(id: string, leaf: Leaf = "body") {
    return this.items.get(id)?.[leaf];
  }
  order() {
    // program order as Hazel would render it
    const sibs = [...this.items.values()].filter((i) => i.parent === null);
    sibs.sort((a, b) =>
      a.kind === "tail" ? 1 : b.kind === "tail" ? -1 : a.order < b.order ? -1 : a.order > b.order ? 1 : a.id < b.id ? -1 : 1,
    );
    return sibs.map((i) => i.id);
  }
}

// Let queued microtasks, timers and MessageChannel traffic drain.
export const settle = async () => {
  for (let i = 0; i < 20; i++) await new Promise((r) => setTimeout(r, 5));
};
