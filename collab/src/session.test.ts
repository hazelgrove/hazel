import { beforeAll, describe, expect, it } from "vitest";
import { initializeBase64Wasm, Repo, type DocHandle } from "@automerge/automerge-repo/slim";
import { automergeWasmBase64 } from "@automerge/automerge/automerge.wasm.base64";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import { initDoc, seedItems, type HazelDoc, type ItemSnapshot, type Leaf } from "./schema";
import { CollabSession, diffText, type HazelSide, type PeerCaret, type RemoteChange } from "./session";

beforeAll(async () => {
  await initializeBase64Wasm(automergeWasmBase64);
});

// A stand-in for Hazel: holds leaf texts and a basis per leaf, applies
// messages in order. With `lag`, messages queue until flush(), like Hazel's
// action queue. A leaf's incoming text is ignored if the leaf was edited
// locally after the message was made (a newer message will follow if the
// merged text differs) — the same rule ScratchCollabMode implements.
class FakeHazel implements HazelSide {
  items = new Map<string, ItemSnapshot>();
  bases = new Map<string, number>();
  loadSeq = 0;
  carets: PeerCaret[] = [];
  queue: (() => void)[] = [];
  session!: CollabSession;
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
  load(seq: number, items: ItemSnapshot[]) {
    this.#enqueue(() => {
      this.items = new Map(items.map((i) => [i.id, { ...i }]));
      this.bases.clear();
      this.loadSeq = seq;
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

const settle = async () => {
  for (let i = 0; i < 20; i++) await new Promise((r) => setTimeout(r, 5));
};

async function setup(opts: { lagA?: boolean; lagB?: boolean } = {}) {
  const { port1, port2 } = new MessageChannel();
  const repoA = new Repo({ network: [new MessageChannelNetworkAdapter(port1)] });
  const repoB = new Repo({ network: [new MessageChannelNetworkAdapter(port2)] });
  const hA: DocHandle<HazelDoc> = repoA.create<HazelDoc>();
  hA.change((d) => {
    initDoc(d, "tail");
    seedItems(d, [
      { id: "x", kind: "def", header: "x", body: "1" },
      { id: "f", kind: "def", header: "f", body: "fun y -> y" },
    ]);
  });
  const hB = await repoB.find<HazelDoc>(hA.url);
  const a = new FakeHazel();
  const b = new FakeHazel();
  a.session = new CollabSession(hA, a, { user: "u-a", name: "Ada", color: "#e53935" });
  b.session = new CollabSession(hB, b, { user: "u-b", name: "Bo", color: "#1e88e5" });
  await settle();
  a.lag = !!opts.lagA;
  b.lag = !!opts.lagB;
  return { a, b, hA, hB, cleanup: () => (a.session.destroy(), b.session.destroy()) };
}

describe("diffText", () => {
  it("finds the single splice", () => {
    expect(diffText("abc", "abc")).toBeNull();
    expect(diffText("ab", "aXb")).toEqual({ index: 1, delete: 0, insert: "X" });
    expect(diffText("aaa", "aa")).toEqual({ index: 2, delete: 1, insert: "" });
    expect(diffText("a😀b", "a😃b")).toEqual({ index: 1, delete: 2, insert: "😃" });
  });
});

describe("CollabSession", () => {
  it("loads the program in order, tail last", async () => {
    const { a, b, cleanup } = await setup();
    expect(a.order()).toEqual(["x", "f", "tail"]);
    expect(b.order()).toEqual(["x", "f", "tail"]);
    expect(b.text("f")).toBe("fun y -> y");
    cleanup();
  });

  it("syncs a local edit to the other side", async () => {
    const { a, b, cleanup } = await setup();
    a.type("f", "body", (s) => s + " + 1");
    await settle();
    expect(b.text("f")).toBe("fun y -> y + 1");
    b.type("x", "header", () => "xx");
    await settle();
    expect(a.text("x", "header")).toBe("xx");
    cleanup();
  });

  it("merges concurrent edits to the same leaf by character", async () => {
    const { a, b, hA, hB, cleanup } = await setup();
    a.type("f", "body", (s) => "(" + s); // "(fun y -> y"
    b.type("f", "body", (s) => s + ")"); // "fun y -> y)"
    await settle();
    expect(String(hA.doc()!.items.f.body)).toBe("(fun y -> y)");
    expect(String(hB.doc()!.items.f.body)).toBe("(fun y -> y)");
    expect(a.text("f")).toBe("(fun y -> y)");
    expect(b.text("f")).toBe("(fun y -> y)");
    cleanup();
  });

  it("merges an edit made against a stale basis (lagging Hazel)", async () => {
    const { a, b, hA, cleanup } = await setup({ lagA: true });
    // B edits; A's Hazel hasn't processed it yet when A types
    b.type("f", "body", (s) => s.replace("fun", "fn"));
    await settle();
    expect(a.text("f")).toBe("fun y -> y"); // still queued
    a.type("f", "body", (s) => s + " + 1"); // based on the old text
    await settle();
    a.flush();
    expect(String(hA.doc()!.items.f.body)).toBe("fn y -> y + 1");
    expect(a.text("f")).toBe("fn y -> y + 1");
    expect(b.text("f")).toBe("fn y -> y + 1");
    // and A can keep typing from its new basis
    a.type("f", "body", (s) => s + "0");
    await settle();
    a.flush();
    expect(b.text("f")).toBe("fn y -> y + 10");
    expect(a.text("f")).toBe("fn y -> y + 10");
    cleanup();
  });

  it("inserts, moves and deletes items; the tail stays last", async () => {
    const { a, b, cleanup } = await setup();
    a.session.insert({ id: "g", kind: "def", header: "g", body: "2" }, "x");
    await settle();
    expect(b.order()).toEqual(["x", "g", "f", "tail"]);
    b.session.move("x", "f");
    await settle();
    expect(a.order()).toEqual(["g", "f", "x", "tail"]);
    a.session.remove("g");
    await settle();
    expect(b.order()).toEqual(["f", "x", "tail"]);
    cleanup();
  });

  it("concurrent inserts at the same place both survive", async () => {
    const { a, b, cleanup } = await setup();
    a.session.insert({ id: "p", kind: "def", header: "p", body: "0" }, "x");
    b.session.insert({ id: "q", kind: "def", header: "q", body: "0" }, "x");
    await settle();
    expect(a.order()).toEqual(b.order());
    expect(a.order().sort()).toEqual(["f", "p", "q", "tail", "x"]);
    cleanup();
  });

  it("a stale echo of a local insert doesn't clobber later typing", async () => {
    const { a, b, cleanup } = await setup({ lagA: true });
    a.insertLocal({ id: "n", kind: "def", header: "", body: "" }, "f");
    a.type("n", "header", () => "n");
    await settle(); // the echo of the insert is now queued in A
    a.type("n", "header", (s) => s + "ew"); // typed before A processes it
    a.type("n", "body", () => "1");
    await settle();
    a.flush();
    expect(a.text("n", "header")).toBe("new");
    expect(a.text("n", "body")).toBe("1");
    expect(b.text("n", "header")).toBe("new");
    expect(b.text("n", "body")).toBe("1");
    expect(a.order()).toEqual(["x", "f", "n", "tail"]);
    cleanup();
  });

  it("keeps typing across a remote edit to another leaf (per-leaf bases)", async () => {
    const { a, b, hA, cleanup } = await setup({ lagA: true });
    b.type("x", "body", () => "2"); // remote change to x
    await settle();
    a.type("f", "body", (s) => s + "!"); // A hasn't seen it yet
    await settle();
    a.flush(); // A applies x = 2
    a.type("x", "body", (s) => s + "0"); // edit x from A's new text
    await settle();
    a.flush();
    expect(String(hA.doc()!.items.x.body)).toBe("20");
    expect(b.text("x")).toBe("20");
    expect(b.text("f")).toBe("fun y -> y!");
    cleanup();
  });

  it("shares carets, which follow concurrent edits", async () => {
    const { a, b, cleanup } = await setup();
    a.session.caret({ id: "f", leaf: "body", anchor: 4, head: 4 }); // before "y"
    await settle();
    expect(b.carets).toHaveLength(1);
    expect(b.carets[0]).toMatchObject({ name: "Ada", id: "f", leaf: "body", anchor: 4, head: 4 });
    b.type("f", "body", (s) => "(" + s);
    await settle();
    expect(b.carets[0].head).toBe(5);
    a.session.caret(null);
    await settle();
    expect(b.carets).toHaveLength(0);
    cleanup();
  });
});
