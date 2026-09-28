// The binding is Hazel's side of Patchwork's drafts and history scrubber:
// the host re-points a mounted view at a draft's clone, or at a url pinned to
// historical heads, and Hazel must follow. These tests play the host: a
// "main" repo owns the document and its draft clone, a second repo (the
// Hazel iframe's replica) is linked to it over a MessageChannel.
import { beforeAll, describe, expect, it } from "vitest";
import {
  initializeBase64Wasm,
  parseAutomergeUrl,
  Repo,
  stringifyAutomergeUrl,
  type AutomergeUrl,
  type DocHandle,
} from "@automerge/automerge-repo/slim";
import { automergeWasmBase64 } from "@automerge/automerge/automerge.wasm.base64";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import { HazelBinding } from "./binding";
import { initDoc, seedItems, type HazelDoc } from "./schema";
import { FakeHazel, settle } from "./testing";

beforeAll(async () => {
  await initializeBase64Wasm(automergeWasmBase64);
});

const identity = { user: "u", name: "Ada", color: "#e53935" };

async function setup() {
  const { port1, port2 } = new MessageChannel();
  const host = new Repo({ network: [new MessageChannelNetworkAdapter(port1)] });
  const replica = new Repo({ network: [new MessageChannelNetworkAdapter(port2)] });
  const main: DocHandle<HazelDoc> = host.create<HazelDoc>();
  main.change((d) => {
    initDoc(d, "tail");
    seedItems(d, [{ id: "x", kind: "def", header: "x", body: "1" }]);
  });
  const hazel = new FakeHazel<HazelBinding>();
  const binding = new HazelBinding(replica, hazel, identity);
  hazel.session = binding;
  await binding.point(main.url);
  await settle();
  return { host, main, hazel, binding, cleanup: () => binding.destroy() };
}

const text = (h: DocHandle<HazelDoc>, id: string) => String(h.doc()!.items[id]?.body ?? "");
const pinned = (h: DocHandle<HazelDoc>): AutomergeUrl =>
  stringifyAutomergeUrl({ documentId: parseAutomergeUrl(h.url).documentId, heads: h.heads() });

describe("HazelBinding", () => {
  it("shows the document it is pointed at", async () => {
    const { hazel, binding, cleanup } = await setup();
    expect(hazel.loads).toBe(1);
    expect(hazel.readonly).toBe(false);
    expect(hazel.order()).toEqual(["x", "tail"]);
    expect(binding.session).not.toBeNull();
    cleanup();
  });

  it("checking out a draft: edits go to the clone, main is untouched", async () => {
    const { host, main, hazel, binding, cleanup } = await setup();
    const clone = host.clone(main); // what the draft overlay forks
    await binding.point(clone.url);
    await settle();
    expect(hazel.loads).toBe(2);
    hazel.type("x", "body", () => "2");
    await settle();
    expect(text(clone, "x")).toBe("2");
    expect(text(main, "x")).toBe("1");
    // and main's later edits don't leak into what Hazel shows
    main.change((d) => {
      d.items.x.header = "y";
    });
    await settle();
    expect(hazel.text("x", "header")).toBe("x");
    cleanup();
  });

  it("switching back to main reloads and edits land on main again", async () => {
    const { host, main, hazel, binding, cleanup } = await setup();
    const clone = host.clone(main);
    await binding.point(clone.url);
    await settle();
    await binding.point(main.url);
    await settle();
    expect(hazel.loads).toBe(3);
    hazel.type("x", "body", (s) => s + "0");
    await settle();
    expect(text(main, "x")).toBe("10");
    expect(text(clone, "x")).toBe("1");
    cleanup();
  });

  it("scrubbing: a url with heads is shown read-only and refuses edits", async () => {
    const { main, hazel, binding, cleanup } = await setup();
    const before = pinned(main);
    main.change((d) => {
      d.items.x.body = "2";
    });
    await settle();
    expect(hazel.text("x")).toBe("2");
    await binding.point(before);
    await settle();
    expect(hazel.readonly).toBe(true);
    expect(binding.readOnly).toBe(true);
    expect(hazel.text("x")).toBe("1"); // the past
    hazel.type("x", "body", () => "9");
    binding.insert({ id: "n", kind: "def", header: "n", body: "0" }, "x");
    binding.remove("x");
    binding.caret({ id: "x", leaf: "body", anchor: 0, head: 0 });
    await settle();
    expect(text(main, "x")).toBe("2"); // nothing got through
    expect(main.doc()!.items.n).toBeUndefined();
    // back to live: writable again, with the live text
    await binding.point(main.url);
    await settle();
    expect(hazel.readonly).toBe(false);
    expect(hazel.text("x")).toBe("2");
    hazel.type("x", "body", (s) => s + "!");
    await settle();
    expect(text(main, "x")).toBe("2!");
    cleanup();
  });

  it("seqs keep increasing across a switch; a stale basis is dropped", async () => {
    const { host, main, hazel, binding, cleanup } = await setup();
    const first = binding.session!;
    hazel.type("x", "body", () => "12"); // basis from the first session
    const staleBasis = hazel.basis("x", "body");
    await settle();
    const clone = host.clone(main);
    // Hazel lags: it types against the old document while the switch happens
    hazel.lag = true;
    await binding.point(clone.url);
    const second = binding.session!;
    expect(second).not.toBe(first);
    expect(second.firstSeq).toBe(first.lastSeq + 1);
    const r = binding.edit(staleBasis, "x", "body", "123");
    expect(r).toBe(staleBasis);
    await settle();
    expect(text(clone, "x")).toBe("12"); // the stale edit was not merged in
    hazel.flush();
    expect(hazel.text("x")).toBe("12");
    cleanup();
  });

  it("a newer point supersedes an older one still resolving", async () => {
    const { host, main, hazel, binding, cleanup } = await setup();
    const clone = host.clone(main);
    const p1 = binding.point(clone.url);
    const p2 = binding.point(main.url);
    await Promise.all([p1, p2]);
    await settle();
    expect(binding.url).toBe(main.url);
    expect(binding.session!.handle.url).toBe(main.url);
    expect(hazel.loads).toBe(2); // main, then main again — never the clone
    cleanup();
  });

  it("pointing at the current url is a no-op", async () => {
    const { main, hazel, binding, cleanup } = await setup();
    await binding.point(main.url);
    await settle();
    expect(hazel.loads).toBe(1);
    cleanup();
  });
});
