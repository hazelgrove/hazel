// Runs inside the Hazel page when it's embedded as a collaborative editor
// (docs/collab-modular.md). The embedding page hands us a MessagePort that
// is a network link to its own Automerge repo, then tells us which document
// to show; we run a replica here, in the same JS realm as Hazel, and bind it
// to Hazel's collab host (registered by ScratchCollab.JsApi.register_host).
//
// Protocol with the embedder:
//   iframe -> parent  { type: "hazel-collab:ready" }
//   parent -> iframe  { type: "hazel-collab:boot", identity, docUrl? }  + [port]
//   parent -> iframe  { type: "hazel-collab:point", docUrl }
//
// `point` may arrive any number of times: the embedder sends the document's
// current *backing* url (see patchwork-tool/src/tool.ts), which changes when
// a draft is checked out or history is scrubbed. A url with heads is shown
// read-only.
import { initializeBase64Wasm, Repo, type AutomergeUrl } from "@automerge/automerge-repo/slim";
import { automergeWasmBase64 } from "@automerge/automerge/automerge.wasm.base64";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import { HazelBinding } from "./binding";
import type { HazelSide, Identity, RemoteChange } from "./session";

type Host = {
  load(json: string): void;
  remote(json: string): void;
  peers(json: string): void;
};

declare global {
  interface Window {
    hazelCollabHost?: Host;
    hazelCollabReady?: () => void;
    hazelCollab?: unknown;
  }
}

// localStorage["hazel-collab-debug"] = "1" logs the traffic with Hazel
const DEBUG = (() => {
  try {
    return !!localStorage.getItem("hazel-collab-debug");
  } catch {
    return false;
  }
})();
const debug = (...args: unknown[]) => DEBUG && console.log("[hazel-collab]", ...args);

let binding: HazelBinding | null = null;
// `point` can arrive before `boot` has finished initialising; keep the latest
let pending: AutomergeUrl | null = null;

window.addEventListener("message", (e: MessageEvent) => {
  if (e.source !== window.parent) return;
  switch (e.data?.type) {
    case "hazel-collab:boot": {
      const [port] = e.ports;
      if (!port) return;
      pending = e.data.docUrl ?? null;
      boot(port, e.data.identity).catch((err) => console.error("[hazel-collab] boot failed", err));
      return;
    }
    case "hazel-collab:point": {
      const url = e.data.docUrl as AutomergeUrl;
      debug("point", url);
      if (binding) void binding.point(url);
      else pending = url;
      return;
    }
  }
});

if (window.parent !== window) window.parent.postMessage({ type: "hazel-collab:ready" }, "*");

async function boot(port: MessagePort, identity: Identity) {
  await initializeBase64Wasm(automergeWasmBase64);
  const repo = new Repo({ network: [new MessageChannelNetworkAdapter(port)] });
  const host = await waitForHost();
  const side: HazelSide = {
    load: (seq, items, readonly) => {
      debug("load", seq, readonly ? "(read-only)" : "", items);
      host.load(JSON.stringify({ seq, items, readonly }));
    },
    remote: (seq, changes) => {
      debug("remote", seq, JSON.stringify(changes));
      host.remote(toWire(seq, changes));
    },
    peers: (carets) => host.peers(JSON.stringify(carets)),
  };
  binding?.destroy();
  const b = new HazelBinding(repo, side, identity);
  binding = b;
  window.hazelCollab = {
    edit: (basis: number, id: string, leaf: "lead" | "header" | "body", text: string) => {
      const r = b.edit(basis, id, leaf, text);
      debug("edit", { basis, id, leaf, text }, "->", r);
      return r;
    },
    insert: (json: string, after: string | null) => {
      const r = b.insert(JSON.parse(json), after);
      debug("insert", json, "after", after, "->", r);
      return r;
    },
    remove: (id: string) => {
      debug("remove", id);
      b.remove(id);
    },
    move: (id: string, after: string | null) => {
      debug("move", id, "after", after);
      b.move(id, after);
    },
    caret: (id: string | null, leaf: "lead" | "header" | "body", anchor: number, head: number) =>
      b.caret(id === null ? null : { id, leaf, anchor, head }),
    caretDelim: (id: string, delim: number, off: number) => b.caret({ id, delim, off }),
    binding: b,
    get session() {
      return b.session;
    },
  };
  window.addEventListener("pagehide", () => b.destroy());
  if (pending) {
    const url = pending;
    pending = null;
    await b.point(url);
  }
}

function waitForHost(): Promise<Host> {
  if (window.hazelCollabHost) return Promise.resolve(window.hazelCollabHost);
  return new Promise((resolve) => {
    window.hazelCollabReady = () => resolve(window.hazelCollabHost!);
  });
}

function toWire(seq: number, changes: RemoteChange[]) {
  const leaves = [];
  const upserts = [];
  const deletes = [];
  for (const c of changes) {
    if (c.t === "leaf") leaves.push({ id: c.id, leaf: c.leaf, text: c.text });
    else if (c.t === "upsert") upserts.push(c.item);
    else deletes.push(c.id);
  }
  return JSON.stringify({ seq, leaves, upserts, deletes });
}
