// Runs inside the Hazel page when it's embedded as a collaborative editor
// (docs/collab-modular.md). The embedding page hands us a MessagePort that
// is a network link to its own Automerge repo, plus the document to edit;
// we run a replica here, in the same JS realm as Hazel, and bind it to
// Hazel's collab host (registered by ScratchCollab.JsApi.register_host).
//
// Protocol with the embedder:
//   iframe -> parent  { type: "hazel-collab:ready" }
//   parent -> iframe  { type: "hazel-collab:boot", docUrl, identity }  + [port]
import { initializeBase64Wasm, Repo, type AutomergeUrl } from "@automerge/automerge-repo/slim";
import { automergeWasmBase64 } from "@automerge/automerge/automerge.wasm.base64";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import type { HazelDoc } from "./schema";
import { CollabSession, type HazelSide, type Identity, type RemoteChange } from "./session";

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

async function boot(port: MessagePort, docUrl: AutomergeUrl, identity: Identity) {
  await initializeBase64Wasm(automergeWasmBase64);
  const repo = new Repo({ network: [new MessageChannelNetworkAdapter(port)] });
  const handle = await repo.find<HazelDoc>(docUrl);
  const host = await waitForHost();
  const side: HazelSide = {
    load: (seq, items) => {
      debug("load", seq, items);
      host.load(JSON.stringify({ seq, items }));
    },
    remote: (seq, changes) => {
      debug("remote", seq, JSON.stringify(changes));
      host.remote(toWire(seq, changes));
    },
    peers: (carets) => host.peers(JSON.stringify(carets)),
  };
  const session = new CollabSession(handle, side, identity);
  window.hazelCollab = {
    edit: (basis: number, id: string, leaf: "lead" | "header" | "body", text: string) => {
      const b = session.edit(basis, id, leaf, text);
      debug("edit", { basis, id, leaf, text }, "->", b);
      return b;
    },
    insert: (json: string, after: string | null) => {
      const b = session.insert(JSON.parse(json), after);
      debug("insert", json, "after", after, "->", b);
      return b;
    },
    remove: (id: string) => {
      debug("remove", id);
      session.remove(id);
    },
    move: (id: string, after: string | null) => {
      debug("move", id, "after", after);
      session.move(id, after);
    },
    caret: (id: string | null, leaf: "lead" | "header" | "body", anchor: number, head: number) =>
      session.caret(id === null ? null : { id, leaf, anchor, head }),
    caretDelim: (id: string, delim: number, off: number) => session.caret({ id, delim, off }),
    session,
  };
  window.addEventListener("pagehide", () => session.destroy());
}

window.addEventListener("message", (e: MessageEvent) => {
  if (e.source !== window.parent || e.data?.type !== "hazel-collab:boot") return;
  const [port] = e.ports;
  if (!port) return;
  boot(port, e.data.docUrl, e.data.identity).catch((err) =>
    console.error("[hazel-collab] boot failed", err),
  );
});

if (window.parent !== window) window.parent.postMessage({ type: "hazel-collab:ready" }, "*");
