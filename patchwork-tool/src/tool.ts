import {
  parseAutomergeUrl,
  stringifyAutomergeUrl,
  type AutomergeUrl,
  type DocHandle,
  type Repo,
  type UrlHeads,
} from "@automerge/automerge-repo";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import type { ToolElement } from "@inkandswitch/patchwork-plugins";
import { subscribe, type DocHandleDescriptor } from "@inkandswitch/patchwork-providers";
import type { HazelDoc } from "../../collab/src/schema";
import "./tool.css";

// The Hazel build ships inside this module (see scripts/pack-hazel.mjs), so
// the iframe loads it from Patchwork's own origin via the service worker,
// not from a foreign server.
const HAZEL_URL = new URL("./hazel/index.html", import.meta.url).href;

// If no provider answers the descriptor subscription (a host without the
// overlay protocol), show the document the handle names.
const DESCRIPTOR_FALLBACK_MS = 3000;

const FALLBACK_COLORS = ["#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1"];

type Identity = { user: string | null; name: string; color: string };

// Hazel runs in an iframe with its own Automerge replica, linked to this
// realm's repo over a MessageChannel. The replica needs to know *which*
// document to show, and that is not simply `handle.url`: Patchwork hands
// tools a handle whose url stays put while the document behind it changes
// (checking out a draft points it at the draft's clone; scrubbing history
// pins it to earlier heads). The host's OverlayRepo learns of those changes
// from a streaming `repo:handle-descriptor` subscription, so we open the same
// subscription and forward every answer's backing url to the iframe, which
// re-points its replica (collab/src/binding.ts). A url with heads is shown
// read-only.
export default function HazelTool(handle: DocHandle<HazelDoc>, element: ToolElement) {
  const repo: Repo = element.repo ?? (window as any).repo;
  // `element.repo` may be an OverlayRepo; the network link belongs on the
  // realm's real repo underneath it.
  const liveRepo: Repo = (repo as Repo & { baseRepo?: Repo }).baseRepo ?? repo;

  const root = document.createElement("div");
  root.className = "hazel-collab";
  const iframe = document.createElement("iframe");
  iframe.className = "hazel-collab-frame";
  iframe.allow = "clipboard-read; clipboard-write";
  root.append(iframe);
  element.append(root);

  let adapter: MessageChannelNetworkAdapter | null = null;
  const disconnect = () => {
    if (!adapter) return;
    try {
      liveRepo.networkSubsystem.removeNetworkAdapter(adapter);
    } catch {
      adapter.disconnect();
    }
    adapter = null;
  };

  // The document the iframe should currently show; null until known.
  let backing: AutomergeUrl | null = null;
  let booted = false;
  const post = (msg: object, transfer: Transferable[] = []) =>
    iframe.contentWindow?.postMessage(msg, "*", transfer);

  const point = (url: AutomergeUrl) => {
    if (url === backing) return;
    backing = url;
    if (booted) post({ type: "hazel-collab:point", docUrl: url });
  };

  const { documentId, heads: presentedHeads } = parseAutomergeUrl(handle.url);
  const unsubscribe = subscribe<DocHandleDescriptor>(
    element,
    { type: "repo:handle-descriptor", url: stringifyAutomergeUrl({ documentId }) },
    (descriptor) => point(backingUrlOf(descriptor, presentedHeads)),
  );
  const fallback = setTimeout(() => {
    if (!backing) point(handle.url);
  }, DESCRIPTOR_FALLBACK_MS);

  // Link the iframe's replica to our repo each time the page (re)loads and
  // asks for one, then tell it what to show.
  const onMessage = async (e: MessageEvent) => {
    if (e.source !== iframe.contentWindow || e.data?.type !== "hazel-collab:ready") return;
    disconnect();
    booted = false;
    const { port1, port2 } = new MessageChannel();
    adapter = new MessageChannelNetworkAdapter(port2);
    liveRepo.networkSubsystem.addNetworkAdapter(adapter);
    const identity = await loadIdentity(repo);
    post({ type: "hazel-collab:boot", identity, docUrl: backing }, [port1]);
    booted = true;
  };
  window.addEventListener("message", onMessage);
  iframe.src = HAZEL_URL;

  return () => {
    clearTimeout(fallback);
    unsubscribe();
    window.removeEventListener("message", onMessage);
    disconnect();
    root.remove();
  };
}

// The document a descriptor says to read: the clone if there is one, keeping
// any heads the remapper pinned onto it (the history scrubber). Heads on the
// url the tool was opened with win, as in OverlayRepo.
function backingUrlOf(descriptor: DocHandleDescriptor, presentedHeads: UrlHeads | undefined): AutomergeUrl {
  const target = parseAutomergeUrl(descriptor.cloneUrl ?? descriptor.url);
  return stringifyAutomergeUrl({
    documentId: target.documentId,
    heads: presentedHeads ?? target.heads,
  });
}

async function loadIdentity(repo: Repo): Promise<Identity> {
  const fallback: Identity = {
    user: null,
    name: "Anonymous",
    color: FALLBACK_COLORS[Math.floor(Math.random() * FALLBACK_COLORS.length)],
  };
  try {
    const contactUrl = (window as any).accountDocHandle?.doc()?.contactUrl;
    if (!contactUrl) return fallback;
    const contact = (await repo.find<any>(contactUrl)).doc();
    return {
      user: contactUrl,
      name: contact?.type === "registered" && contact.name ? contact.name : "Anonymous",
      color: contact?.color || fallback.color,
    };
  } catch {
    return fallback;
  }
}
