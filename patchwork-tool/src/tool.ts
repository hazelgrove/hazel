import type { DocHandle, Repo } from "@automerge/automerge-repo";
import { MessageChannelNetworkAdapter } from "@automerge/automerge-repo-network-messagechannel";
import type { ToolElement } from "@inkandswitch/patchwork-plugins";
import type { HazelDoc } from "../../collab/src/schema";
import "./tool.css";

// The Hazel build ships inside this module (see scripts/pack-hazel.mjs), so
// the iframe loads it from Patchwork's own origin via the service worker,
// not from a foreign server.
const HAZEL_URL = new URL("./hazel/index.html", import.meta.url).href;

const FALLBACK_COLORS = ["#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#00ACC1"];

type Identity = { user: string | null; name: string; color: string };

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

export default function HazelTool(handle: DocHandle<HazelDoc>, element: ToolElement) {
  const repo: Repo = element.repo ?? (window as any).repo;
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
      repo.networkSubsystem.removeNetworkAdapter(adapter);
    } catch {
      adapter.disconnect();
    }
    adapter = null;
  };

  // The iframe runs its own Automerge replica; link it to our repo with a
  // MessageChannel each time the page (re)loads and asks for one.
  const onMessage = async (e: MessageEvent) => {
    if (e.source !== iframe.contentWindow || e.data?.type !== "hazel-collab:ready") return;
    disconnect();
    const { port1, port2 } = new MessageChannel();
    adapter = new MessageChannelNetworkAdapter(port2);
    repo.networkSubsystem.addNetworkAdapter(adapter);
    const identity = await loadIdentity(repo);
    iframe.contentWindow?.postMessage(
      { type: "hazel-collab:boot", docUrl: handle.url, identity },
      "*",
      [port1],
    );
  };
  window.addEventListener("message", onMessage);
  iframe.src = HAZEL_URL;

  return () => {
    window.removeEventListener("message", onMessage);
    disconnect();
    root.remove();
  };
}
