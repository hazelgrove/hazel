/**
 * Hazel-Patchwork PostMessage Protocol
 *
 * These types define the messages sent between the Hazel iframe and its parent.
 * Communication is bidirectional via window.postMessage().
 *
 * Type Conversion Flow:
 *   TypeScript (this file) --[ts2ocaml]--> OCaml (MessageTypes.mli)
 *
 * To regenerate OCaml types after modifying this file:
 *   cd embed && pnpm type:messages
 *
 * Message handling:
 *   - Parent side: HazelEmbed.tsx listens for HazelToParent messages
 *   - Iframe side: Iframe.re listen() handles ParentToHazel messages
 *
 * Typical message flow:
 *   1. Iframe loads, sends Init to parent
 *   2. Parent sends Init back (handshake complete)
 *   3. User edits in iframe -> iframe sends EditorState to parent
 *   4. Parent stores in Automerge, syncs to other clients
 *   5. Other client's edit arrives -> parent sends EditorState to iframe
 *   6. Iframe applies change via SyncReplace action
 */

import type { HazelDoc } from "./delta";

/** Sent when iframe loads or parent connects - handshake message */
export interface Init {
  t: "init";
  message: string;
}

/** Connection test message */
export interface Ping {
  t: "ping";
  message: string;
}

/** Response to ping */
export interface Pong {
  t: "pong";
  message: string;
}

/**
 * The main sync message - contains full document state.
 * Currently uses full-state sync (not diff-based).
 */
export interface EditorState {
  t: "state";
  state: HazelDoc;
}

/** Messages sent from Hazel iframe to parent (Patchwork) */
export type HazelToParent = Init | Ping | Pong | EditorState;

/** Messages sent from parent (Patchwork) to Hazel iframe */
export type ParentToHazel = Init | Ping | Pong | EditorState;
