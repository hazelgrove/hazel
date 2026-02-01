/**
 * Hazel-Patchwork PostMessage Protocol
 *
 * These types define the messages sent between the Hazel iframe and its parent.
 * Communication is bidirectional via window.postMessage().
 *
 * Type Conversion Flow:
 *   TypeScript (this file) --[ts2ocaml]--> OCaml (PatchworkMessages.mli)
 *
 * To regenerate OCaml types after modifying this file:
 *   cd embed && pnpm type:patchworkmessages
 *
 * Message handling:
 *   - Parent side: HazelEmbed.tsx listens for HazelToParent messages
 *   - Iframe side: PatchworkComm.re listen() handles ParentToHazel messages
 *
 * Typical message flow:
 *   1. Iframe loads, sends Init to parent (signals readiness)
 *   2. Parent responds with EditorState (full document)
 *   3. User edits in iframe -> iframe sends EditorState to parent
 *   4. Parent stores in Automerge, syncs to other clients
 *   5. Other client's edit arrives -> parent sends EditorState to iframe
 *   6. Iframe applies change via SyncReplace action
 */

import type { HazelDoc } from "./flatdoc";

/** Sent when iframe loads to signal readiness to receive state */
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
 * The main sync message - contains document state delta.
 *
 * - `state`: Changed/added pieces (partial HazelDoc with only affected pieces)
 * - `deleted`: IDs of pieces to remove from Automerge
 *
 * Why explicit deletion? Hazel uses a tree structure where deleted pieces
 * simply disappear. Automerge uses a flat map where pieces persist unless
 * explicitly removed. Without explicit deletion, deleted pieces become
 * "orphans" in Automerge, causing undo/redo sync to fail: when undo restores
 * a piece, it's already in Automerge (unchanged), so it's not forwarded to
 * other clients, who then crash when the parent references a missing piece.
 *
 * See docs/patchwork-integration.md "Explicit Deletion Sync" for details.
 */
export interface EditorState {
  t: "state";
  state: HazelDoc;
  deleted?: string[];
}

/**
 * Caret position update - sent from Hazel iframe when local caret moves.
 * Used for collaborative cursor display via ephemeral broadcast.
 *
 * Position model:
 * - pieceId: ID of the piece the caret is "on" (first of right siblings, or last of left if at end)
 * - shardIdx: For tiles, which shard (delimiter) of the tile. null for non-tiles.
 *               Multi-shard tiles (let/in, if/then/else) share one ID across all shards.
 *               We need shardIndex to look up the correct shard's measurement.
 * - caretOffset: 0 = Outer (at piece's left edge), n = Inner(n-1) (n columns into the piece)
 * - shape: Caret shape for rendering at piece boundaries (null when inside a piece)
 * - side: Which edge of the piece the caret is on when at Outer position.
 *         "left" = caret is at left edge of piece (normal case, piece is to the right)
 *         "right" = caret is at right edge of piece (end-of-segment, piece is to the left)
 *         null = caret is inside the piece (Inner position)
 */
export interface CaretUpdate {
  t: "caret";
  pieceId: string;
  shardIdx: number | null;
  caretOffset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}

/**
 * Remote user's caret position - sent from parent to iframe.
 * Contains user identification and styling info for rendering.
 */
export interface RemoteCaret {
  t: "remote-caret";
  userId: string;
  userName?: string; // Display name for label (undefined if not available)
  color: string;
  pieceId: string;
  shardIdx: number | null;
  caretOffset: number;
  shape: "left" | "right" | null;
  side: "left" | "right" | null;
}

/**
 * Notification that a remote user disconnected - sent from parent to iframe.
 * Iframe should remove the corresponding remote caret from display.
 */
export interface RemoteCaretRemove {
  t: "remote-caret-remove";
  userId: string;
}

/** Messages sent from Hazel iframe to parent (Patchwork) */
export type HazelToParent = Init | Ping | Pong | EditorState | CaretUpdate;

/** Messages sent from parent (Patchwork) to Hazel iframe */
export type ParentToHazel = Ping | Pong | EditorState | RemoteCaret | RemoteCaretRemove;
