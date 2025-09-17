import type { HazelDoc } from "./delta";

export interface Init {
  t: "init";
  message: string;
}

export interface Ping {
  t: "ping";
  message: string;
}

export interface Pong {
  t: "pong";
  message: string;
}

export interface EditorState {
  t: "state";
  state: HazelDoc;
}

export interface CaretPosition {
  t: "caret";
  pieceId: string;
  caretOffset: number;
}

export type HazelToParent = Init | Ping | Pong | EditorState | CaretPosition;
export type ParentToHazel = Init | Ping | Pong | EditorState | CaretPosition;
