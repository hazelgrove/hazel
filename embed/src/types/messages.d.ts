import type { EditScript } from "./delta";

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

export interface EditorDelta {
  t: "delta";
  delta: EditScript;
}

export type HazelToParent = Init | Ping | Pong | EditorDelta;
export type ParentToHazel = Init | Ping | Pong | EditorDelta;
