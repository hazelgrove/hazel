/* Adventure Mode: Interactive Tutorial System
 *
 * This module defines the core types for adventure mode - an interactive
 * guided tutorial system that introduces users to Hazel features.
 *
 * The adventure "agent" can display messages, perform editor actions,
 * and wait for users to complete tasks before advancing.
 */

open Util;
open Haz3lcore;

/* Message configuration */
[@deriving (show({with_path: false}), sexp, yojson)]
type message_config = {
  text: string,
  can_advance: bool /* Show "Next" button */
};

/* Agent action configuration */
[@deriving (show({with_path: false}), sexp, yojson)]
type agent_action_config = {
  actions: list(Action.t),
  narration: option(string) /* Text shown during action */
};

/* Gate predicate for user tasks */
[@deriving (show({with_path: false}), sexp, yojson)]
type gate_predicate =
  | HasAnyProbe /* User has added any probe */
  | HasProbeOnIndicated /* Probe on currently indicated term */
  | TextContains(string) /* Editor text contains this substring */
  | TextEquals(string) /* Editor text exactly equals this */
  | TermSatisfies(string) /* Description for debugging; actual check is via custom fn */
  | And(list(gate_predicate))
  | Or(list(gate_predicate));

/* Gate configuration for user tasks */
[@deriving (show({with_path: false}), sexp, yojson)]
type gate_config = {
  predicate: gate_predicate,
  hint: string, /* Help text shown to user */
  action_threshold: int /* Actions before suggesting reset */
};

/* Adventure script step */
[@deriving (show({with_path: false}), sexp, yojson)]
type step =
  | Message(message_config)
  | AgentAction(agent_action_config)
  | UserGate(gate_config)
  | Checkpoint
  | LoadEditor(Zipper.t);

/* A complete adventure script */
[@deriving (show({with_path: false}), sexp, yojson)]
type script = {
  id: string,
  title: string,
  steps: list(step),
};

/* Helper constructors for cleaner script authoring */

let message = (~can_advance=true, text): step =>
  Message({
    text,
    can_advance,
  });

let agent_action = (~narration=?, actions): step =>
  AgentAction({
    actions,
    narration,
  });

let user_gate = (~action_threshold=15, ~hint="", predicate): step =>
  UserGate({
    predicate,
    hint,
    action_threshold,
  });

let checkpoint: step = Checkpoint;

let load_editor = (zipper): step => LoadEditor(zipper);
