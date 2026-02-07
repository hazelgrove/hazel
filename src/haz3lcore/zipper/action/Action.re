open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chunkiness =
  | ByChar
  | ByToken;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type goal =
  | Hole(Direction.t)
  | TileId([@equal (_, _) => true] Id.t)
  | BindingSiteOfIndicatedVar;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type vertical =
  | Up
  | Down;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type move =
  | Start
  | End
  | Line(Direction.t)
  | Local(Direction.t, chunkiness)
  | Vertical(vertical)
  | Point(Point.t)
  | Goal(goal);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type rel =
  | Current
  | Id([@equal (_, _) => true] Id.t, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type select =
  | All
  | Resize(move)
  | Smart(int)
  | Tile(rel)
  | Term(rel)
  | ToggleFocus
  | SetFocus(Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type sample_cursor =
  | Capture(Language.Sample.t, option(Id.t))
  | TogglePin(Language.Sample.call_stack)
  | Reset;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chooser =
  | Specific(ProjectorCore.Kind.t)
  | ChooseLivelit;

/* This type defines the top-level actions used to manage
 * projectors,as distinguished from external_action,
 * which defines the actions available internally to all projectors,
 * and from each projector's own internal action type */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type project =
  | SampleCursor(sample_cursor)
  | SetIndicated(chooser) /* Project syntax at caret */
  | RemoveIndicated /* Remove projector at caret */
  | SetSyntax(int, Base.segment) /* Set underlying syntax */
  | SetModel(int, ProjectorCore.Kind.t, string) /* Set serialized model (projector or refractor) */
  | Focus(int, ProjectorCore.Kind.t, option(Util.Direction.t)) /* Pass control to projector */
  | Escape(int, Direction.t); /* Pass control to parent editor */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type agent =
  | TyDi
  | LLM(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  | Set(agent)
  | Clear
  | Accept;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paste =
  | String(string)
  | Segment(Segment.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type read_action =
  // Lists all the use sites of the indicated variable
  | ShowUseSites(string)
  // Displays the typing context/scope at the current let expression in the AST
  // | ShowContext //todo: technically this is accomplished via showing sibs/parent
  // Displays the entire definition of the current node, with no child/sub definitions abstracted away
  | ShowReferences(string);

// --- Edit Actions ---

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type edit_action =
  | Initialize(string) // Code
  | UpdateDefinition(string, string) // Path, Code
  | UpdateBody(string, string) // Path, Code
  | UpdatePattern(string, string) // Path, Code
  | UpdateBindingClause(string, string) // Path, Code
  | DeleteBindingClause(string) // Path
  | DeleteBody(string) // Path
  | InsertAfter(string, string) // Path, Code
  | InsertBefore(string, string); // Path, Code

// AddToolLabel_1.0: Make the action types (above) and add their cases to the funs (below)
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type agent_editor_action =
  | Read(read_action) // Language server helpers
  | Edit(edit_action); // Main source of editing the codebase

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type probe =
  | ToggleManual
  | ToggleAuto
  | ToggleStatics
  | StepInto(Language.Sample.t, Id.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Reparse
  | Buffer(buffer)
  | Paste(paste)
  | Copy
  | Cut
  | Project(project)
  | Move(move)
  | Select(select)
  | Unselect(option(Direction.t))
  | Destruct(Direction.t)
  | Insert(string)
  | Put_down
  | Introduce
  | Probe(probe)
  | AgentEditorAction(agent_editor_action)
  | Dump;

module Failure = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Cant_move
    | Cant_insert
    | Cant_destruct
    | Cant_select
    | Cant_put_down
    | Cant_project
    | CantPaste
    | CantReparse
    | CantAccept
    | Cant_undo
    | Cant_redo
    | CantIntroduce
    | Composition_action_failure(string)
    | Cant_derive_local_AST_information;

  exception Exception(t);
};

module Result = {
  include Result;
  type t('success) = Result.t('success, Failure.t);
};

let is_edit: t => bool =
  fun
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Put_down
  | Introduce
  | Buffer(Accept | Clear | Set(_))
  | AgentEditorAction(_)
  | Dump => true
  | Copy
  | Move(_)
  | Select(_)
  | Unselect(_) => false
  | Project(p) =>
    switch (p) {
    | SetModel(_) => false
    | SetSyntax(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | SampleCursor(_)
    | Escape(_) => false
    }
  | Probe(_) => true;

/* Determines whether undo/redo skips action */
let is_historic: t => bool =
  fun
  | Copy
  | Move(_)
  | Select(_)
  | Unselect(_) => false
  | Cut
  | Buffer(Accept | Clear | Set(_))
  | Paste(_)
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Put_down
  | Introduce
  | AgentEditorAction(_)
  | Dump => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | SampleCursor(_)
    | Escape(_) => false
    }
  | Probe(_) => true;

let prevent_in_read_only_editor = (a: t) =>
  switch (a) {
  | Copy
  | Move(_)
  | Unselect(_)
  | Select(_) => false
  | Buffer(Set(_) | Accept | Clear)
  | Cut
  | Paste(_)
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Put_down
  | Introduce
  | AgentEditorAction(_)
  | Dump => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_) => true
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated
    | Focus(_)
    | SampleCursor(_)
    | Escape(_) => false
    }
  | Probe(_) => false
  };

/* Currently animations are disabled during drag selection
 * to paper over a weird interaction with scroll-to-caret.
 * There is assuredly a better way to handle it but the
 * approaches I tried weren't wholly successful. */
let should_animate: t => bool =
  fun
  | Select(s) =>
    switch (s) {
    | Resize(_) => false
    | All
    | Smart(_)
    | Tile(_)
    | Term(_)
    | ToggleFocus
    | SetFocus(_) => true
    }
  | Unselect(_)
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Introduce
  | Destruct(_)
  | Put_down
  | Buffer(Accept | Clear | Set(_))
  | Copy
  | Move(_)
  | Project(_)
  | AgentEditorAction(_)
  | Probe(_)
  | Dump => true;
