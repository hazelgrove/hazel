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
type structure =
  | Node(Id.t);

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
type chooser =
  | Specific(ProjectorCore.Kind.t)
  | ChooseLivelit;

/* This type defines the top-level actions used to manage
 * projectors,as distinguished from external_action,
 * which defines the actions available internally to all projectors,
 * and from each projector's own internal action type */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type project =
  | SetIndicated(chooser) /* Project syntax at caret */
  | RemoveIndicated /* Remove projector at caret */
  | SetSyntax(Id.t, Base.segment) /* Set underlying syntax */
  | SetModel(Id.t, string) /* Set serialized projector model */
  | Focus(Id.t, ProjectorCore.Kind.t, option(Util.Direction.t)) /* Pass control to projector */
  | Escape(Id.t, Direction.t); /* Pass control to parent editor */

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

/*
 * ------------------------------
 *  Structure-Based Action Language
 * ------------------------------
 */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type via =
  | NameAndIdx(string, option(int))
  | Stepwise(Direction.t);

// --- Navigation Actions ---
// These actions are used to navigate the AST, and do not modify the program
// or provide additional information to the LLM. They strictly move the cursor
// through the AST.
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type nav_action =
  // Goes to the parent node of the current node in the AST
  | GoToParent
  // Goes to the child node of the current node in the AST
  | GoToChild(string, option(int))
  // Jumps to the root node of the AST
  | GoToSibling(via);

// --- File-Read Actions ---
// These actions are used purely to read information from the program,
// and do not modify the program or the cursor location in the AST.

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type read_action =
  // Displays the definition of the current node in the AST
  | ViewDefinition;

// --- Edit Actions ---
// These actions are used to modify the program. They do provide additional
// information to the LLM (via reading), but may move the cursor (eg. removing
// a node will require the cursor to be moved elsewhere).
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type edit_action =
  | UpdateAll(string)
  | UpdateDefinition(string)
  | UpdateBody(string)
  | UpdatePattern(string)
  | UpdateBindingClause(string)
  | DeleteBindingClause
  | DeleteBody
  | InsertAfter(string)
  | InsertBefore(string);

// AddToolLabel_1.0: Make the action types (above) and add their cases to the funs (below)
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type composition_action =
  | Nav(nav_action)
  | Read(read_action)
  | Edit(edit_action);

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
  | AssistantComposition(composition_action)
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
  | AssistantComposition(Edit(_))
  | Dump => true
  | Copy
  | Move(_)
  | Select(_)
  | AssistantComposition(Nav(_))
  | AssistantComposition(Read(_))
  | Unselect(_) => false
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | Escape(_) => false
    };

/* Determines whether undo/redo skips action */
let is_historic: t => bool =
  fun
  | Copy
  | Move(_)
  | Select(_)
  | AssistantComposition(Nav(_))
  | AssistantComposition(Read(_))
  | Unselect(_) => false
  | Cut
  | Buffer(Accept | Clear | Set(_))
  | Paste(_)
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Put_down
  | Introduce
  | AssistantComposition(Edit(_))
  | Dump => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | Escape(_) => false
    };

let prevent_in_read_only_editor = (a: t) => {
  switch (a) {
  | Copy
  | Move(_)
  | Unselect(_)
  | AssistantComposition(Nav(_))
  | AssistantComposition(Read(_))
  | Select(_) => false
  | Buffer(Set(_) | Accept | Clear)
  | Cut
  | Paste(_)
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Put_down
  | Introduce
  | AssistantComposition(Edit(_))
  | Dump => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_) => true
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated
    | Focus(_)
    | Escape(_) => false
    }
  };
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
  | AssistantComposition(_)
  | Dump => true;
