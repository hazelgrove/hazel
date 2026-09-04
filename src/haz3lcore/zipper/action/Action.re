open Util_web;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type chunkiness =
  | ByChar
  | ByToken
  /* Smart-rounded selection: char inside the starting token, whole
   * pieces once the focus has left that token's span. Only meaningful
   * for Select(Resize(Local | Vertical)) and Point-based selection. */
  | BySmart;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type goal =
  | Hole(Direction.t)
  | NextProblem(Direction.t)
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
  | Vertical(vertical, chunkiness)
  /* Point-based move/select. The optional chunkiness overrides the
   * selection_chunkiness setting for Select(Resize(Point(...))) drag;
   * `None` falls back to the settings-driven default. Ignored for
   * Move(Point(...)), which always lands the caret at the closest
   * grid position. */
  | Point(Point.t, option(chunkiness))
  | Goal(goal);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type rel =
  | Current
  | Id([@equal (_, _) => true] Id.t, Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type select =
  | All
  | PointToPoint((Point.t, Point.t))
  | Resize(move)
  | Smart(int)
  | Tile(rel)
  | Term(rel)
  | ToggleFocus
  | SetFocus(Direction.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type sample_focus =
  | Capture(Language.Sample.Capture.t, option(Id.t))
  | TogglePin(Language.CallStack.t)
  | SetIndex(int) /* Navigate to a specific depth in the call stack */
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
  | SampleFocus(sample_focus)
  | SetIndicated(chooser) /* Project syntax at caret */
  | RemoveIndicated /* Remove projector at caret */
  | SetSyntax(int, ProjectorCore.Kind.t, Base.segment) /* Set underlying syntax */
  | SetModel(int, ProjectorCore.Kind.t, string) /* Set serialized model (projector or refractor) */
  | Focus(int, ProjectorCore.Kind.t, option(Util_web.Direction.t)) /* Pass control to projector */
  | Escape(int, Direction.t) /* Pass control to parent editor */
  | EscapeToLineEnd(int, ProjectorCore.Kind.t); /* Pass control to parent editor, move to end of line */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type completion_source =
  | TyDi
  | LLM(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type buffer =
  | Set(completion_source)
  | Clear
  | Accept;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type paste = string;

module Structural = {
  /* A path identifies a let/type-alias binding by name, using `/` to
     address nested bindings (e.g. "a", "a/inner"). Resolved against
     the HighLevelNodeMap. */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type path = string;

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type code = string;

  /* Which sub-expression of a binding to target */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type target =
    | Definition /* RHS of `=`, before `in` */
    | Body /* expression after `in` */
    | Pattern /* LHS of `=`, after `let`/`type`; updates also rename use sites */
    | BindingClause; /* entire `let ... = ... in` / `type ... = ... in` */

  /* Where to insert relative to the target binding */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type insert_target =
    | After /* insert within body (after target binding) */
    | Before; /* insert before target binding (wrapping around it) */

  /* Targeted structural edits on bindings identified by path */
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t =
    | Update(target, path, code)
    | Delete(target, path)
    | Insert(insert_target, path, code);
};

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type probe =
  | ToggleManual
  | ToggleAuto
  | ToggleStatics
  | StepInto(Language.CallStack.t, Id.t)
  | Pin(Language.CallStack.t, Id.t)
  | RemoveAll;

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
  | PrettyPrint
  | Dump
  | ToggleLineComment
  | Structural(Structural.t);

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
  | PrettyPrint
  | Buffer(Accept | Clear | Set(_))
  | Structural(_)
  | Dump
  | ToggleLineComment => true
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
    | SampleFocus(_)
    | Escape(_)
    | EscapeToLineEnd(_) => false
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
  | PrettyPrint
  | Structural(_)
  | Dump
  | ToggleLineComment => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | SampleFocus(_)
    | Escape(_)
    | EscapeToLineEnd(_) => false
    }
  | Probe(_) => true;

let should_animate: t => bool =
  fun
  | Select(s) =>
    switch (s) {
    | All
    | Resize(_)
    | PointToPoint(_)
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
  | Structural(_)
  | Probe(_)
  | PrettyPrint
  | Dump
  | ToggleLineComment => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | SetIndicated(_)
    | RemoveIndicated
    | Focus(_)
    | SampleFocus(_)
    | Escape(_) => true
    | EscapeToLineEnd(_) => false
    };
