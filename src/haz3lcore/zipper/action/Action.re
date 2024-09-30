include ActionBase;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ActionBase.t(Segment.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type project = ActionBase.project(Segment.t);

let of_piece_goal =
  fun
  | Grout => (
      fun
      | Piece.Grout(_) => true
      | _ => false
    );

let is_edit: t => bool =
  fun
  | Paste(_)
  | Cut
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down
  | Buffer(Accept | Clear | Set(_)) => true
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | ToggleIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | Escape(_) => false
    };

/* Determines whether undo/redo skips action */
let is_historic: t => bool =
  fun
  | Buffer(Set(_) | Clear)
  | Copy
  | Move(_)
  | Jump(_)
  | Select(_)
  | Unselect(_)
  | RotateBackpack
  | MoveToBackpackTarget(_) => false
  | Cut
  | Buffer(Accept)
  | Paste(_)
  | Reparse
  | Insert(_)
  | Destruct(_)
  | Pick_up
  | Put_down => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_)
    | SetModel(_)
    | ToggleIndicated(_)
    | RemoveIndicated => true
    | Focus(_)
    | Escape(_) => false
    };

let prevent_in_read_only_editor = (a: t) => {
  switch (a) {
  | Copy
  | Move(_)
  | Unselect(_)
  | Jump(_)
  | Select(_) => false
  | Buffer(Set(_) | Accept | Clear)
  | Cut
  | Paste(_)
  | Reparse
  | Destruct(_)
  | Insert(_)
  | Pick_up
  | Put_down
  | RotateBackpack
  | MoveToBackpackTarget(_) => true
  | Project(p) =>
    switch (p) {
    | SetSyntax(_) => true
    | SetModel(_)
    | ToggleIndicated(_)
    | RemoveIndicated
    | Focus(_)
    | Escape(_) => false
    }
  };
};
