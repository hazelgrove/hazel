let undoable_action = (action: Action.t): bool => {
  switch (action) {
  | UpdateApPalette(_)
  | Delete
  | Backspace
  | Construct(_) => true
  | MoveTo(_)
  | MoveToBefore(_)
  | MoveLeft
  | MoveRight
  | MoveToNextHole
  | MoveToPrevHole
  | ShiftLeft
  | ShiftRight
  | ShiftUp
  | ShiftDown => false
  };
};
