// return all paths to the next steppable sub-expression
let next_steppables: DHExp.t => list(CursorPath.steps) =
  fun
  | EmptyHole(_)
  | BoolLit(_)
  | IntLit(_)
  | FloatLit(_)
  | NonEmptyHole(_) => [[]]
  | _ => failwith("not implemented");

// make dpaths using next_steppable
// use dpaths + pretty printed dhexp to figure out yellow highlight
// capture path of highlighted dhexp term that is clicked and pair with next result state
