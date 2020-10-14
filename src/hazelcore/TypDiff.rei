type t =
  | Hole(bool)
  | Int(bool)
  | Float(bool)
  | Bool(bool)
  | Arrow(t, t, bool)
  | Sum(t, t, bool)
  | Prod(list(t), bool)
  | List(t, bool);

let precedence: t => int;

let mk_diff: (HTyp.t, HTyp.t) => (t, t);
