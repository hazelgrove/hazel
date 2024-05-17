open Sexplib.Std;

module rec Prop: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity;
  let eq: (t, t) => bool;
  let repr: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Atom(string)
    | And(t, t)
    | Or(t, t)
    | Implies(t, t)
    | Truth
    | Falsity;
  let eq = (x, y) => x == y;
  let rec repr =
    fun
    | Atom(s) => s
    | And(a, b) => Printf.sprintf("%s ∧ %s", repr(a), repr(b))
    | Or(a, b) => Printf.sprintf("%s ∨ %s", repr(a), repr(b))
    | Implies(a, b) =>
      if (b == Falsity) {
        Printf.sprintf("¬%s", repr(a));
      } else {
        Printf.sprintf("%s ⊃ %s", repr(a), repr(b));
      }
    | Truth => "⊤"
    | Falsity => "⊥";
};
