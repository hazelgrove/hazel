open Sexplib.Std;

module StmtLoc = {
  [@deriving sexp]
  type t = list(int);

  let init =  [0];

  let next =
    fun
    | [i, ...is] => [i + 1, ...is]
    | [] => failwith("unreachable nil list");

  let nest = is => [0, ...is];

  let compare = List.compare(Int.compare);
};
