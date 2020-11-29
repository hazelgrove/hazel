module HoleMap = {
  include Map.Make({
    type t = CursorPath.steps;
    let compare = CursorPath.compare_steps;
  });
};

type t = HoleMap.t(fill_state)
and fill_state =
  | Filled(UHExp.t, t)
  | Filling(ZList.t(UHExp.t, UHExp.t));

let t_of_sexp = _ => failwith("Synthesizing.t_of_sexp todo");
let sexp_of_t = _ => failwith("Synthesizing.sexp_of_t todo");

let empty = HoleMap.empty;
let is_empty = HoleMap.is_empty;

let of_seq = HoleMap.of_seq;

let bindings = HoleMap.bindings;

let find_opt = HoleMap.find_opt;
