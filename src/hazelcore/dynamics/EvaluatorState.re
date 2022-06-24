open Sexplib.Std;

[@deriving sexp]
type t = {step: int};

let init = {step: 0};

let take_step = ({step, _}) => {step: step + 1};
