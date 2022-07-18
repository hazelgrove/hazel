open Sexplib.Std;

[@deriving sexp]
type t = {step: int};

let initial: t = {step: 0};

let step = ({step}) => {step: step + 1};
let step_count = ({step}) => step;
