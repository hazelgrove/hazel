open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {step: int};

let initial: t = {step: 0};

let take_step = ({step}) => {step: step + 1};
let reset_step = ({step}) => {step: step * 0};
let get_step = ({step}) => step;

let time_out = ({step}) => step > 100;
