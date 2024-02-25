open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {step: int};

let initial: t = {step: 0};

let take_step = ({step}) => {step: step + 1};
let get_step = ({step}) => step;
let put_step = step => {step: step};
