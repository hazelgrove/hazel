open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: string,
};

let space = " ";
let linebreak = "⏎"; //alternative: "¶"

let mk_space = id => {content: space, id};

let is_space: t => bool = w => w.content == space;
let is_linebreak: t => bool = w => w.content == linebreak;

let id = w => w.id;
