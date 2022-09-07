open Sexplib.Std;
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  content: string,
};

let space = " ";
let linebreak = "⏎"; //alternative: "¶"

let is_space: t => bool = w => w.content == space;
let is_linebreak: t => bool = w => w.content == linebreak;

let mk = content => {
  open IdGen.Syntax;
  let+ id = IdGen.fresh;
  {id, content};
};
let mk_space = mk(space);
let mk_linebreak = mk(linebreak);
