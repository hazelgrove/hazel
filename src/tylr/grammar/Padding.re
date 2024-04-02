open Sexplib.Std;

// specifies preferred whitespace padding around the contents of a cell
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  // whether to pad cell contents in horizontal layout
  space: bool,
  // whether to indent cell contents in vertical layout
  indent: bool,
};

let mk = (~space=true, ~indent=true, ()) => {space, indent};

// padding for space cells
let empty = mk(~space=false, ~indent=false, ());

let root = mk(~space=false, ~indent=false, ());
