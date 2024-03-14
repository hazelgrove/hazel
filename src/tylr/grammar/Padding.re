// specifies preferred whitespace padding around the contents of a cell
[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t = {
  // whether to insert space on either end in horizontal layout
  space: (bool, bool),
  // whether to indent cell contents in vertical layout
  indent: bool,
};

let mk = (~l=false, ~r=false, ~indent=true, ()) => {space: (l, r), indent};

let root = mk(~indent=false, ());
