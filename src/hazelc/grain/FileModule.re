open Module;

[@deriving sexp]
type t = {
  path: import_path,
  prog,
};

let mk = (path, prog) => {path, prog};
let mk_lib = path => {path, prog: ([], [])};

let path = ({path, _}) => path;
let prog = ({prog, _}) => prog;

let to_module = prog;
