[@deriving (show({with_path: false}), sexp, yojson)]
type t = (MetaVar.t, HoleInstanceId.t);

let u_of = ((u, _): t): MetaVar.t => u;
let i_of = ((_, i): t): HoleInstanceId.t => i;

let result: t = ((-1), 0);
