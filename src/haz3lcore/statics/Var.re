open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = string;

let gen: Base_quickcheck.Generator.t(t) = Base_quickcheck.Generator.string;
let eq = String.equal;

let length = String.length;

let is_true = eq("true");

let is_false = eq("false");

let is_let = eq("let");

let is_fun = eq("fun");

let is_case = eq("case");

let is_wild = eq("_");

let split = (pos, name) => {
  let left_var = String.sub(name, 0, pos);
  let right_var = String.sub(name, pos, String.length(name) - pos);
  (left_var, right_var);
};

/* Used for VarBstMap */
let compare = (x: t, y: t) => compare(x, y);
