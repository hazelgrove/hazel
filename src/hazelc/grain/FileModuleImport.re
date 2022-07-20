open Expr;
open Module;

[@deriving sexp]
type t = {
  name: var,
  path: import_path,
};

let mk = (fmd, name) => {name, path: FileModule.path(fmd)};

let name = ({name, _}) => name;
let path = ({path, _}) => path;

let import = fmdi => (name(fmdi), path(fmdi));

let ident = (fmdi, x) => name(fmdi) ++ "." ++ x;
let var = (fmdi, x) => x |> ident(fmdi) |> mk_var;

let ap = (fmdi, fn, args) => mk_ap(var(fmdi, fn), args);
let ap1 = (fmdi, fn, arg) => ap(fmdi, fn, [arg]);
let ap2 = (fmdi, fn, arg1, arg2) => ap(fmdi, fn, [arg1, arg2]);
let ap3 = (fmdi, fn, arg1, arg2, arg3) => ap(fmdi, fn, [arg1, arg2, arg3]);
let ap4 = (fmdi, fn, arg1, arg2, arg3, arg4) =>
  ap(fmdi, fn, [arg1, arg2, arg3, arg4]);

let ctor = (fmdi, name, args) => ECtor(ident(fmdi, name), args);
let ctor1 = (fmdi, name, arg) => ctor(fmdi, name, [arg]);
let ctor2 = (fmdi, name, arg1, arg2) => ctor(fmdi, name, [arg1, arg2]);
let ctor3 = (fmdi, name, arg1, arg2, arg3) =>
  ctor(fmdi, name, [arg1, arg2, arg3]);
let ctor4 = (fmdi, name, arg1, arg2, arg3, arg4) =>
  ctor(fmdi, name, [arg1, arg2, arg3, arg4]);

let pat_ctor = (fmdi, name, args) => PCtor(ident(fmdi, name), args);
let pat_ctor1 = (fmdi, name, arg) => pat_ctor(fmdi, name, [arg]);
let pat_ctor2 = (fmdi, name, arg1, arg2) =>
  pat_ctor(fmdi, name, [arg1, arg2]);
let pat_ctor3 = (fmdi, name, arg1, arg2, arg3) =>
  pat_ctor(fmdi, name, [arg1, arg2, arg3]);
let pat_ctor4 = (fmdi, name, arg1, arg2, arg3, arg4) =>
  pat_ctor(fmdi, name, [arg1, arg2, arg3, arg4]);
