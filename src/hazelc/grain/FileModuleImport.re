open Expr;
open Module;

[@deriving sexp]
type t = {
  name: ident,
  path: import_path,
};

let mk = (fmd, name) => {name, path: FileModule.path(fmd)};

let name = ({name, _}) => name;
let path = ({path, _}) => path;

let import = fmdi => (name(fmdi), path(fmdi));

let ident = (fmdi, x) => name(fmdi) ++ "." ++ x;
let var = (fmdi, x) => x |> ident(fmdi) |> Expr.var;
let pvar = (fmdi, x) => x |> ident(fmdi) |> Expr.pvar;

let ap = (fmdi, fn, args) => Expr.ap(var(fmdi, fn), args);
let ap1 = (fmdi, fn, arg) => ap(fmdi, fn, [arg]);
let ap2 = (fmdi, fn, arg1, arg2) => ap(fmdi, fn, [arg1, arg2]);
let ap3 = (fmdi, fn, arg1, arg2, arg3) => ap(fmdi, fn, [arg1, arg2, arg3]);
let ap4 = (fmdi, fn, arg1, arg2, arg3, arg4) =>
  ap(fmdi, fn, [arg1, arg2, arg3, arg4]);
let ap5 = (fmdi, fn, arg1, arg2, arg3, arg4, arg5) =>
  ap(fmdi, fn, [arg1, arg2, arg3, arg4, arg5]);

let ctor = (fmdi, name, args) => Expr.ctor(ident(fmdi, name), args);
let ctor0 = (fmdi, name) => var(fmdi, name);
let ctor1 = (fmdi, name, arg) => ctor(fmdi, name, [arg]);
let ctor2 = (fmdi, name, arg1, arg2) => ctor(fmdi, name, [arg1, arg2]);
let ctor3 = (fmdi, name, arg1, arg2, arg3) =>
  ctor(fmdi, name, [arg1, arg2, arg3]);
let ctor4 = (fmdi, name, arg1, arg2, arg3, arg4) =>
  ctor(fmdi, name, [arg1, arg2, arg3, arg4]);
let ctor5 = (fmdi, name, arg1, arg2, arg3, arg4, arg5) =>
  ctor(fmdi, name, [arg1, arg2, arg3, arg4, arg5]);

let pctor = (fmdi, name, args) => Expr.pctor(ident(fmdi, name), args);
let pctor0 = (fmdi, name) => pvar(fmdi, name);
let pctor1 = (fmdi, name, arg) => pctor(fmdi, name, [arg]);
let pctor2 = (fmdi, name, arg1, arg2) => pctor(fmdi, name, [arg1, arg2]);
let pctor3 = (fmdi, name, arg1, arg2, arg3) =>
  pctor(fmdi, name, [arg1, arg2, arg3]);
let pctor4 = (fmdi, name, arg1, arg2, arg3, arg4) =>
  pctor(fmdi, name, [arg1, arg2, arg3, arg4]);
let pctor5 = (fmdi, name, arg1, arg2, arg3, arg4, arg5) =>
  pctor(fmdi, name, [arg1, arg2, arg3, arg4, arg5]);
