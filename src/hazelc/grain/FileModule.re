let import = (path, ~name, ~from) => {
  let path =
    path
    |> Path.relativize(~root=from)
    |> Option.value(~default=path)
    |> ImportPath.of_path;

  Import.mk(name, path);
};

module Full = {
  [@deriving sexp]
  type t = (Path.t, Module.t);

  let mk = (path, modl) => (path, modl);

  let path = ((path, _)) => path;
  let modl = ((_, modl)) => modl;

  let import = fmodl => import(path(fmodl));
};

module Stub = {
  [@deriving sexp]
  type t = Path.t;

  let mk = path => path;

  let path = path => path;

  let import = fmodl => import(path(fmodl));
};

[@deriving sexp]
type full = Full.t;

[@deriving sexp]
type stub = Stub.t;

[@deriving sexp]
type t =
  | Full(full)
  | Stub(stub);

let of_full = ful => Full(ful);
let of_stub = stb => Stub(stb);

let to_full =
  fun
  | Full(ful) => Some(ful)
  | Stub(_) => None;
let to_stub =
  fun
  | Full(_) => None
  | Stub(stb) => Some(stb);

let mk = (path, modl) => Full.mk(path, modl) |> of_full;
let mk' = path => Stub.mk(path) |> of_stub;

let path =
  fun
  | Full(ful) => ful |> Full.path
  | Stub(stb) => stb |> Stub.path;

let modl =
  fun
  | Full(ful) => Some(ful |> Full.modl)
  | Stub(_) => None;

let import = fmodl => import(path(fmodl));
