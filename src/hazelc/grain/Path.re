open Sexplib.Std;

type t = Fpath.t;

let sexp_of_t = path => path |> Fpath.to_string |> sexp_of_string;
let t_of_sexp = sexp =>
  switch (sexp |> string_of_sexp |> Fpath.of_string) {
  | Ok(path) => path
  | Error(`Msg(err)) =>
    raise(
      Sexplib0.Sexp.Of_sexp_error(Failure("sexp_of_Fpath.t: " ++ err), sexp),
    )
  };

let to_string = Fpath.to_string;

let of_fpath = Fpath.normalize;
let v = str => str |> Fpath.v |> of_fpath;
let of_string = str =>
  str
  |> Fpath.of_string
  |> Result.map(of_fpath)
  |> Result.map_error(
       fun
       | `Msg(error) => error,
     );

let segs = Fpath.segs;

let is_rel = Fpath.is_rel;
let is_abs = Fpath.is_abs;

let add_seg = Fpath.add_seg;
let append = Fpath.append;

let add_ext = Fpath.add_ext;

let split_base = Fpath.split_base;

let relativize = Fpath.relativize;
let strip_root = relativize(~root=Fpath.(v(dir_sep)));
let strip_root' = path => strip_root(path) |> Option.value(~default=path);
