[@deriving sexp]
type t =
  | /** Library path, containing an absolute path. */
    ImportLib(Path.t)
  | /** Relative import path, contianing a relative path. */
    ImportRel(Path.t);

let get =
  fun
  | ImportLib(path) => `Lib(path)
  | ImportRel(path) => `Rel(path);

let of_path = path =>
  switch (path) {
  | path when Path.is_abs(path) => ImportLib(path)
  | path => ImportRel(path)
  };

let of_string = str => str |> Path.v |> of_path;

let to_path =
  fun
  | ImportLib(path) => Path.strip_root(path) |> Option.get
  | ImportRel(path) => path;

let to_string = path => path |> to_path |> Path.to_string;

let is_lib =
  fun
  | ImportLib(_) => true
  | ImportRel(_) => false;

let is_rel =
  fun
  | ImportLib(_) => false
  | ImportRel(_) => true;
