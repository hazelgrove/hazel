open Sexplib.Std;

[@deriving sexp]
type t = string;

let of_string = s' => s';
let to_string = s => s;

let apply = (f, s) => s |> to_string |> f |> of_string;

let empty = String.empty;
let length = String.length;
