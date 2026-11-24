open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a) = list('a);

let of_items = items => items;

let items = closed => closed;

let map = (f, xs) => List.map(xs, ~f);

let filter = (f, xs) => List.filter(xs, ~f);
