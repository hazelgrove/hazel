open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a) = list('a);

let of_items = items => items;

let items = closed => closed;

let map = List.map;

let filter = List.filter;
