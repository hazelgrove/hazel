open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('item) = list('item);

let of_items = items => items;

let items = closed => closed;

let map = List.map;

let filter = List.filter;

let map_option = (f, items) => {
  let rec go = items =>
    switch (items) {
    | [] => Some([])
    | [item, ...later] =>
      switch (f(~later, item)) {
      | None => None
      | Some(b) => Option.map(rest => [b, ...rest], go(later))
      }
    };
  go(items);
};
