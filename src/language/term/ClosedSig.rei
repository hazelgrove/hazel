/* The items of a closed module signature: each item's type is written in
   terms of the context outside the signature, so no item mentions a sibling
   and the items are independent of one another and of their order. The type
   is abstract: building one asserts that of its items. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a);

/* Items whose types are already written in terms of the context outside
   the signature. */
let of_items: list('a) => t('a);

let items: t('a) => list('a);

let map: ('a => 'b, t('a)) => t('b);

let filter: ('item => bool, t('item)) => t('item);

/* [f] applied to each item; None if any is. */
let map_option: ('a => option('b), t('a)) => option(t('b));
