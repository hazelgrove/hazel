/* The items of a closed signature: each item's type is written in terms of
   the context outside the signature, so no item mentions a sibling and the
   items are independent of one another and of their order. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('item);

/* Items already closed. Call this only from Typ.close, which closes the
   items of a signature as written; from module synthesis in Statics, whose
   members are typed outside the module as they are checked; and from tests.
   Anywhere else, items are read as written and must go through Typ.close. */
let of_items: list('item) => t('item);

let items: t('item) => list('item);

let map: ('a => 'b, t('a)) => t('b);

let filter: ('item => bool, t('item)) => t('item);

/* [f] applied to each item, given the items after it; None if any is. */
let map_option:
  ((~later: list('a), 'a) => option('b), t('a)) => option(t('b));
