/**
 * An alternating list with an even number of elements,
 * starting with elements of type 'b
 */
[@deriving sexp]
type blist('b, 'a) = list(('b, 'a));
/**
 * An alternating list with an odd number of elements,
 * starting with elements of type 'a
 */
[@deriving sexp]
type alist('a, 'b) = ('a, blist('b, 'a));

[@deriving sexp]
type t('a, 'b) = alist('a, 'b);

let get_as: t('a, 'b) => list('a);
let get_bs: t('a, 'b) => list('b);
