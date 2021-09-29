open AltList;

/**
 * The frame around an element of type 'a
 * in a list of type AltList.t('a, 'b)
 */
type aframe('a, 'b) = (blist('b, 'a), blist('b, 'a));
/**
 * The frame around an element of type 'b
 * in a list of type t('a, 'b)
 */
type bframe('a, 'b) = (alist('a, 'b), alist('a, 'b));

let filter_a: ('a => bool, 'b => bool, aframe('a, 'b))