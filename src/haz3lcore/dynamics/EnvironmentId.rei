/**
  Identifier for environments.
 */

/**
  The type for identifiers.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = int;

/**
  [init] is the initial identifier.
 */
let init: t;

/**
  [equal ei ei'] is true if and only if [ei] and [ei'] are equal.
 */
let equal: (t, t) => bool;

/**
  [invalid] is an invalid identifier.
 */
let invalid: t;

/**
  [is_invalid ei] is [equal invalid ei].
 */
let is_invalid: t => bool;
