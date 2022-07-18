/**
   Identifier for environments.
 */

[@deriving sexp]
type t = int;

let init: t;
let equal: (t, t) => bool;

let next: t => t;

let invalid: t;
let is_invalid: t => bool;
