/**
  Generator for {!type:EnvironmentId.t}.
 */

/**
  The type for the {!type:EnvironmentId.t} generator.
 */
[@deriving (show({with_path: false}), sexp, yojson)]
type t;

/**
  [init] is the initial generator.
 */
let init: t;

/**
  [next eig] is [(ei, eig')] where [ei] is the next generated identifier.
 */
let next: t => (EnvironmentId.t, t);
