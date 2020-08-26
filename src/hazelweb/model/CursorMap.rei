/**
 * An immutable map from (row, col) character positions
 * in a program editor to {!type:CursorPath_common.rev_t} values.
 * Used to handle cursor movement.
 *
 * Unenforced assumptions:
 * - monospaced font
 * - every present row contains at least one cursor position
 * - indices of present rows form a contiguous range [0, max]
 */
type t;

module Row: {
  [@deriving sexp]
  type t = int;
};
module Col: {
  [@deriving sexp]
  type t = int;
};
[@deriving sexp]
type binding = ((Row.t, Col.t), CursorPath_common.rev_t);

/**
 * Builds a cursor map given a {!type:UHLayout.t}.
 * Returns, in addition to the cursor map, a binding
 * pointing to the current cursor location if found.
 */
let mk: UHLayout.t => (t, option(binding));

/**
 * All movement functions will throw {!exception:Not_found}
 * if the given {!type:Row.t} is not present in the map.
 */

let move_up: ((Row.t, Col.t), t) => option(binding);
let move_down: ((Row.t, Col.t), t) => option(binding);

let move_left: (binding, t) => option(binding);
let move_right: (binding, t) => option(binding);

let move_sol: (Row.t, t) => binding;
let move_eol: (Row.t, t) => binding;

let find_nearest_within_row: ((Row.t, Col.t), t) => binding;
