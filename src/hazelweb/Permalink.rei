/**
 * Exception raised when there is no URL set for the current window.
 */
exception EmptyCurrent;

/**
 * The type for the permalink.
 */
type t;

/**
 * {update url model} updates {url} for {model}.
 */
let put_program: (t, Program.t) => t;

/**
 * {get_program url} returns the {Program.t} encoded in {url}, if there is one.
 */
let get_program: t => option(Program.t);

/**
 * {set_current url} sets the current window's URL to {url}.
 */
let set_current: t => unit;

/**
 * {get_current ()} gets the current window's URL, throwing {EmptyCurrent} if
 * it is not set.
 */
let get_current: unit => t;
