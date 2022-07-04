/**
   This module is concerned with the logic for loading and restoring program to
   and from a permalink.
 */

/**
   The type for the permalink.
 */
type t;

/**
   [update url model] updates [url] for [model].
 */
let put_model: (t, Model.t) => t;

/**
   [get_program url] returns the {!type:UHExp.t} encoded in [url], if there is
   one.
 */
let get_exp: t => option(UHExp.t);

/**
   [is_empty url] returns true if the fragment of [url] is an empty string.
 */
let is_empty: t => bool;

/**
   [get_fragment url] returns the fragment of [url].
 */
let get_fragment: t => string;

/**
   [put_fragment url frag] returns [url] with the new fragment [frag].
 */
let put_fragment: (t, string) => t;

/**
   [update_fragment f url] returns [url] with [f] applied to the URL fragment.
 */
let update_fragment: (string => string, t) => t;

/**
   [clear_fragment url] returns [url] with no hash fragment.
 */
let clear_fragment: t => t;

/**
   [set_current url] sets the current window's URL to [url].
 */
let set_current: t => unit;

/**
   [get_current ()] gets the current window's URL.
 */
let get_current: unit => option(t);
