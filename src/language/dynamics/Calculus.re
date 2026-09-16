/* The family of incremental-evaluation calculi from the incremental
 * evaluation paper, selectable at runtime so that one binary can run a
 * benchmark trace under every re-calculation scheme.
 *
 * Hazel's shipped evaluator is APL: re-use keyed by expression id, with
 * provenance carrying a projection path (aP), and cache entries carrying the
 * probe-sample slice an expression contributes (aL).
 *
 * The calculi live together behind this flag rather than on separate
 * branches, so one build can run a benchmark trace under all of them and the
 * timings are comparable. A mode whose machinery is not implemented yet
 * reports itself unavailable rather than silently running a different scheme
 * under the wrong name. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  /* Section 4: reference lambda calculus, no cache. */
  | A0
  /* Sections 7 and 10: id + projection-path re-use, with log slices. */
  | APL
  /* Section 8: adds tuple-shaped clean flags. */
  | AM
  /* Section 6: adds callstack-keyed cache entries. */
  | A2
  /* Figure "calculi": aM and a2 and aL combined. */
  | AStar;

/* Which machinery each mode turns on. The evaluator branches on these
 * capabilities rather than on the mode itself, so each calculus branch
 * touches only the field it introduces. */
type capabilities = {
  /* Consult and populate the re-use cache at all. False only for A0, the
   * non-incremental control, which must also skip maintaining the re-use map
   * so that it does not pay for bookkeeping it never reads. */
  reuse: bool,
  /* aM: clean flags mirror the shape of a tuple value, so that editing one
   * component of a tuple only dirties the bindings projected from it. */
  tuple_flags: bool,
  /* a2: entries are keyed by (callstack, id) rather than by id alone, which
   * lifts the restriction that nothing inside a function call is cached. */
  callstack_keys: bool,
};

/* Flipped as each calculus's machinery lands. Separate declarations so that
 * landing one does not touch the other's line. */
let supports_tuple_flags = false;
let supports_callstack_keys = false;

/* Depth limit for a2's guard G(e, c): an expression is cacheable only when
 * the callstack is no deeper than this.
 *
 * Section 6 leaves G open, and this particular choice has no proof attached
 * to it yet, so it may not survive into the paper. It is a single constant on
 * purpose: dropping the guard is `max_int`, and recovering aPL exactly is 0
 * (section 6's own theorem that an empty-callstack guard recovers id-reuse),
 * which also makes it the knob to sweep when measuring the trade-off. */
let callstack_depth_limit = 4;

let capabilities = (mode: t): capabilities =>
  switch (mode) {
  | A0 => {
      reuse: false,
      tuple_flags: false,
      callstack_keys: false,
    }
  | APL => {
      reuse: true,
      tuple_flags: false,
      callstack_keys: false,
    }
  | AM => {
      reuse: true,
      tuple_flags: true,
      callstack_keys: false,
    }
  | A2 => {
      reuse: true,
      tuple_flags: false,
      callstack_keys: true,
    }
  | AStar => {
      reuse: true,
      tuple_flags: true,
      callstack_keys: true,
    }
  };

/* A mode is runnable only if every capability it asks for is implemented. */
let is_available = (mode: t): bool => {
  let c = capabilities(mode);
  (!c.tuple_flags || supports_tuple_flags)
  && (!c.callstack_keys || supports_callstack_keys);
};

let all: list(t) = [A0, APL, AM, A2, AStar];

let available: list(t) = List.filter(is_available, all);

/* The mode Hazel evaluates in when nothing asks for a specific one. */
let default: t = APL;

let name = (mode: t): string =>
  switch (mode) {
  | A0 => "a0"
  | APL => "aPL"
  | AM => "aM"
  | A2 => "a2"
  | AStar => "aStar"
  };

let of_name = (s: string): option(t) =>
  List.find_opt(mode => name(mode) == s, all);

let describe = (mode: t): string =>
  switch (mode) {
  | A0 => "reference: re-evaluate everything, no cache"
  | APL => "id and projection-path re-use with log slices"
  | AM => "aPL plus tuple-shaped clean flags"
  | A2 => "aPL plus callstack-keyed cache entries"
  | AStar => "aM and a2 combined"
  };
