/** Endpoints for clients using the {e Smyth} library.

    Two main endpoints are exposed: the {!section:solve} endpoint and the
    {!section:test} endpoint.
    A third endpoint, {!section:assertion}, is also provided for the more niche
    case of gathering information about assertions encoded in sketches.

    These endpoints take in the surface-level string representations of their
    inputs. */;

/** The type of errors that clients might receive. */

type error =
  | ParseError(list(Bark.dead_end(Parse.context, Parse.problem)))
  | TypeError((Lang.exp, Type.error))
  | EvalError(string)
  | TimedOut(float)
  | NoSolutions
  | /** The given partial assertions are not a subset of the full assertions. */
    PartialNotSubsetFull;

/** The response type that clients will receive. */

type response('a) = result('a, error);

/** {1:solve Solve} */;

/** The result of a successful "solve" operation. */

type solve_result = {
  /** A list of hole fillings that satisfy the constraints of a sketch. */
  hole_fillings: list(list((Lang.hole_name, Lang.exp))),
  /** The time taken to produce the valid hole fillings. */
  time_taken: float,
};

/** [solve sketch] tries to return a {!solve_result} that satisfies the
    assertions in [sketch]. */

let solve: (~sketch: string) => response(solve_result);

/** {1:test Test} */;

/** The result of a successful "test" operation. */

type test_result = {
  /** The number of assertions in the specification. */
  specification_assertion_count: int,
  /** The number of assertions in the sketch. */
  assertion_count: int,
  /** Whether or not the top-ranked solution is valid. */
  top_success: bool,
  /** Whether or not the top-ranked recursive solution is valid. */
  top_recursive_success: bool,
  /** The time taken to produce these results. */
  time_taken: float,
};

/** [test specification sketch examples] synthesizes hole fillings for
    [sketch] using [examples], then tests these hole fillings for validity
    against [specification].

   In typical use, [specification] and [examples] are lists of assertions, and
   [sketch] is a partially-completed program with no assertions. */

let test:
  (~specification: string, ~sketch: string, ~examples: string) =>
  response(test_result);

/** A convenience wrapper for {!test} that takes in a list of parsed assertions
    rather than a string. */

let test_assertions:
  (
    ~specification: string,
    ~sketch: string,
    ~assertions: list((Lang.exp, Lang.exp))
  ) =>
  response(test_result);

/** {1:assertion Assertion Info} */;

/** [assertion_info specification assertions] returns a list consisting of
    values [(included, [x0, ..., xN], y)] for each assertion in
    [specification], which should each be of the form [f x0 ... xN == y].
    [included] is true if and only if the corresponding assertion is also in
    [assertions]. */

let assertion_info:
  (~specification: string, ~assertions: string) =>
  response(list((bool, list(Lang.exp), Lang.exp)));
