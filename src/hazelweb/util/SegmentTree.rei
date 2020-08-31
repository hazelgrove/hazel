/**
 * A data structure that stores a union of intervals whose
 * endpoints lie in a finite, fixed set of points
 *
 * cf Section 1.2.3.1 of Computational Geometry: An Introduction
 * by Preparata & Shamos
 */
type t;
type interval = (float, float);

/**
 * `mk(fs)` initializes a segment tree that may store intervals
 * with endpoints in `fs`
 */
let mk: list(float) => t;

/**
 * Raises `Invalid_argument` if the interval has endpoints
 * outside of those specified at initialization
 */
let insert: (interval, t) => t;

/**
 * Raises `Invalid_argument` if the interval has endpoints
 * outside of those specified at initialization
 */
let delete: (interval, t) => t;

/**
 * `complement_intersection(i, tree)` returns the intersection
 * of interval `i` with the complement of the union of intervals
 * in `tree`
 */
let complement_intersection: (interval, t) => list(interval);
