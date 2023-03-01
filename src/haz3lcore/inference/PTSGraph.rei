/**
 * An EqGraph is effectively a map from different types (which for inference, must always contain holes)
 * to their current equivalence classes. In some senses, the EqGraph is a condensed representation
 * of an undirected graph where all nodes are types and edges constitute equivalences.
 *
 * For more context:
 * The set of all constraints accumulated in static type inference constitutes a series of edges between
 * types that can be used to create a graph.
 * Consider the connected component a type is a member of. The solution associated with any
 * type in a connected component is the least upper bound of all types within it (if it exists).
 */

type t = Hashtbl.t(ITyp.t, MutablePotentialTypeSet.t);

let create: unit => t;

let add_typ_as_node: (t, ITyp.t) => unit;

let create_traversable_edge: (t, ITyp.t, ITyp.t) => unit;

let create_solution_edge: (t, ITyp.t, ITyp.t) => unit;

let make_occurs_check: (t, ITyp.t, ITyp.t) => unit;
