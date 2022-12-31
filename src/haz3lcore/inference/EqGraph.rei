type t = Hashtbl.t(ITyp.t, MutableEqClass.t);

let create: unit => t;

let add_typ_as_node: (t, ITyp.t) => unit;

let equate_nodes: (t, ITyp.t, ITyp.t) => unit;

let equate_node_to_primitive_typ: (t, ITyp.t, ITyp.t) => unit;

let make_occurs_check: (t, ITyp.t, ITyp.t) => unit;
