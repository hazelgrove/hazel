type t = Hashtbl.t(ITyp.t, MutableEqClass.t);

let create: unit => t;

let add_typ_as_node: (t, ITyp.t) => unit;

let equate_typs: (t, ITyp.t, ITyp.t) => unit;

let make_occurs_check: (t, ITyp.t, ITyp.t) => unit;
