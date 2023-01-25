type error_status =
  | Occurs;

type t = UnionFind.elem((mut_eq_typs, option(error_status)))
and mut_eq_typs = list(mut_eq_typ)
and mut_eq_typ =
  | Base(EqClass.base_typ)
  | Mapped(EqClass.unary_ctor, t)
  | Compound(EqClass.binary_ctor, t, t);

let snapshot_class: (t, ITyp.t) => (EqClass.t, option(error_status));

let eq_class_to_mut_eq_class: EqClass.t => t;
let eq_typ_to_mut_eq_typ: EqClass.eq_typ => mut_eq_typ;

let derive_nested_keys_and_eq_classes: ITyp.t => (list(ITyp.t), list(t));

let union: (t, t) => unit;

let mark_failed_occurs: t => unit;
