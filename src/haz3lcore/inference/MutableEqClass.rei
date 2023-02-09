/**
 * A mutable version of the EqClass.t type that allows extension via UnionFind
 * such that if one foo: MutableEqClass.t is extended (or unioned) with
 * bar: MutableEqClass.t, both EqClasses and all sub-EqClasses contained
 * within them are union-found with each other.
 * Consequently, if either foo or bar are extended with another MutableEqClass,
 * both will have access to the fully updated EqClass without need to dfs
 * (as will their children).
 *
 * NOTE: Preferred usage when not extending/unioning is to call MutableEqClass.snapshot_class
 * to get an immutable EqClass and perform computation on that instead to avoid bugs.
 */

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
