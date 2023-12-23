/**
 * A mutable version of the PotentialTypeSet.t type that allows extension via UnionFind
 * such that if one foo: MutablePotentialTypeSet.t is extended (or unioned) with
 * bar: MutablePotentialTypeSet.t, both PotentialTypeSetes and all sub-PotentialTypeSetes contained
 * within them are union-found with each other.
 * Consequently, if either foo or bar are extended with another MutablePotentialTypeSet,
 * both will have access to the fully updated PotentialTypeSet without need to dfs
 * (as will their children).
 *
 * NOTE: Preferred usage when not extending/unioning is to call MutablePotentialTypeSet.snapshot_class
 * to get an immutable PotentialTypeSet and perform computation on that instead to avoid bugs.
 */

type error_status =
  | Occurs;

type t = UnionFind.elem((mut_pot_typs, option(error_status)))
and mut_pot_typs = list(mut_pot_typ)
and mut_pot_typ =
  | Base(PotentialTypeSet.base_typ)
  | Unary(PotentialTypeSet.unary_ctor, t)
  | Binary(PotentialTypeSet.binary_ctor, t, t);

let snapshot_class:
  (t, ITyp.t) => (PotentialTypeSet.t, option(error_status));

let pot_typ_set_to_mut_pot_typ_set: PotentialTypeSet.t => t;
let pot_typ_to_mut_pot_typ: PotentialTypeSet.potential_typ => mut_pot_typ;

let derive_nested_keys_and_potential_typ_sets:
  ITyp.t => (list(ITyp.t), list(t));

let union: (t, t) => unit;

let mark_failed_occurs: t => unit;
