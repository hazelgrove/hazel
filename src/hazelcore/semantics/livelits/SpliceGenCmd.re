type esi = SpliceInfo.t(UHExp.t);
type out('a) = ('a, esi, MetaVarGen.t);
[@deriving sexp]
type t('a) = (esi, MetaVarGen.t) => out('a);
let return = (x, psi, u_gen) => (x, psi, u_gen);
let new_splice =
    ({next, splice_map, splice_order}: esi, u_gen)
    : out(SpliceInfo.splice_name) => {
  let (u, u_gen) = MetaVarGen.next_hole(u_gen);
  (
    next,
    {
      next: next + 1,
      splice_map:
        NatMap.extend_unique(
          splice_map,
          (next, (HTyp.Hole, UHExp.(Block.wrap(EmptyHole(u))))),
        ),
      splice_order: splice_order @ [next],
    },
    u_gen,
  );
};
let drop_splice =
    (to_drop, {splice_map, splice_order, _} as psi: esi, u_gen)
    : out((HTyp.t, UHExp.t)) => {
  let (new_splice_map, dropped_exp) =
    OptUtil.get(
      () =>
        failwith(
          "Cannot delete non-existant splice named "
          ++ SpliceInfo.var_of_splice_name(to_drop),
        ),
      NatMap.drop(splice_map, to_drop),
    );
  (
    dropped_exp,
    {
      ...psi,
      splice_map: new_splice_map,
      splice_order: ListUtil.drop_first(to_drop, splice_order),
    },
    u_gen,
  );
};
let bind = (cmd, f, psi, u_gen) => {
  let (a, psi, u_gen) = cmd(psi, u_gen);
  f(a, psi, u_gen);
};

let exec = (cmd, psi, u_gen) => {
  cmd(psi, u_gen);
};

/* Have to add these functions to stub ppx_deriving.show for types that use this type */
let pp = (_, _, _) => ();
let show = _ => "SpliceGenCmd";
