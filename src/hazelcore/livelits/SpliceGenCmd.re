type esi = SpliceInfo.t(UHExp.t);
type out('a) = ('a, esi, MetaVarGen.t);
[@deriving sexp]
type t('a) = (esi, MetaVarGen.t) => out('a);
let return = (x, psi, u_gen) => (x, psi, u_gen);
let new_splice =
    (
      ~init_uhexp_gen=u_gen => {
                        let (u, u_gen) = MetaVarGen.next_hole(u_gen);
                        (UHExp.(Block.wrap(EmptyHole(u))), u_gen);
                      },
      htyp,
      {next, splice_map, splice_order}: esi,
      u_gen,
    )
    : out(SpliceName.t) => {
  let (init_uhexp, u_gen) = init_uhexp_gen(u_gen);
  (
    next,
    {
      next: next + 1,
      splice_map:
        NatMap.extend_unique(splice_map, (next, (htyp, init_uhexp))),
      splice_order: splice_order @ [next],
    },
    u_gen,
  );
};

let map_splice =
    (to_map, f, {splice_map, _} as psi: esi, u_gen): out((HTyp.t, UHExp.t)) => {
  let old_ =
    NatMap.lookup(splice_map, to_map)
    |> OptUtil.get(() =>
         failwith(
           Printf.sprintf("Cannot map non-existant splice %d", to_map),
         )
       );
  let (new_, u_gen) = f(old_, u_gen);
  let new_splice_map = NatMap.insert_or_update(splice_map, (to_map, new_));
  (old_, {...psi, splice_map: new_splice_map}, u_gen);
};

let drop_splice =
    (to_drop, {splice_map, splice_order, _} as psi: esi, u_gen)
    : out((HTyp.t, UHExp.t)) => {
  let (new_splice_map, dropped_exp) =
    OptUtil.get(
      () =>
        failwith(
          Printf.sprintf("Cannot delete non-existant splice %d", to_drop),
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
