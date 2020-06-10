open Sexplib.Std;

[@deriving sexp]
type zsplice_map('exp, 'zexp) = ZNatMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));
[@deriving sexp]
type t('exp, 'zexp) = {
  next: SpliceInfo.splice_name,
  zsplice_map: zsplice_map('exp, 'zexp),
  splice_order: list(SpliceInfo.splice_name),
};
let erase = (zpsi: t('exp, 'zexp), erase_z): SpliceInfo.t('exp) =>
  SpliceInfo.{
    next: zpsi.next,
    splice_map: ZNatMap.erase(zpsi.zsplice_map, erase_z),
    splice_order: zpsi.splice_order,
  };
let select_opt =
    (
      splice_info: SpliceInfo.t('exp),
      n: int,
      f: ((HTyp.t, 'exp)) => option((HTyp.t, 'zexp)),
    )
    : option(t('exp, 'zexp)) => {
  let {SpliceInfo.next, SpliceInfo.splice_map, SpliceInfo.splice_order} = splice_info;
  switch (NatMap.drop(splice_map, n)) {
  | None => None
  | Some((splice_map, ty_e)) =>
    switch (f(ty_e)) {
    | None => None
    | Some(ty_ze) =>
      let zsplice_map = (splice_map, (n, ty_ze));
      Some({next, zsplice_map, splice_order});
    }
  };
};
