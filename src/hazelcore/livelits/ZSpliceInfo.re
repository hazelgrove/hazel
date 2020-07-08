open Sexplib.Std;

[@deriving sexp]
type zsplice_map('exp, 'zexp) = ZIntMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));
[@deriving sexp]
type t('exp, 'zexp) = {
  next: SpliceName.t,
  zsplice_map: zsplice_map('exp, 'zexp),
  splice_order: list(SpliceName.t),
};
let erase = (zpsi: t('exp, 'zexp), erase_z): SpliceInfo.t('exp) =>
  SpliceInfo.{
    next: zpsi.next,
    splice_map: ZIntMap.erase(zpsi.zsplice_map, erase_z),
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
  switch (IntMap.find_opt(n, splice_map)) {
  | None => None
  | Some(ty_e) =>
    switch (f(ty_e)) {
    | None => None
    | Some(ty_ze) =>
      let zsplice_map = (IntMap.remove(n, splice_map), (n, ty_ze));
      Some({next, zsplice_map, splice_order});
    }
  };
};

let prj_ze = (zsi: t('exp, 'zexp)): 'zexp => {
  let (_, ze) = ZIntMap.prj_z_v(zsi.zsplice_map);
  ze;
};

let prj_z = (zsi: t('exp, 'zexp)): (HTyp.t, 'zexp) =>
  ZIntMap.prj_z_v(zsi.zsplice_map);

let update_z = (zsi: t('exp, 'zexp), z: (HTyp.t, 'zexp)): t('exp, 'zexp) => {
  ...zsi,
  zsplice_map: ZIntMap.update_z(zsi.zsplice_map, z),
};
