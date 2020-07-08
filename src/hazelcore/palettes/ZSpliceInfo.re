open Sexplib.Std;

[@deriving sexp]
type zsplice_map('exp, 'zexp) = ZIntMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));
[@deriving sexp]
type t('exp, 'zexp) = {
  next: SpliceInfo.splice_name,
  zsplice_map: zsplice_map('exp, 'zexp),
  splice_order: list(SpliceInfo.splice_name),
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
