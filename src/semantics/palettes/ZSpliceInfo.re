open SemanticsCommon;
type zsplice_map('exp, 'zexp) = Util.ZNatMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));
type t('exp, 'zexp) = {next: SpliceInfo.splice_name, zsplice_map: zsplice_map('exp, 'zexp)};
let erase = (zpsi : t('exp, 'zexp), erase_z): SpliceInfo.t('exp) => 
  SpliceInfo.({ next: zpsi.next, splice_map: Util.ZNatMap.erase(zpsi.zsplice_map, erase_z) }); 
let select_opt = 
  (splice_info : SpliceInfo.t('exp), 
   n : nat, 
   f : ((HTyp.t, 'exp)) => option((HTyp.t, 'zexp))) 
  : option(t('exp, 'zexp)) => {
  let {SpliceInfo.next, SpliceInfo.splice_map} = splice_info;
  switch (Util.NatMap.drop(splice_map, n)) {
  | None => None
  | Some((splice_map, ty_e)) => 
    switch (f(ty_e)) {
    | None => None
    | Some(ty_ze) => 
      let zsplice_map = (splice_map, (n, ty_ze));
      Some({next, zsplice_map})
    }
  }
};
