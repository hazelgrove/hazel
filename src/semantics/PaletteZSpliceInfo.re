open SemanticsCommon;
type zsplice_map('exp, 'zexp) = Util.ZNatMap.t((HTyp.t, 'exp), (HTyp.t, 'zexp));
type t('exp, 'zexp) = {next: PaletteSpliceInfo.splice_name, zsplice_map: zsplice_map('exp, 'zexp)};
let erase = (zpsi : t('exp, 'zexp), erase_z): PaletteSpliceInfo.t('exp) => 
  PaletteSpliceInfo.({ next: zpsi.next, splice_map: Util.ZNatMap.erase(zpsi.zsplice_map, erase_z) }); 
