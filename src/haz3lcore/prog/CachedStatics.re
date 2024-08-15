open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: UExp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
};

let empty: t = {
  term: UExp.{ids: [Id.invalid], copied: false, term: Tuple([])},
  info_map: Id.Map.empty,
  error_ids: [],
};

let init = (~settings: CoreSettings.t, ~stitch, z: Zipper.t): t => {
  // Modify here to allow passing in an initial context
  let ctx_init = Builtins.ctx_init;
  let term = MakeTerm.from_zip_for_sem(z).term |> stitch;
  let info_map = Statics.mk(settings, ctx_init, term);
  let error_ids = Statics.Map.error_ids(info_map);
  {term, info_map, error_ids};
};

let init = (~settings: CoreSettings.t, ~stitch, z: Zipper.t) =>
  settings.statics ? init(~settings, ~stitch, z) : empty;
