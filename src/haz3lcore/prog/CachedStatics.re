open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  term: Exp.t,
  elaborated: Exp.t,
  info_map: Statics.Map.t,
  error_ids: list(Id.t),
};

let empty: t = {
  term: {
    annotation: {
      ids: [Id.invalid],
      copied: false,
    },
    term: Tuple([]),
  },
  elaborated: {
    annotation: {
      ids: [Id.invalid],
      copied: false,
    },
    term: Tuple([]),
  },
  info_map: Id.Map.empty,
  error_ids: [],
};

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

let init_from_term = (~settings: CoreSettings.t, term): t => {
  let ctx_init = Builtins.ctx_init;
  if (!settings.statics) {
    let elaborated = dh_err("Statics disabled");
    {term, elaborated, info_map: Id.Map.empty, error_ids: []};
  } else {
    let info_map = Statics.mk(ctx_init, term);
    let error_ids = Statics.error_ids(info_map);
    if (!settings.dynamics && !settings.elaborate) {
      let elaborated = dh_err("Dynamics & Elaboration disabled");

      {term, elaborated, info_map, error_ids};
    } else {
      let elaborated =
        switch (elaborate(info_map, term)) {
        | DoesNotElaborate => dh_err("Elaboration returns None")
        | Elaborates(d, _, _) => d
        };
      {term, elaborated, info_map, error_ids};
    };
  };
};

let init = (~settings: CoreSettings.t, ~stitch, z: Zipper.t): t => {
  let term = MakeTerm.from_zip_for_sem(z).term |> stitch;
  init_from_term(~settings, term);
};

let init = (~settings: CoreSettings.t, ~stitch, z: Zipper.t) =>
  settings.statics ? init(~settings, ~stitch, z) : empty;
