open Util;
open Language;

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
    },
    term: Tuple([]),
  },
  elaborated: {
    annotation: {
      ids: [Id.invalid],
    },
    term: Tuple([]),
  },
  info_map: Id.Map.empty,
  error_ids: [],
};

let elaborate =
  Core.Memo.general(~cache_size_bound=1000, Elaborator.uexp_elab);

let dh_err = (error: string): DHExp.t => Var(error) |> DHExp.fresh;

let init_from_term =
    (~settings: CoreSettings.t, ~is_dynamic_term, ~ctx=?, ~ana, term): t => {
  let ctx_init =
    Option.value(
      ~default=Builtins.ctx_init(is_dynamic_term ? None : Some(Int)),
      ctx,
    );
  let info_map =
    Statics.uexp_to_info_map(
      ~ctx=ctx_init,
      ~ana,
      ~ancestors=[],
      ~duplicates=[],
      ~expected_labels=None,
      ~label_sort=false,
      term,
      Id.Map.empty,
    )
    |> snd;
  let error_ids = Statics.Map.error_ids(info_map);
  let elaborated =
    switch () {
    | _ when !settings.statics => dh_err("Statics disabled")
    | _ when !settings.dynamics && !settings.elaborate =>
      dh_err("Dynamics & Elaboration disabled")
    | _ =>
      switch (elaborate(info_map, term)) {
      | DoesNotElaborate => dh_err("Elaboration returns None")
      | Elaborates(d, _) => d
      }
    };
  {
    term,
    elaborated,
    info_map,
    error_ids,
  };
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~ana,
      z: Zipper.t,
    )
    : t => {
  let term = MakeTerm.from_zip_for_sem(z).term |> stitch;
  init_from_term(~settings, ~ctx?, ~is_dynamic_term, ~ana, term);
};

let init =
    (
      ~settings: CoreSettings.t,
      ~is_dynamic_term,
      ~stitch,
      ~ctx=?,
      ~ana,
      z: Zipper.t,
    ) =>
  settings.statics
    ? init(~settings, ~stitch, ~ctx?, ~is_dynamic_term, ~ana, z) : empty;
