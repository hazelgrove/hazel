type common = {
  settings: CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  statics: CachedStatics.t,
  dynamics: Dynamics.Map.t,
};

type edit_mode =
  | ReadOnly
  | Editable;

type editor_utility('ed_m, 'ed_a) = {
  mk: Any.t => 'ed_m,
  make_term: (Sort.t, 'ed_m) => Any.t,
};

type projector_utility('p_kind, 'p_m) = {
  mk: ('p_kind, Any.t) => option('p_m),
  get_focusable: 'p_kind => ProjectorBase.Focusable.t,
  shape_of_projector: (common, Sort.t, 'p_m) => ProjectorShape.t,
  projector_to_term: (common, Sort.t, 'p_m) => Any.t,
  make_term: (Sort.t, 'p_m) => Any.t,
};
