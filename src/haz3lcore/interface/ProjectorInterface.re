type common = {
  settings: CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Dynamics.Map.t,
};

type edit_mode('p_k, 'p_m, 'e_f) =
  | ReadOnly
  | Editable({
      inject: Action.t('p_k, 'p_m) => Ui_effect.t(unit),
      make_active: 'e_f => Ui_effect.t(unit),
      has_focus: option('e_f),
    });

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
