type common = {
  settings: CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Dynamics.Map.t,
};

type edit_mode('p_k, 'p_m, 'p_a, 'e_f) =
  | ReadOnly
  | Editable({
      inject: Action.t('p_k, 'p_m, 'p_a) => Ui_effect.t(unit),
      make_active: 'e_f => Ui_effect.t(unit),
      has_focus: option('e_f),
    });
