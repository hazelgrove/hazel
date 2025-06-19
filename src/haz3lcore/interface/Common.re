type t = {
  settings: Language.CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Language.Dynamics.Map.t,
};
