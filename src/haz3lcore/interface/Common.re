// TODO(Matt): Once dependencies are resolved, this should ideally
// be combined with Settings.re, but for now, these types are here
// so they can be used in Projectors and Editors.

type global = {
  settings: Language.CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
};

type t = {
  settings: Language.CoreSettings.t,
  font_metrics: FontMetrics.t,
  secondary_icons: bool,
  show_backpack_targets: bool,
  color_highlights: option(ColorSteps.colorMap),
  statics: CachedStatics.t,
  dynamics: Language.Dynamics.Map.t,
};

let t_of_global = (~statics, ~dynamics=Id.Map.empty, global: global): t => {
  {
    settings: global.settings,
    font_metrics: global.font_metrics,
    secondary_icons: global.secondary_icons,
    show_backpack_targets: global.show_backpack_targets,
    color_highlights: global.color_highlights,
    statics,
    dynamics,
  };
};

let global_of_t = (t: t): global => {
  {
    settings: t.settings,
    font_metrics: t.font_metrics,
    secondary_icons: t.secondary_icons,
    show_backpack_targets: t.show_backpack_targets,
    color_highlights: t.color_highlights,
  };
};
