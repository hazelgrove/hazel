/* The editor context menu, driven entirely by CodeEditable. `WithContext` is
   the update/key-handling pair; menu construction is private. */

module Model = Util.Menu;

module WithContext: {
  let update:
    (
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.TermBase.exp_t,
      ~zipper: Haz3lcore.ZipperBase.t,
      Model.action,
      Util.Menu.t
    ) =>
    Util.Menu.t;
  let handle_listener_key:
    (
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.TermBase.exp_t,
      ~zipper: Haz3lcore.ZipperBase.t,
      ~dispatch_menu: Model.action => Ui_effect.t(unit),
      ~dispatch_action: Haz3lcore.Action.t => Ui_effect.t(unit),
      Util.Menu.t,
      string
    ) =>
    option(Ui_effect.t(unit));
};

let view:
  (
    ~inject: Haz3lcore.Action.t => Ui_effect.t(unit),
    ~inject_menu: Model.action => Ui_effect.t(unit),
    ~syntax: Haz3lcore.CachedSyntax.t,
    ~info_map: Language.Statics.Map.t,
    ~elaborated: Language.TermBase.exp_t,
    ~font_metrics: FontMetrics.t,
    ~model: Util.Menu.t,
    Haz3lcore.ZipperBase.t
  ) =>
  Virtual_dom.Vdom.Node.t;
