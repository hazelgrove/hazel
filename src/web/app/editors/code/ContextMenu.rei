/* The editor's right-click menu: what it offers for the term at the caret
   (probes, statics, go-to, introduce, select, projectors) and where it opens.
   The menu's state and keyboard behavior are Util.Menu's. */

module Model = Util.Menu;

type command =
  | Perform(Haz3lcore.Action.t)
  | PasteFromClipboard
  /* a link to what is under the cursor (the livelits branch, #2635) */
  | CopyDeepLink;

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
  /** `None` for keys the menu doesn't handle, so the editor sees them. */
  let handle_listener_key:
    (
      ~linkable: bool,
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.TermBase.exp_t,
      ~syntax: Haz3lcore.CachedSyntax.t,
      ~dynamics: Language.Sample.Map.t,
      ~zipper: Haz3lcore.ZipperBase.t,
      ~dispatch_menu: Model.action => Ui_effect.t(unit),
      ~dispatch_action: command => Ui_effect.t(unit),
      Util.Menu.t,
      string
    ) =>
    option(Ui_effect.t(unit));
};

let view:
  (
    ~linkable: bool,
    ~inject: command => Ui_effect.t(unit),
    ~inject_menu: Model.action => Ui_effect.t(unit),
    ~syntax: Haz3lcore.CachedSyntax.t,
    ~info_map: Language.Statics.Map.t,
    ~elaborated: Language.TermBase.exp_t,
    ~dynamics: Language.Sample.Map.t,
    ~font_metrics: FontMetrics.t,
    ~model: Util.Menu.t,
    Haz3lcore.ZipperBase.t
  ) =>
  Virtual_dom.Vdom.Node.t;

/* Added on the livelits branch (#2635), before this interface existed. */
let direction_class: Model.open_direction => string;
module SpliceHost: {
  let find_host:
    (Uuidm.t, Haz3lcore.Base.segment) => option(Haz3lcore.Base.projector);
  let data:
    (
      ~syntax: Haz3lcore.CachedSyntax.t,
      ~info_map: Language.Statics.Map.t,
      ~dynamics: Language.Sample.Map.t,
      ~elaborated: Language.TermBase.exp_t,
      Haz3lcore.ZipperBase.t
    ) =>
    list(Model.item(command));
};
