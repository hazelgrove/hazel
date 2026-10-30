/* The editor's right-click menu: what it offers for the term at the caret
   (probes, statics, go-to, introduce, select, projectors) and where it opens.
   The menu's state and keyboard behavior are Util.Menu's. */

module Model = Util.Menu;

type command =
  | Perform(Haz3lcore.Action.t)
  | PasteFromClipboard
  | ExpandSelection
  | ShrinkSelection;

/** The "Put down" row, offered exactly when the backpack can be dropped. */
let put_down_data: Haz3lcore.ZipperBase.t => list(Util.Menu.item(command));

/** The selection rows: select the term at the caret, or grow the selection,
    and shrink it back when it was grown. */
let selection_data:
  (~can_shrink: bool, Haz3lcore.ZipperBase.t) =>
  list(Util.Menu.item(command));

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
      ~info_map: Language.Statics.Map.t,
      ~elaborated: Language.TermBase.exp_t,
      ~zipper: Haz3lcore.ZipperBase.t,
      ~can_shrink: bool,
      ~dispatch_menu: Model.action => Ui_effect.t(unit),
      ~dispatch_action: command => Ui_effect.t(unit),
      Util.Menu.t,
      string
    ) =>
    option(Ui_effect.t(unit));
};

let view:
  (
    ~inject: command => Ui_effect.t(unit),
    ~inject_menu: Model.action => Ui_effect.t(unit),
    ~syntax: Haz3lcore.CachedSyntax.t,
    ~info_map: Language.Statics.Map.t,
    ~elaborated: Language.TermBase.exp_t,
    ~font_metrics: FontMetrics.t,
    ~model: Util.Menu.t,
    ~can_shrink: bool,
    Haz3lcore.ZipperBase.t
  ) =>
  Virtual_dom.Vdom.Node.t;
