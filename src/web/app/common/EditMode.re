open Virtual_dom.Vdom;

/* Bundles focus and injection state for an editor view.
 *
 * ReadOnly: the editor is displayed but does not accept input.
 * Editable: the editor can receive focus and handle keyboard events.
 *
 * - inject: dispatch editor-specific actions
 * - escape: called when arrow keys hit an editor boundary,
 *   allowing the parent to navigate to the next cell/projector.
 *   NOTE: direction means "which side to escape TO", not "which key
 *   was pressed". Left arrow at start → escape(Left).
 * - take_focus: accept focus from a parent (e.g. keyboard handoff
 *   from an adjacent projector)
 * - focus: current focus state, None if this editor is not active */
type t('action, 'focus) =
  | ReadOnly
  | Editable({
      inject: 'action => Effect.t(unit),
      escape: Util_web.Direction.t => Effect.t(unit),
      take_focus: 'focus => Effect.t(unit),
      focus: option('focus),
    });

let is_active =
  fun
  | ReadOnly => false
  | Editable({focus: Some(_), _}) => true
  | Editable({focus: None, _}) => false;
