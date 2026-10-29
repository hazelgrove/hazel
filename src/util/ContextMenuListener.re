/* Document-level listeners for the editor context menu. See MenuListener
 * for the shared machinery.
 *
 * Keys are dispatched at the document level (rather than via
 * tabindex+on_keydown on the menu div) so the menu never has to take
 * focus from the editor.
 *
 * Synced by the menu's own hook (ContextMenu.Layer), not by every editor.
 * The menu is placed in the viewport, in the top layer, so scrolling the
 * page would only move the code out from under it. */

include MenuListener.Make({
  let menu_class = "context-menu";
  let supports_keys = true;
  let scroll_into_view = false;
  let close_on_scroll = false;
});
