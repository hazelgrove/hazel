/* Document-level listeners for the editor context menu. See MenuListener
 * for the shared machinery.
 *
 * Keys are dispatched at the document level (rather than via
 * tabindex+on_keydown on the menu div) so the menu never has to take
 * focus from the editor. */

include MenuListener.Make({
  let menu_class = "context-menu";
  let supports_keys = true;
  let scroll_into_view = true;
  let close_on_scroll = false;
});
