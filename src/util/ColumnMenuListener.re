/* Document-level listeners for the table column menu. See MenuListener
 * for the shared machinery.
 *
 * Keys are handled at the document level (rather than via
 * tabindex+on_keydown on the menu div) because Hazel's editor (#page)
 * aggressively reclaims focus to the clipboard shim, which would
 * otherwise eat the menu's key events. */

include MenuListener.Make({
  let menu_class = "context-menu";
  let supports_keys = true;
  let scroll_into_view = true;
});
