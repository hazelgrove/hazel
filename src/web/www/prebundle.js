
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// This is the default behavior for the hotkeys module but I'm overriding it for the clipboard-shim,
// and to look through shadow DOM boundaries so that typing in shadow-DOM inputs (e.g. the ninja-keys
// command palette search) doesn't trigger global hotkeys like Cmd+A.
hotkeys.filter = event => {
  // For events that cross a shadow DOM boundary (e.g. the <input> inside the ninja-keys
  // LitElement), event.target is retargeted to the shadow host. composedPath()[0] gives
  // the actual original target so we can detect inputs inside shadow roots.
  const path = typeof event.composedPath === 'function' ? event.composedPath() : [];
  const target = path[0] || event.target || event.srcElement;
  const { tagName, id } = target;

  // Override happening here
  if(id == "clipboard-shim") {
    return true;
  }

  let flag = true;
  const isInput = tagName === 'INPUT' && !['checkbox', 'radio', 'range', 'button', 'file', 'reset', 'submit', 'color'].includes(target.type);
  // ignore: isContentEditable === 'true', <input> and <textarea> when readOnly state is false, <select>
  if (
    target.isContentEditable
    || ((isInput || tagName === 'TEXTAREA' || tagName === 'SELECT') && !target.readOnly)
  ) {
    flag = false;
  }
  return flag;
  };
