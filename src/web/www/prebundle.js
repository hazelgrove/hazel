
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// hotkeys-js only runs the command palette's own keys. ninja-keys registers
// every action's hotkey with it too, and never unregisters one, but Hazel
// matches those chords itself before any editor sees them
// (ContextualAction.of_key, via Page.View.shortcut_listener), so letting
// hotkeys-js fire them as well would run stale or rebound actions.
hotkeys.filter = event => {
  // composedPath() lets us see the original target even when the event has been
  // retargeted across a shadow DOM boundary (e.g. the <input> inside ninja-keys).
  const path = typeof event.composedPath === 'function' ? event.composedPath() : [];
  const target = event.target || event.srcElement;
  const { tagName, id } = target;

  // Inside the palette: its navigation and close keys.
  const inNinjaKeys = path.some(el => el && el.tagName === 'NINJA-KEYS');
  if (inNinjaKeys) {
    return ['Escape', 'Enter', 'ArrowUp', 'ArrowDown', 'Backspace', 'Tab'].includes(event.key);
  }

  // Outside it: only the key that opens it (ninja-keys' openHotkey), and not
  // while typing in a field. The clipboard shim is a textarea only so that
  // copy and paste work, so it does not count as one.
  const opensPalette = (event.metaKey || event.ctrlKey) && event.key.toLowerCase() === 'k';
  const isInput = tagName === 'INPUT' && !['checkbox', 'radio', 'range', 'button', 'file', 'reset', 'submit', 'color'].includes(target.type);
  const inField =
    id !== 'clipboard-shim'
    && (target.isContentEditable
        || ((isInput || tagName === 'TEXTAREA' || tagName === 'SELECT') && !target.readOnly));
  return opensPalette && !inField;
};
