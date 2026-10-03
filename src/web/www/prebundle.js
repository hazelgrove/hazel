
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// ninja-keys leaves focus nowhere when it closes: hand it back to whatever
// had it when the palette opened, unless the chosen action moved it.
const ninjaOpen = NinjaKeys.prototype.open;
const ninjaClose = NinjaKeys.prototype.close;
NinjaKeys.prototype.open = function (...args) {
  if (!this.visible) this._returnFocus = document.activeElement;
  return ninjaOpen.apply(this, args);
};
NinjaKeys.prototype.close = function (...args) {
  const prev = this._returnFocus;
  this._returnFocus = null;
  const result = ninjaClose.apply(this, args);
  if (prev && prev.isConnected && document.activeElement === this)
    prev.focus({ preventScroll: true });
  return result;
};

// This is the default behavior for the hotkeys module but I'm overriding it for the
// ninja-keys command palette (which lives inside a shadow DOM).
hotkeys.filter = event => {
  // composedPath() lets us see the original target even when the event has been
  // retargeted across a shadow DOM boundary (e.g. the <input> inside ninja-keys).
  const path = typeof event.composedPath === 'function' ? event.composedPath() : [];
  const target = event.target || event.srcElement;
  const { tagName } = target;

  // When the event originates inside the ninja-keys command palette, only let
  // its own navigation/close keys through. This stops globally-registered action
  // hotkeys (e.g. Cmd+A for "Select All") from firing while the user is typing
  // in the palette's search box, while still letting Esc close the palette and
  // the arrow/enter keys navigate it.
  const inNinjaKeys = path.some(el => el && el.tagName === 'NINJA-KEYS');
  if (inNinjaKeys) {
    return ['Escape', 'Enter', 'ArrowUp', 'ArrowDown', 'Backspace', 'Tab'].includes(event.key);
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
