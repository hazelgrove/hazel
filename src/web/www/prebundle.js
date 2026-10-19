
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// This is the default behavior for the hotkeys module but I'm overriding it for the
// clipboard-shim and the ninja-keys command palette (which lives inside a shadow DOM).
hotkeys.filter = event => {
  // composedPath() lets us see the original target even when the event has been
  // retargeted across a shadow DOM boundary (e.g. the <input> inside ninja-keys).
  const path = typeof event.composedPath === 'function' ? event.composedPath() : [];
  const target = event.target || event.srcElement;
  const { tagName, id } = target;

  // Override happening here
  if(id == "clipboard-shim") {
    return true;
  }

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

// ninja-keys binds a hotkeys-js handler for every hotkeyed action each time
// `data` is set and never releases it, so a palette refreshed on every render
// fired each hotkey once per render (hazelgrove/hazel#2586). This replaces its
// `update` with one that releases the handlers it bound the previous time.
const boundHotkeys = new WeakMap();
NinjaKeys.prototype.update = function (changedProperties) {
  if (changedProperties.has('data')) {
    this._flatData = this._flattern(this.data);
    if (!this.disableHotkeys) {
      (boundHotkeys.get(this) || []).forEach(({ hotkey, method }) =>
        hotkeys.unbind(hotkey, method)
      );
      const bound = this._flatData
        .filter((action) => !!action.hotkey)
        .map((action) => {
          const method = (event) => {
            event.preventDefault();
            if (action.handler) {
              action.handler(action);
            }
          };
          hotkeys(action.hotkey, method);
          return { hotkey: action.hotkey, method };
        });
      boundHotkeys.set(this, bound);
    }
  }
  Object.getPrototypeOf(NinjaKeys.prototype).update.call(this, changedProperties);
};
