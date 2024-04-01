import {NinjaKeys} from 'https://unpkg.com/ninja-keys?module';
import hotkeys from "https://unpkg.com/hotkeys-js@3.8.7?module"
hotkeys.filter = event => {
  const target = event.target || event.srcElement;
  const { tagName, id } = target;
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
