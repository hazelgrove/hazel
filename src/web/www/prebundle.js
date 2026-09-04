
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

// ---------------------------------------------------------------------------
// Fumola livelit runtime.
//
// This is the external store the Fumola livelit's model names:
//
//     sigma : FumolaInstanceId -> FumolaRuntimeState
//
// The store itself lives inside the Fumola wasm module (see
// crates/fumola_wasm in the Fumola repo); what lives here is the glue that
// loads that module and presents a *synchronous* interface to Hazel, since
// livelit expansion and rendering are both synchronous.
//
// Instantiating wasm is unavoidably asynchronous, so calls made before the
// module has loaded answer "Pending" rather than blocking. Once loaded, every
// call is synchronous.
//
// The wasm artifacts are generated, not checked in. Build them with
// scripts/build-fumola-wasm.sh. Without them the livelit degrades to a clear
// message instead of breaking the Hazel build.
window.fumola = (() => {
  let wasm = null;
  let loadError = null;

  // instance_id -> owner token (a projector's persistent Hazel id)
  const owners = new Map();
  // instance_id -> {src, result}: the last program evaluated in that runtime.
  // Re-running is skipped when the text has not changed, which is exactly the
  // invariant that sigma(i) is synchronized with the model's program text.
  const lastEval = new Map();

  // Hidden from the bundler so that Hazel builds without the generated files.
  const dynamicImport = new Function("p", "return import(p)");
  dynamicImport("./fumola/fumola_wasm.js")
    .then((mod) => mod.default("./fumola/fumola_wasm_bg.wasm").then(() => {
      wasm = mod;
    }))
    .catch((e) => {
      loadError = String(e);
      console.warn("Fumola livelit: wasm runtime unavailable:", e);
    });

  const ready = () => wasm !== null;

  // Decide which runtime this projector should use, given the id currently in
  // its model and its own persistent identity.
  const claim = (id, owner) => {
    if (!ready()) return id;
    if (id === 0) {
      // Never claimed a runtime before.
      const fresh = wasm.fumola_create();
      owners.set(fresh, owner);
      return fresh;
    }
    const existing = owners.get(id);
    if (existing === undefined) {
      // Reload: a saved program names an id this session has never seen.
      wasm.fumola_realize(id);
      owners.set(id, owner);
      return id;
    }
    if (existing === owner) return id;
    // Duplication: another live projector already owns this runtime, so this
    // one is a copy. Ids are generative -- give the copy its own runtime
    // rather than letting two livelits share one execution history.
    const fresh = wasm.fumola_create();
    owners.set(fresh, owner);
    return fresh;
  };

  // Fumola syntax errors carry the parser's full expected-token set, which
  // runs to well over a thousand characters. That is useless in a livelit the
  // width of an expression, so keep only the head of the message.
  const summarize = (message) => {
    const text = String(message).replace(/\s+/g, " ").trim();
    const cut = text.indexOf(", expected:");
    const head = cut === -1 ? text : text.slice(0, cut) + " }";
    return head.length > 160 ? head.slice(0, 157) + "..." : head;
  };

  const evalSync = (id, src) => {
    if (!ready()) {
      return loadError === null
        ? "Pending:Fumola runtime is still loading"
        : "Err:Fumola runtime unavailable (run scripts/build-fumola-wasm.sh)";
    }
    const last = lastEval.get(id);
    if (last !== undefined && last.src === src) return last.result;

    if (!wasm.fumola_has(id)) wasm.fumola_realize(id);

    let result;
    try {
      const parsed = JSON.parse(wasm.fumola_eval(id, src));
      if (parsed.ok && parsed.tag === "Int") {
        result = "Int:" + parsed.value;
      } else if (parsed.ok) {
        result = "Err:Fumola returned a " + parsed.tag +
          ", which this livelit cannot yet translate";
      } else {
        result = "Err:" + summarize(parsed.error);
      }
    } catch (e) {
      result = "Err:" + String(e);
    }
    lastEval.set(id, { src, result });
    return result;
  };

  return { ready, claim, evalSync };
})();
