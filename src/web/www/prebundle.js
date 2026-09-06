
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
  // The name of the source that answered, once something has loaded. Worth
  // being able to ask: the sources are indistinguishable in behaviour, so
  // without this there is no way to tell which one a page is running.
  let loadedFrom = null;

  // instance_id -> owner token (a projector's Hazel id)
  const owners = new Map();
  // owner token -> the instance it was given, so that repeating a claim is
  // idempotent rather than allocating a fresh runtime every render.
  const claimedByOwner = new Map();
  // instance_id -> {src, result}: the last program evaluated in that runtime.
  // Re-running is skipped when the text has not changed, which is exactly the
  // invariant that sigma(i) is synchronized with the model's program text.
  const lastEval = new Map();

  // Where the runtime is fetched from. Tried in order; the first that loads
  // wins, and the rest are never asked.
  //
  // The local pair is what scripts/build-fumola-wasm.sh writes, and is used
  // when working on Fumola and Hazel together. Otherwise the canonical build
  // is used: it is published to GitHub Pages from the Fumola repo, which
  // serves it with CORS and the correct application/wasm content type.
  // (A GitHub release asset cannot be used -- release downloads carry no
  // Access-Control-Allow-Origin header, so a browser cannot fetch one.)
  //
  // That published build is listed twice, under the two names the same files
  // answer to. fumola.org is where the Fumola Pages site is going;
  // adapton.github.io is where it is served from until that domain is
  // claimed. Both are here because the switch cannot be made atomically from
  // this side: giving a Pages site a custom domain makes its old URL 301 to
  // the new one, and a redirect response carries no
  // Access-Control-Allow-Origin -- which a cross-origin import must have on
  // every hop of the chain, not merely on the last. So each URL works on
  // exactly one side of the switch, and listing both spans it from either
  // side. Once the domain has settled, the adapton.github.io entry only ever
  // redirects, and can be dropped.
  //
  // The glue and the .wasm are always taken from the same place. They are
  // generated together by wasm-bindgen and will not load if their versions
  // disagree.
  const here = (path) => new URL(path, document.baseURI).href;
  const SOURCES = [
    {
      name: "local",
      glue: here("./fumola/fumola_wasm.js"),
      wasm: here("./fumola/fumola_wasm_bg.wasm"),
    },
    {
      name: "fumola.org",
      glue: "https://fumola.org/fumola_wasm.js",
      wasm: "https://fumola.org/fumola_wasm_bg.wasm",
    },
    {
      name: "adapton.github.io",
      glue: "https://adapton.github.io/fumola/fumola_wasm.js",
      wasm: "https://adapton.github.io/fumola/fumola_wasm_bg.wasm",
    },
  ];

  // Hidden from the bundler so that Hazel builds without the generated files.
  const dynamicImport = new Function("p", "return import(p)");
  const load = async (from) => {
    const mod = await dynamicImport(from.glue);
    await mod.default({ module_or_path: from.wasm });
    return mod;
  };
  (async () => {
    // Sequential on purpose: a later source is a fallback, not a race. Loading
    // two runtimes and discarding one would instantiate wasm twice.
    const failures = [];
    for (const from of SOURCES) {
      try {
        wasm = await load(from);
        loadedFrom = from.name;
        console.info("Fumola livelit: runtime loaded from " + from.name);
        return;
      } catch (e) {
        failures.push(from.name + " (" + e + ")");
      }
    }
    loadError = "tried " + failures.join("; ");
    console.warn("Fumola livelit: wasm runtime unavailable: " + loadError);
  })();

  const ready = () => wasm !== null;

  // Give a livelit that has never named a runtime one of its own.
  //
  // Called only for id 0. Reclaiming an already-named livelit is what would
  // let a duplicated one be given a fresh runtime, but doing that from the
  // view makes rendering rewrite its own syntax, which can loop. So a copy
  // currently inherits its original's id and therefore shares its execution
  // history -- a known gap, waiting on a projector identity that is stable
  // across model edits.
  const claim = (id, owner) => {
    if (!ready() || id !== 0) return id;
    // A claim only takes effect once the model has been rewritten to name the
    // new runtime. Until that lands the livelit still reads as id 0 and will
    // claim again on the next render, so answer with the runtime this owner
    // was already given rather than allocating another one each time.
    const already = claimedByOwner.get(owner);
    if (already !== undefined) return already;
    const fresh = wasm.fumola_create();
    owners.set(fresh, owner);
    claimedByOwner.set(owner, fresh);
    return fresh;
  };

  // Evaluate at the top level of the runtime, with no thunk around the
  // program. Uncached, because it is used for programs whose point is their
  // effect on the runtime rather than their value.
  const evalTop = (id, src) => {
    if (!ready()) {
      return JSON.stringify({
        ok: false,
        kind: "runtime",
        error: "the Fumola runtime is not loaded",
      });
    }
    if (!wasm.fumola_has(id)) wasm.fumola_realize(id);
    try {
      return wasm.fumola_eval_top(id, src);
    } catch (e) {
      return JSON.stringify({ ok: false, kind: "runtime", error: String(e) });
    }
  };

  const evalSync = (id, thunkName, src) => {
    if (!ready()) {
      return JSON.stringify({
        ok: false,
        kind: "runtime",
        error:
          loadError === null
            ? "the Fumola runtime is still loading"
            : "the Fumola runtime is unavailable",
      });
    }
    // Keyed by thunk as well as program: two thunk livelits share a runtime
    // but not a thunk, so one's result must not answer for the other.
    const key = thunkName + "\u0000" + src;
    const last = lastEval.get(id);
    if (last !== undefined && last.key === key) return last.result;

    if (!wasm.fumola_has(id)) wasm.fumola_realize(id);

    // The raw JSON from the runtime, passed through verbatim. Flattening it
    // to a tagged string here would not survive structure: a Fumola tuple or
    // record has to reach Hazel as a tree, so that it can be rebuilt as a
    // Hazel tuple or record rather than as something Hazel must take apart.
    let result;
    try {
      result = wasm.fumola_eval(id, thunkName, src);
    } catch (e) {
      result = JSON.stringify({ ok: false, error: String(e) });
    }
    lastEval.set(id, { key, result });
    return result;
  };

  const source = () => loadedFrom;

  /* Run a program without touching the cache, in either direction.
     
     Used for dereferencing pointers while translating a result. Those calls
     must not be cached: a later edit can change what a cell holds, and a
     stale answer would give the pointer the wrong type. They must not evict
     the cached main program either -- the cache holds only the last program
     per instance, so alternating between the two would make every render
     re-run everything. */
  // Declare the semantics an instance runs. Idempotent in the wasm: asking
  // for the mode an instance already has does not reset it, which matters
  // because a livelit re-expands on every edit and a reset is destructive.
  const ensureMode = (id, mode) => {
    if (!ready()) {
      return JSON.stringify({
        ok: false,
        error: "the Fumola runtime is not loaded",
      });
    }
    try {
      return wasm.fumola_ensure_mode(id, mode);
    } catch (e) {
      return JSON.stringify({ ok: false, kind: "runtime", error: String(e) });
    }
  };

  const evalFresh = (id, src) => {
    if (!ready()) {
      return JSON.stringify({
        ok: false,
        error: "the Fumola runtime is not loaded",
      });
    }
    if (!wasm.fumola_has(id)) wasm.fumola_realize(id);
    try {
      return wasm.fumola_eval_top(id, src);
    } catch (e) {
      return JSON.stringify({ ok: false, kind: "runtime", error: String(e) });
    }
  };

  return { ready, source, claim, ensureMode, evalSync, evalTop, evalFresh };
})();
