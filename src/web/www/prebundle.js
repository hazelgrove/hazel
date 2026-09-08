
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
  // A published origin offers two ways in. `runtime.json` names a
  // content-addressed directory for the current build; the pair at the root
  // is that same build, at a URL that does not change.
  //
  // The manifest is what makes a deploy visible. GitHub Pages stamps
  // `Cache-Control: max-age=600` on everything it serves and offers no way to
  // change it, so a new build cannot invalidate a binary a browser already
  // holds. Giving each build its own URL sidesteps that: the stale copy is
  // simply never requested again. Without it, a livelit can run against a
  // runtime a version behind and look like a bug rather than a cache.
  // See Adapton/fumola#69.
  //
  // Both remain one source with one timeout budget, rather than two entries
  // each -- doubling the source count would double the worst case a page
  // waits before reporting the runtime unavailable.
  const published = (name, origin) => ({
    name,
    origin,
    manifest: origin + "/runtime.json",
    glue: origin + "/fumola_wasm.js",
    wasm: origin + "/fumola_wasm_bg.wasm",
  });
  const SOURCES = [
    {
      name: "local",
      glue: here("./fumola/fumola_wasm.js"),
      wasm: here("./fumola/fumola_wasm_bg.wasm"),
    },
    published("fumola.org", "https://fumola.org"),
    published("adapton.github.io", "https://adapton.github.io/fumola"),
  ];

  // Hidden from the bundler so that Hazel builds without the generated files.
  const dynamicImport = new Function("p", "return import(p)");
  const load = async (from) => {
    let { glue, wasm } = from;
    let version = "stable";
    if (from.manifest) {
      try {
        // `no-cache` revalidates rather than trusting the 600s window: this
        // is the one fetch whose staleness would defeat the point, and it is
        // a few hundred bytes answered by a 304.
        const reply = await fetch(from.manifest, { cache: "no-cache" });
        if (reply.ok) {
          const manifest = await reply.json();
          // The two are taken together or not at all. wasm-bindgen emits them
          // as a matched set and they do not load if their versions disagree,
          // so a half-applied manifest would be worse than ignoring it.
          if (manifest.js && manifest.wasm) {
            glue = from.origin + manifest.js;
            wasm = from.origin + manifest.wasm;
            version = manifest.hash || "unknown";
          }
        }
      } catch (e) {
        // No manifest, or it did not parse: fall through to the pair at the
        // root, which is this origin's current build at publish time. This is
        // also the path for an origin published before manifests existed.
      }
    }
    const mod = await dynamicImport(glue);
    await mod.default({ module_or_path: wasm });
    return { mod, version };
  };
  // A source that fails is fine: the loop moves on. A source that HANGS is
  // not. Nothing in fetch or import times out on its own, so a source that
  // stalls at the TCP or TLS level -- rather than refusing -- parks the loop
  // forever. No later source is tried and no event is dispatched, which is
  // exactly the stuck "still loading" this event exists to end, reached by a
  // different road.
  //
  // The budget is generous because a slow connection is not a broken one and
  // the wasm is several megabytes. And an overrunning source is set aside
  // rather than cancelled: if it lands later and nothing else has won by
  // then, it is still the runtime we wanted, so it is taken and announced.
  // Per source, so a page where every source stalls waits this many times the
  // number of sources before it can say "unavailable" -- currently 90s. Long,
  // but it replaces a message that was wrong forever rather than slow.
  const LOAD_TIMEOUT_MS = 30000;

  const withTimeout = (attempt, ms, name) =>
    new Promise((resolve, reject) => {
      const timer = setTimeout(
        () => reject(new Error(name + " did not answer within " + ms + "ms")),
        ms
      );
      attempt.then(
        (v) => {
          clearTimeout(timer);
          resolve(v);
        },
        (e) => {
          clearTimeout(timer);
          reject(e);
        }
      );
    });

  (async () => {
    try {
      // Sequential by default: a later source is a fallback, not a race, so
      // the common path loads one runtime and no more.
      //
      // The budget below is the one exception, and it is deliberate. A source
      // that overruns is set aside rather than cancelled -- cancelling an
      // in-flight import is not on offer anyway -- so two loads can briefly
      // be in flight, and an abandoned one that lands after another has won
      // will have instantiated a second module before it is discarded. That
      // is a transient second copy of several megabytes on an uncommon path,
      // accepted because the alternative is throwing away a source that was
      // merely slow.
      const failures = [];
      for (const from of SOURCES) {
        // Set when this source overruns its budget and the search moves on.
        // The late claim below is conditional on it: without that test the
        // claim also fires on the ordinary path, because it is attached
        // before the one inside withTimeout and so runs first, while wasm is
        // still null.
        let abandoned = false;
        try {
          const attempt = load(from);
          // Claim a late arrival, but only if the search gave up on it and
          // nothing else has answered since.
          attempt.then(
            (loaded) => {
              if (abandoned && wasm === null) {
                wasm = loaded.mod;
                loadedFrom = from.name + " @ " + loaded.version;
                console.info(
                  "Fumola livelit: runtime loaded from " + loadedFrom + " (late)"
                );
                window.dispatchEvent(new Event("fumola-runtime-ready"));
              }
            },
            () => {}
          );
          const loaded = await withTimeout(attempt, LOAD_TIMEOUT_MS, from.name);
          wasm = loaded.mod;
          // The version is part of the answer, not decoration: "which Fumola
          // is this page holding?" is otherwise unanswerable from a browser,
          // and it is the first question worth asking when a livelit
          // misbehaves for someone and not for you.
          loadedFrom = from.name + " @ " + loaded.version;
          console.info("Fumola livelit: runtime loaded from " + loadedFrom);
          return;
        } catch (e) {
          abandoned = true;
          failures.push(from.name + " (" + e + ")");
        }
      }
      loadError = "tried " + failures.join("; ");
      console.warn("Fumola livelit: wasm runtime unavailable: " + loadError);
    } finally {
      // Announce the outcome, either way.
      //
      // This load is asynchronous and the wasm is a few megabytes, usually
      // fetched cross-origin, so a program is routinely elaborated before it
      // arrives. Such a program is not wrong -- its Fumola livelits expand to
      // "the runtime is still loading", which was true when it was said -- but
      // nothing would ever revisit it, so the card stayed that way until the
      // page was reloaded. Hazel listens for this and recalculates.
      //
      // In a finally so that it covers the early return above as well as
      // running out of sources, and so it is reached after wasm and loadError
      // are set: a listener that asks ready() gets the settled answer. On
      // failure it matters too -- the message settles from "still loading" to
      // "unavailable" rather than waiting on a load that is not coming.
      window.dispatchEvent(new Event("fumola-runtime-ready"));
    }
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
