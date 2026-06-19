
// This file is bundled into bundle.js as part of the build process.

// z3-solver's browser entry reads the Node global `global` (mapped to
// globalThis by esbuild's --define), so make sure it exists.
if (typeof globalThis.global === 'undefined') {
  globalThis.global = globalThis;
}

import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

// Test input generation: the official Z3 WebAssembly build. The native OCaml
// z3 bindings can't run under js_of_ocaml, so the web frontend solves via
// this WASM package. We lazily initialize it (the wasm is multi-MB) and
// expose a callback-style API so the OCaml side (src/web/projectors/Z3Wasm.re)
// avoids Promise interop entirely.
//
// In the browser, z3-solver's `init()` expects a global `initZ3` factory,
// which is defined only by loading the emscripten glue (z3-built.js) as a
// classic script. esbuild bundling doesn't do that, so we inject the script
// ourselves on first use. z3-built.js locates z3-built.wasm relative to its
// own URL, so both files are served from the www root (see src/web/www/dune).
import {init as initZ3Pkg} from 'z3-solver';
let z3 = null; // the initialized low-level Z3 module (WASM), created once
function loadZ3Builder() {
  return new Promise((resolve, reject) => {
    if (globalThis.initZ3) {
      resolve();
      return;
    }
    const s = document.createElement('script');
    s.src = 'z3-built.js';
    s.onload = () => resolve();
    s.onerror = () => reject(new Error('failed to load z3-built.js'));
    document.head.appendChild(s);
  });
}
async function ensureZ3() {
  if (!z3) {
    // z3-solver's WASM is a pthreads build: it spawns worker threads, which
    // need SharedArrayBuffer, which the browser only exposes on
    // cross-origin-isolated pages. Without it z3 dies with a cryptic
    // "thread constructor failed"; surface the real cause instead.
    if (!globalThis.crossOriginIsolated) {
      throw new Error(
        'Test input generation needs cross-origin isolation (for SharedArrayBuffer). '
          + 'Serve Hazel with headers "Cross-Origin-Opener-Policy: same-origin" and '
          + '"Cross-Origin-Embedder-Policy: credentialless".'
      );
    }
    await loadZ3Builder();
    z3 = (await initZ3Pkg()).Z3;
  }
  return z3;
}
// hazelZ3Solve(smtlib2String, onResult): runs the script and calls onResult
// with the solver's raw output text (or an "error\n<msg>" string on failure,
// which TestGen.parse_model reports as an Error outcome).
//
// A FRESH context per solve: eval_smtlib2_string mutates the context's
// internal solver — set-logic, declarations, and assertions all persist — so
// reusing one context leaks the previous program's state into the next solve.
//
// Solves are SERIALIZED through a promise queue. The z3-solver WASM is a single
// module instance; overlapping eval_smtlib2_string calls interleave their
// strings in the shared WASM heap and corrupt each other (the solver then
// reports "unexpected character" parse errors). Callers legitimately fire
// several solves at once (e.g. a point plus all its groups), so we run them one
// at a time. onResult is invoked exactly once per call, and the queue always
// advances even if a solve or callback throws.
let z3Queue = Promise.resolve();
function runOneZ3Solve(smt, onResult) {
  return ensureZ3()
    .then(async Z3 => {
      const cfg = Z3.mk_config();
      const ctx = Z3.mk_context(cfg);
      Z3.del_config(cfg);
      try {
        return await Z3.eval_smtlib2_string(ctx, smt);
      } finally {
        try {
          Z3.del_context(ctx);
        } catch (_) {
          /* best-effort cleanup */
        }
      }
    })
    .then(
      output => {
        try {
          onResult(output);
        } catch (_) {
          /* don't let a callback failure stall the queue */
        }
      },
      err => {
        try {
          onResult('error\n' + (err && err.message ? err.message : String(err)));
        } catch (_) {
          /* ditto */
        }
      }
    );
}
window.hazelZ3Solve = function (smt, onResult) {
  z3Queue = z3Queue.then(() => runOneZ3Solve(smt, onResult));
};

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
