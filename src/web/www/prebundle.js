
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;

let jscoqModulePromise = null;
const loadJsCoq = async () => {
  if (!jscoqModulePromise) {
    const runtimeImport = new Function('specifier', 'return import(specifier)');
    jscoqModulePromise = runtimeImport('/jscoq/jscoq.js');
  }
  return jscoqModulePromise;
};

const ensureStyle = href => {
  if ([...document.styleSheets].some(sheet => sheet.href && sheet.href.endsWith(href))) {
    return;
  }
  const link = document.createElement('link');
  link.rel = 'stylesheet';
  link.href = href;
  document.head.appendChild(link);
};

const ensureCoqHost = () => {
  let host = document.getElementById('hazel-jscoq-host');
  if (host) return host;

  host = document.createElement('div');
  host.id = 'hazel-jscoq-host';
  host.className = 'hazel-jscoq-host';
  host.style.position = 'fixed';
  host.style.right = '12px';
  host.style.bottom = '12px';
  host.style.width = 'min(720px, 90vw)';
  host.style.height = 'min(480px, 70vh)';
  host.style.zIndex = '1000';
  host.style.display = 'none';

  const close = document.createElement('button');
  close.type = 'button';
  close.textContent = 'x';
  close.title = 'Hide JSCoq panel';
  close.style.position = 'absolute';
  close.style.top = '6px';
  close.style.right = '8px';
  close.style.zIndex = '1001';
  close.style.border = '1px solid #777';
  close.style.background = '#fff';
  close.style.color = '#222';
  close.style.cursor = 'pointer';
  close.onclick = () => {
    host.style.display = 'none';
  };

  const wrapper = document.createElement('div');
  wrapper.id = 'hazel-jscoq-wrapper';
  wrapper.className = 'jscoq-main';

  const code = document.createElement('textarea');
  code.id = 'hazel-jscoq-code';
  code.value = '(* Hazel generated Coq will appear here. *)';

  host.appendChild(close);
  wrapper.appendChild(code);
  host.appendChild(wrapper);
  document.body.appendChild(host);
  return host;
};

window.HazelJSCoq = {
  manager: null,

  async start({code = '', show = true} = {}) {
    ensureStyle('jscoq/frontend/classic/css/ide-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-light.css');
    const host = ensureCoqHost();
    const codeNode = document.getElementById('hazel-jscoq-code');
    if (code) codeNode.value = code;
    host.style.display = show ? 'block' : 'none';

    if (!this.manager) {
      const { JsCoq } = await loadJsCoq();
      const jscoqBasePath = new URL('/jscoq/', window.location.href).href;
      const jscoqPkgPath =
        new URL('/jscoq/coq-pkgs/', window.location.href).href;
      const nodeModulesPath =
        new URL('/node_modules/', window.location.href).href;
      this.manager = await JsCoq.start(
        jscoqBasePath,
        nodeModulesPath,
        ['hazel-jscoq-code'],
        {
          wrapper_id: 'hazel-jscoq-wrapper',
          base_path: jscoqBasePath,
          pkg_path: jscoqPkgPath,
          node_modules_path: nodeModulesPath,
          prelaunch: true,
          prelude: true,
          implicit_libs: true,
          init_pkgs: ['init', 'coq-base', 'coq-arith', 'coq-reals'],
          all_pkgs: ['coq'],
          show,
          focus: false,
          replace: false,
          editor: { keyMap: 'default' },
        },
      );
    }

    return this.manager;
  },

  async check(code, {show = true, advanceLimit = 200} = {}) {
    // The browser-bundled JSCoq currently uses the pre-Coq-9 logical prefix,
    // while downloaded files target modern coqc with Stdlib.
    const jscoqCode = code.replaceAll('From Stdlib Require', 'From Coq Require');
    const manager = await this.start({code: jscoqCode, show});
    await manager.when_ready.promise;

    const waitForSettled = () => new Promise(resolve => {
      const poll = () => {
        const active = manager.doc.sentences.some(stm =>
          stm && (stm.phase === 'pending' || stm.phase === 'adding' ||
            stm.phase === 'added' || stm.phase === 'processing')
        );
        if (!active) resolve();
        else window.setTimeout(poll, 20);
      };
      poll();
    });

    let steps = 0;
    while (steps < advanceLimit) {
      const advanced = manager.goNext(false);
      if (!advanced) break;
      steps += 1;
      await waitForSettled();
      if (manager.error.length > 0) break;
    }

    return {
      ok: manager.error.length === 0,
      steps,
      errors: manager.error,
      manager,
    };
  },

  checkAndReport(code, callback, opts = {}) {
    const formatErrors = errors => {
      if (!errors || errors.length === 0) return 'no JSCoq errors reported';
      return errors.map(error => {
        if (typeof error === 'string') return error;
        if (error && error.message) return error.message;
        try {
          return JSON.stringify(error);
        } catch (_) {
          return String(error);
        }
      }).join('\n');
    };

    console.log('[Hazel JSCoq] checking generated Coq program');
    const timeoutMs = opts.timeoutMs || 45000;
    const timeout = new Promise(resolve => {
      window.setTimeout(
        () => resolve({
          ok: false,
          steps: 0,
          errors: [`JSCoq check timed out after ${timeoutMs}ms.`],
        }),
        timeoutMs,
      );
    });
    return Promise.race([this.check(code, {show: false, ...opts}), timeout])
      .then(result => {
        const message = result.ok
          ? 'JSCoq passed.'
          : `JSCoq failed:\n${formatErrors(result.errors)}`;
        console.log('[Hazel JSCoq]', message);
        callback(result.ok ? 'ok' : 'error', message);
        return result;
      })
      .catch(error => {
        const message = `JSCoq check failed to run: ${error && error.message ? error.message : String(error)}`;
        console.error('[Hazel JSCoq]', message, error);
        callback('error', message);
        return {ok: false, errors: [error]};
      });
  },
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
