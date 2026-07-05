
// This file is bundled into bundle.js as part of the build process.
import {NinjaKeys} from 'ninja-keys';
import hotkeys from 'hotkeys-js'
import Algebrite from 'algebrite';
window.Algebrite = Algebrite;
window.HazelAlgebrite = {
  simplifyToString(input) {
    if (!window.Algebrite || !window.Algebrite.simplify) {
      throw new Error('Algebrite is not available.');
    }
    const result = window.Algebrite.simplify(input);
    return result && result.toString ? result.toString() : String(result);
  },
};

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

const ensureCoqHost = ({
  hostId = 'hazel-jscoq-host',
  wrapperId = 'hazel-jscoq-wrapper',
  codeId = 'hazel-jscoq-code',
} = {}) => {
  let host = document.getElementById(hostId);
  if (host) return host;

  host = document.createElement('div');
  host.id = hostId;
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
  wrapper.id = wrapperId;
  wrapper.className = 'jscoq-main';

  const code = document.createElement('textarea');
  code.id = codeId;
  code.value = '(* Hazel generated Coq will appear here. *)';

  host.appendChild(close);
  wrapper.appendChild(code);
  host.appendChild(wrapper);
  document.body.appendChild(host);
  return host;
};

window.HazelJSCoq = {
  manager: null,
  warmupPromise: null,
  checkCounter: 0,
  activeChecks: new Map(),
  completedChecks: [],
  maxCompletedChecks: 20,
  retiredManagers: [],
  maxRetiredManagers: 2,
  managerRetirementDelayMs: 5000,

  jscoqPaths() {
    return {
      jscoqBasePath: new URL('/jscoq/', window.location.href).href,
      jscoqPkgPath: new URL('/jscoq/coq-pkgs/', window.location.href).href,
      nodeModulesPath: new URL('/node_modules/', window.location.href).href,
    };
  },

  installHiddenManagerGuards(manager) {
    if (!manager || manager.__hazelHiddenGuardsInstalled) return;
    manager.__hazelHiddenGuardsInstalled = true;

    const markError = (sid, loc, msg) => {
      const stm = manager.doc && manager.doc.stm_id
        ? manager.doc.stm_id[sid]
        : null;
      if (stm) {
        stm.phase = 'error';
        stm.feedback = stm.feedback || [];
        stm.feedback.push({level: 'Error', loc, msg});
        if (!manager.error.includes(stm)) manager.error.push(stm);
      } else if (manager.error.length === 0) {
        manager.error.push({coq_sid: sid, feedback: [{level: 'Error', loc, msg}]});
      }
    };

    manager.feedMessage = (sid, lvl, loc, msg) => {
      const level = Array.isArray(lvl) ? lvl[0] : lvl;
      if (level === 'Error') markError(sid, loc, msg);
    };
    manager.coqLog = () => {};
    manager.coqLibError = () => {};
    manager.coqJsonExn = msg => {
      if (manager.error.length === 0) {
        manager.error.push({coq_sid: 0, feedback: [{level: 'Error', msg}]});
      }
    };
    manager.coqCoqExn = ({pp, msg, sids} = {}) => {
      markError(sids && sids.length > 0 ? sids[0] : 0, undefined, pp || msg);
    };
    if (manager.pprint) manager.pprint.adjustBreaks = () => {};
  },

  async makeManager({code = '', show = true, fresh = false} = {}) {
    ensureStyle('jscoq/frontend/classic/css/ide-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-light.css');
    const hostIds = fresh
      ? (() => {
          const id = ++this.checkCounter;
          return {
            hostId: `hazel-jscoq-check-host-${id}`,
            wrapperId: `hazel-jscoq-check-wrapper-${id}`,
            codeId: `hazel-jscoq-check-code-${id}`,
          };
        })()
      : {
          hostId: 'hazel-jscoq-host',
          wrapperId: 'hazel-jscoq-wrapper',
          codeId: 'hazel-jscoq-code',
        };
    const host = ensureCoqHost(hostIds);
    const codeNode = document.getElementById(hostIds.codeId);
    if (code) codeNode.value = code;
    host.style.display = show ? 'block' : 'none';

    const { JsCoq } = await loadJsCoq();
    const {jscoqBasePath, jscoqPkgPath, nodeModulesPath} = this.jscoqPaths();
    const manager = await JsCoq.start(
      jscoqBasePath,
      nodeModulesPath,
      [hostIds.codeId],
      {
        wrapper_id: hostIds.wrapperId,
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
    manager.__hazelHostId = hostIds.hostId;
    manager.__hazelCodeBytes = code.length;
    if (!show) this.installHiddenManagerGuards(manager);
    return manager;
  },

  endManagerWorker(manager) {
    if (!manager || manager.__hazelWorkerEnded) return;
    try {
      if (manager.coq && typeof manager.coq.end === 'function') {
        manager.coq.end();
      }
      manager.__hazelWorkerEnded = true;
    } catch (error) {
      console.warn('[Hazel JSCoq] worker termination failed during cleanup', error);
    }
  },

  cleanupManager(manager, {force = false} = {}) {
    if (!manager) return;
    const hostId = manager.__hazelHostId;

    if (force) {
      manager.__hazelAborted = true;
      try {
        const lastAdded = typeof manager.lastAdded === 'function' ? manager.lastAdded() : null;
        if (lastAdded && typeof manager.cancel === 'function') manager.cancel(lastAdded);
      } catch (error) {
        console.warn('[Hazel JSCoq] manager cancel failed during cleanup', error);
      }
    }

    if (manager.__hazelRetireTimer) {
      window.clearTimeout(manager.__hazelRetireTimer);
      manager.__hazelRetireTimer = null;
    }

    const removeHiddenHost = () => {
      if (!hostId || hostId === 'hazel-jscoq-host') return;
      const host = document.getElementById(hostId);
      if (host) host.remove();
    };

    const finishCleanup = () => {
      this.endManagerWorker(manager);
      removeHiddenHost();
      this.retiredManagers = this.retiredManagers.filter(retired => retired !== manager);
    };

    if (force) finishCleanup();
    else {
      manager.__hazelRetireTimer = window.setTimeout(
        finishCleanup,
        this.managerRetirementDelayMs,
      );
    }
  },

  retireManager(manager) {
    if (!manager) return;
    this.retiredManagers.push(manager);
    this.cleanupManager(manager);
    while (this.retiredManagers.length > this.maxRetiredManagers) {
      const oldManager = this.retiredManagers.shift();
      this.cleanupManager(oldManager, {force: true});
    }
    this.cleanupStrayHiddenHosts();
  },

  cleanupRetiredManagers({force = false} = {}) {
    for (const manager of this.retiredManagers) {
      this.cleanupManager(manager, {force});
    }
    this.retiredManagers = [];
  },

  cleanupStrayHiddenHosts() {
    const retainedHostIds = new Set(
      this.retiredManagers
        .map(manager => manager && manager.__hazelHostId)
        .filter(Boolean),
    );
    for (const check of this.activeChecks.values()) {
      if (check.manager && check.manager.__hazelHostId) {
        retainedHostIds.add(check.manager.__hazelHostId);
      }
    }
    document
      .querySelectorAll('.hazel-jscoq-host[id^="hazel-jscoq-check-host-"]')
      .forEach(node => {
        if (!retainedHostIds.has(node.id)) node.remove();
      });
  },

  reset({clearModule = false} = {}) {
    console.warn('[Hazel JSCoq] resetting JSCoq bridge state');
    for (const check of this.activeChecks.values()) {
      this.cleanupManager(check.manager, {force: true});
    }
    this.activeChecks.clear();
    this.cleanupRetiredManagers({force: true});
    this.cleanupManager(this.manager, {force: true});
    this.manager = null;
    this.warmupPromise = null;
    document
      .querySelectorAll('.hazel-jscoq-host[id^="hazel-jscoq-check-host-"]')
      .forEach(node => node.remove());
    if (clearModule) jscoqModulePromise = null;
    return this.stats();
  },

  stats() {
    return {
      activeChecks: this.activeChecks.size,
      completedChecks: this.completedChecks.slice(),
      checkCounter: this.checkCounter,
      hasWarmManager: !!this.manager,
      hasWarmupPromise: !!this.warmupPromise,
      retiredManagers: this.retiredManagers.length,
      memory: performance && performance.memory
        ? {
            usedJSHeapSize: performance.memory.usedJSHeapSize,
            totalJSHeapSize: performance.memory.totalJSHeapSize,
            jsHeapSizeLimit: performance.memory.jsHeapSizeLimit,
          }
        : null,
      hiddenCheckHosts:
        document.querySelectorAll('.hazel-jscoq-host[id^="hazel-jscoq-check-host-"]').length,
    };
  },

  recordCompletedCheck(record) {
    this.completedChecks.push(record);
    while (this.completedChecks.length > this.maxCompletedChecks) {
      this.completedChecks.shift();
    }
  },

  async start({code = '', show = true} = {}) {
    ensureStyle('jscoq/frontend/classic/css/ide-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-base.css');
    ensureStyle('jscoq/frontend/classic/css/coq-light.css');

    if (!this.manager) {
      this.manager = await this.makeManager({code, show, fresh: false});
    }

    return this.manager;
  },

  async check(code, {show = true, advanceLimit = 200} = {}) {
    // The browser-bundled JSCoq currently uses the pre-Coq-9 logical prefix,
    // while downloaded files target modern coqc with Stdlib.
    const jscoqCode = code.replaceAll('From Stdlib Require', 'From Coq Require');
    const startedAt = performance.now();
    const checkId = ++this.checkCounter;
    console.log(
      '[Hazel JSCoq] starting check',
      {checkId, codeBytes: jscoqCode.length, advanceLimit},
    );
    const manager = await this.makeManager({
      code: jscoqCode,
      show,
      fresh: true,
    });
    this.activeChecks.set(checkId, {manager, startedAt, codeBytes: jscoqCode.length});
    await manager.when_ready.promise;

    const waitForSettled = () => new Promise(resolve => {
      const poll = () => {
        if (manager.__hazelAborted) {
          resolve();
          return;
        }
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
      if (manager.__hazelAborted) break;
      const advanced = manager.goNext(false);
      if (!advanced) break;
      steps += 1;
      await waitForSettled();
      if (manager.__hazelAborted) break;
      if (manager.error.length > 0) break;
    }

    const durationMs = Math.round(performance.now() - startedAt);
    const result = {
      ok: !manager.__hazelAborted && manager.error.length === 0,
      steps,
      errors: manager.__hazelAborted
        ? ['JSCoq check was aborted.']
        : manager.error,
      manager,
      checkId,
      durationMs,
      codeBytes: jscoqCode.length,
    };
    this.activeChecks.delete(checkId);
    this.recordCompletedCheck({
      checkId,
      ok: result.ok,
      steps,
      durationMs,
      codeBytes: jscoqCode.length,
      errorCount: manager.error.length,
    });
    console.log('[Hazel JSCoq] finished check', this.completedChecks[this.completedChecks.length - 1]);
    if (!show) this.retireManager(manager);
    return result;
  },

  warmupSearch(opts = {}) {
    if (!this.warmupPromise) {
      const warmupCode = [
        'From Coq Require Import Reals.',
        'Open Scope R_scope.',
        'Theorem hazel_jscoq_warmup : forall x : R, sin x = sin x.',
        'Proof. intros. reflexivity. Qed.',
      ].join('\n');
      console.log('[Hazel JSCoq] warming up Rocq tactic search');
      this.warmupPromise = this.start({code: warmupCode, show: false})
        .then(async manager => {
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
          const advanceLimit = opts.advanceLimit || 80;
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
        })
        .then(result => {
          console.log(
            '[Hazel JSCoq]',
            result.ok ? 'Rocq tactic search warmup passed.' : 'Rocq tactic search warmup failed.',
          );
          return result;
        })
        .catch(error => {
          this.warmupPromise = null;
          console.warn('[Hazel JSCoq] Rocq tactic search warmup failed to run', error);
          return {ok: false, errors: [error]};
        });
    }
    return this.warmupPromise;
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

    console.log(
      '[Hazel JSCoq] checking generated Coq program',
      {codeBytes: code.length, activeChecks: this.activeChecks.size},
    );
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
    const startedAt = performance.now();
    return Promise.race([this.check(code, {show: false, ...opts}), timeout])
      .then(result => {
        if (result && result.errors && result.errors.some(error =>
          typeof error === 'string' && error.includes('timed out')
        )) {
          this.reset();
        }
        const message = result.ok
          ? 'JSCoq passed.'
          : `JSCoq failed:\n${formatErrors(result.errors)}`;
        console.log(
          '[Hazel JSCoq]',
          message,
          {durationMs: Math.round(performance.now() - startedAt), stats: this.stats()},
        );
        callback(result.ok ? 'ok' : 'error', message);
        return result;
      })
      .catch(error => {
        const message = `JSCoq check failed to run: ${error && error.message ? error.message : String(error)}`;
        console.error('[Hazel JSCoq]', message, error);
        this.reset();
        callback('error', message);
        return {ok: false, errors: [error]};
      });
  },

  searchAndReport(code, callback, opts = {}) {
    console.log('[Hazel JSCoq] running Rocq tactic search candidate');
    console.log('[Hazel JSCoq] Rocq tactic search candidate source:\n' + code);
    return this.checkAndReport(code, callback, {show: false, ...opts});
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
