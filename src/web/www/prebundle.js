
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
  factorToString(input) {
    if (!window.Algebrite || !window.Algebrite.factor) {
      throw new Error('Algebrite factoring is not available.');
    }
    const result = window.Algebrite.factor(input);
    return result && result.toString ? result.toString() : String(result);
  },
  hasNontrivialFactorization(input) {
    if (!window.Algebrite || !window.Algebrite.factor || !window.Algebrite.run) {
      return false;
    }
    const factored = window.Algebrite.factor(input).toString();
    const canonicalInput = window.Algebrite.run(input).toString();
    return factored !== canonicalInput;
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
  // Temporary diagnostic switch: capture generated Rocq programs without
  // starting JSCoq or its warmup worker.
  debugSkipRocqSearch: false,
  debugPrograms: [],
  manager: null,
  managerPromise: null,
  warmupPromise: null,
  checkCounter: 0,
  activeChecks: new Map(),
  searchRequests: new Map(),
  completedChecks: [],
  maxCompletedChecks: 20,
  retiredManagers: [],
  maxRetiredManagers: 2,
  managerRetirementDelayMs: 5000,
  managerGenerationCounter: 0,
  recycleCounter: 0,
  recyclePromise: null,
  maxPersistentStateId: 80,
  maxPersistentUserChecks: 12,
  warmupLibraryCode:
    'From Coq Require Import ZArith Lia Ring Rbase Rfunctions Rtrigo1 Cos_plus Lra Reals.Ranalysis1 Reals.Ranalysis3 Reals.Rtrigo_reg.\n',

  jscoqPaths() {
    return {
      jscoqBasePath: new URL('/jscoq/', window.location.href).href,
      jscoqPkgPath: new URL('/jscoq/coq-pkgs/', window.location.href).href,
      nodeModulesPath: new URL('/node_modules/', window.location.href).href,
    };
  },

  settleCancellationFutures(manager, sid) {
    if (!sid || !manager || !manager.coq || !manager.coq.sids) return;
    for (const [futureSid, future] of Object.entries(manager.coq.sids)) {
      if (+futureSid >= sid && future) {
        // JSCoq rejects these bookkeeping futures with `undefined` when
        // Cancel is sent. Settle them first so an intentional Hazel
        // cancellation does not become an unhandled page rejection.
        if (typeof future.resolve === 'function') future.resolve(null);
        else if (future.promise) future.promise.catch(() => {});
      }
    }
  },

  installHiddenManagerGuards(manager) {
    if (!manager || manager.__hazelHiddenGuardsInstalled) return;
    manager.__hazelHiddenGuardsInstalled = true;

    if (manager.coq && typeof manager.coq.cancel === 'function') {
      const rawCancel = manager.coq.cancel.bind(manager.coq);
      manager.coq.cancel = sid => {
        this.settleCancellationFutures(manager, sid);
        return rawCancel(sid);
      };
    }

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
      const activeSentence = manager.doc && manager.doc.sentences
        ? manager.doc.sentences.slice().reverse().find(stm =>
            stm && (stm.phase === 'pending' || stm.phase === 'adding' ||
              stm.phase === 'added' || stm.phase === 'processing'))
        : null;
      const sid = sids && sids.length > 0
        ? sids[0]
        : activeSentence && activeSentence.coq_sid
          ? activeSentence.coq_sid
          : 0;
      markError(sid, undefined, pp || msg);
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
    const needsReals = !fresh || code.includes('Require Import Reals');
    const initPackages = ['init', 'coq-base', 'coq-arith'];
    if (needsReals) initPackages.push('coq-reals');
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
        init_pkgs: initPackages,
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

  cancelManagerWork(manager) {
    if (!manager || !manager.doc) return false;
    const activeSentence = manager.doc.sentences.slice().reverse().find(stm =>
      stm && (stm.phase === 'pending' || stm.phase === 'adding' ||
        stm.phase === 'added' || stm.phase === 'processing')
    );
    if (!activeSentence) return false;
    try {
      const sid = activeSentence.coq_sid;
      if (typeof manager.cancel === 'function') manager.cancel(activeSentence);
      else if (manager.coq && typeof manager.coq.cancel === 'function' &&
          sid) {
        manager.coq.cancel(sid);
      } else return false;
      return true;
    } catch (error) {
      console.warn('[Hazel JSCoq] active proof cancellation failed', error);
      return false;
    }
  },

  abortActiveCheck(activeCheck, reason = 'The Hazel proof goal changed.') {
    if (!activeCheck || activeCheck.aborted) return false;
    activeCheck.aborted = true;
    activeCheck.abortReason = reason;
    this.cancelManagerWork(activeCheck.manager);
    return true;
  },

  cancelSearch(requestId, reason = 'The Hazel proof goal changed.') {
    const request = this.searchRequests.get(requestId);
    if (!request || request.settled || request.cancelled) return false;
    request.cancelled = true;
    request.cancelReason = reason;
    if (!request.callbackDelivered && request.callback) {
      request.callbackDelivered = true;
      request.callback('cancelled', reason);
    }
    if (request.activeCheckId != null) {
      this.abortActiveCheck(
        this.activeChecks.get(request.activeCheckId),
        reason,
      );
    }
    console.log('[Hazel JSCoq] cancelled obsolete Rocq search', {requestId, reason});
    return true;
  },

  cancelSearchesExcept(requestId = null) {
    let cancelled = 0;
    for (const id of this.searchRequests.keys()) {
      if (requestId == null || id !== requestId) {
        if (this.cancelSearch(id)) cancelled += 1;
      }
    }
    return cancelled;
  },

  managerHealth(manager = this.manager) {
    if (!manager) {
      return {generation: 0, userChecks: 0, highestStateId: 0};
    }
    const stateIds = [];
    if (manager.doc && manager.doc.sentences) {
      for (const sentence of manager.doc.sentences) {
        if (sentence && sentence.coq_sid) stateIds.push(+sentence.coq_sid);
      }
    }
    if (manager.doc && manager.doc.stm_id) {
      for (const sid of Object.keys(manager.doc.stm_id)) stateIds.push(+sid);
    }
    const lastAdded = typeof manager.lastAdded === 'function'
      ? manager.lastAdded()
      : null;
    if (lastAdded && lastAdded.coq_sid) stateIds.push(+lastAdded.coq_sid);
    return {
      generation: manager.__hazelGeneration || 0,
      userChecks: manager.__hazelUserChecks || 0,
      highestStateId: Math.max(manager.__hazelHighestStateId || 0, ...stateIds, 0),
    };
  },

  noteManagerCheck(manager, kind) {
    if (!manager) return this.managerHealth(manager);
    if (kind !== 'warmup') {
      manager.__hazelUserChecks = (manager.__hazelUserChecks || 0) + 1;
    }
    const health = this.managerHealth(manager);
    manager.__hazelHighestStateId = health.highestStateId;
    return health;
  },

  managerNeedsRecycle(manager = this.manager) {
    const health = this.managerHealth(manager);
    return health.highestStateId >= this.maxPersistentStateId ||
      health.userChecks >= this.maxPersistentUserChecks;
  },

  async recycleWarmManagerIfNeeded({request = null} = {}) {
    if (request && request.cancelled) return false;
    if (this.recyclePromise) return this.recyclePromise;
    const manager = this.manager;
    if (!manager || !this.managerNeedsRecycle(manager)) return false;
    if (this.activeChecks.size > 0) return false;

    const health = this.managerHealth(manager);
    this.recyclePromise = (async () => {
      if (this.manager !== manager || this.activeChecks.size > 0) return false;
      console.warn('[Hazel JSCoq] recycling persistent Rocq worker', health);
      this.manager = null;
      this.managerPromise = null;
      this.warmupPromise = null;
      this.cleanupManager(manager, {force: true});
      this.recycleCounter += 1;
      const warmup = await this.warmupSearch();
      if (!warmup || !warmup.ok) {
        throw new Error('The replacement persistent Rocq worker failed to warm up.');
      }
      return true;
    })().finally(() => {
      this.recyclePromise = null;
    });
    return this.recyclePromise;
  },

  waitForActiveChecks({request = null, timeoutMs = 5000} = {}) {
    return new Promise(resolve => {
      const startedAt = performance.now();
      const poll = () => {
        if (request && request.cancelled) resolve({idle: false, cancelled: true});
        else if (this.activeChecks.size === 0) resolve({idle: true});
        else if (performance.now() - startedAt >= timeoutMs) {
          resolve({idle: false, cancelled: false});
        } else window.setTimeout(poll, 20);
      };
      poll();
    });
  },

  reset({clearModule = false} = {}) {
    console.warn('[Hazel JSCoq] resetting JSCoq bridge state');
    for (const check of this.activeChecks.values()) {
      check.aborted = true;
      this.cleanupManager(check.manager, {force: true});
    }
    this.activeChecks.clear();
    for (const request of this.searchRequests.values()) {
      request.cancelled = true;
      request.settled = true;
    }
    this.searchRequests.clear();
    this.cleanupRetiredManagers({force: true});
    this.cleanupManager(this.manager, {force: true});
    this.manager = null;
    this.managerPromise = null;
    this.warmupPromise = null;
    this.recyclePromise = null;
    document
      .querySelectorAll('.hazel-jscoq-host[id^="hazel-jscoq-check-host-"]')
      .forEach(node => node.remove());
    if (clearModule) jscoqModulePromise = null;
    return this.stats();
  },

  stats() {
    return {
      activeChecks: this.activeChecks.size,
      activeSearchRequests: this.searchRequests.size,
      completedChecks: this.completedChecks.slice(),
      checkCounter: this.checkCounter,
      hasWarmManager: !!this.manager,
      hasWarmupPromise: !!this.warmupPromise,
      managerHealth: this.managerHealth(),
      recycleCounter: this.recycleCounter,
      recycleInProgress: !!this.recyclePromise,
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

    if (!this.managerPromise) {
      this.managerPromise = this.makeManager({code, show, fresh: false})
        .then(manager => {
          manager.__hazelGeneration = ++this.managerGenerationCounter;
          manager.__hazelUserChecks = 0;
          manager.__hazelHighestStateId = 0;
          this.manager = manager;
          return manager;
        })
        .catch(error => {
          this.managerPromise = null;
          throw error;
        });
    }

    return this.managerPromise;
  },

  async replaceManagerDocument(manager, code) {
    const prefixSentences = manager.__hazelWarmupPrefixSentences ||
      manager.doc.sentences.slice(0, 1);
    const firstState = manager.doc.sentences
      .slice(prefixSentences.length)
      .find(stm => stm && stm.coq_sid);
    if (firstState) {
      const sid = firstState.coq_sid;
      manager.coq.cancel(sid);
      await new Promise((resolve, reject) => {
        const startedAt = performance.now();
        const poll = () => {
          if (!manager.doc.stm_id[sid]) resolve();
          else if (performance.now() - startedAt > 3000) {
            reject(new Error('Timed out while clearing the previous JSCoq document.'));
          } else window.setTimeout(poll, 20);
        };
        poll();
      });
    }

    manager.provider.retract();
    manager.doc.sentences = prefixSentences.slice();
    manager.doc.stm_id = [];
    for (const sentence of prefixSentences) {
      if (sentence && sentence.coq_sid) {
        manager.doc.stm_id[sentence.coq_sid] = sentence;
      }
    }
    manager.error = [];
    const snippet = manager.provider.snippets && manager.provider.snippets[0];
    if (!snippet || !snippet.editor) {
      throw new Error('JSCoq document provider is not ready.');
    }
    const suffix = manager.__hazelWarmupPrefixCode
      ? code.replace(/^From (?:Coq|Stdlib) Require Import [^\n]*\.\s*$/gm, '')
      : code;
    snippet.editor.setValue((manager.__hazelWarmupPrefixCode || '') + suffix);
    return {
      reusedWarmupPrefix: !!manager.__hazelWarmupPrefixCode,
      submittedCodeBytes: suffix.length,
      retainedSentenceCount: prefixSentences.length,
    };
  },

  async check(
    code,
    {show = true, advanceLimit = 200, kind = 'validation', request = null} = {},
  ) {
    // The browser-bundled JSCoq currently uses the pre-Coq-9 logical prefix,
    // while downloaded files target modern coqc with Stdlib.
    const jscoqCode = code.replaceAll('From Stdlib Require', 'From Coq Require');
    const startedAt = performance.now();
    const checkId = ++this.checkCounter;
    const activeCheck = {
      manager: null,
      aborted: !!(request && request.cancelled),
      abortReason: request && request.cancelReason,
      startedAt,
      codeBytes: jscoqCode.length,
    };
    this.activeChecks.set(checkId, activeCheck);
    if (request) request.activeCheckId = checkId;
    console.log(
      '[Hazel JSCoq] starting check',
      {checkId, kind, codeBytes: jscoqCode.length, advanceLimit},
    );
    let manager;
    let preparedAt = startedAt;
    let documentStats = {
      reusedWarmupPrefix: false,
      submittedCodeBytes: jscoqCode.length,
      retainedSentenceCount: 0,
    };
    try {
      manager = await this.start({code: '', show});
      activeCheck.manager = manager;
      await manager.when_ready.promise;
      if (activeCheck.aborted || (request && request.cancelled)) {
        activeCheck.aborted = true;
      } else {
        documentStats = await this.replaceManagerDocument(manager, jscoqCode);
        preparedAt = performance.now();
      }
    } catch (error) {
      this.activeChecks.delete(checkId);
      throw error;
    }
    if (activeCheck.aborted) {
      this.activeChecks.delete(checkId);
      return {
        ok: false,
        steps: 0,
        errors: [activeCheck.abortReason || 'JSCoq check was cancelled during startup.'],
        cancelled: true,
        manager,
        checkId,
        durationMs: Math.round(performance.now() - startedAt),
        codeBytes: jscoqCode.length,
      };
    }
    const waitForSettled = () => new Promise(resolve => {
      const poll = () => {
        if (activeCheck.aborted || manager.__hazelAborted || manager.error.length > 0) {
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
      if (activeCheck.aborted || manager.__hazelAborted) break;
      const advanced = manager.goNext(false);
      if (!advanced) break;
      steps += 1;
      await waitForSettled();
      if (activeCheck.aborted || manager.__hazelAborted) break;
      if (manager.error.length > 0) break;
    }

    const durationMs = Math.round(performance.now() - startedAt);
    const result = {
      ok: !activeCheck.aborted && !manager.__hazelAborted && manager.error.length === 0,
      steps,
      errors: activeCheck.aborted
        ? [activeCheck.abortReason || 'JSCoq check was cancelled.']
        : manager.__hazelAborted
          ? ['JSCoq check was aborted.']
        : manager.error,
      cancelled: activeCheck.aborted,
      manager,
      checkId,
      durationMs,
      startupDurationMs: Math.round(preparedAt - startedAt),
      proofDurationMs: Math.round(performance.now() - preparedAt),
      codeBytes: jscoqCode.length,
      ...documentStats,
    };
    const managerHealth = this.noteManagerCheck(manager, kind);
    this.activeChecks.delete(checkId);
    this.recordCompletedCheck({
      checkId,
      kind,
      ok: result.ok,
      steps,
      durationMs,
      startupDurationMs: result.startupDurationMs,
      proofDurationMs: result.proofDurationMs,
      codeBytes: jscoqCode.length,
      ...documentStats,
      errorCount: manager.error.length,
      cancelled: activeCheck.aborted,
      managerHealth,
    });
    console.log('[Hazel JSCoq] finished check', this.completedChecks[this.completedChecks.length - 1]);
    return result;
  },

  warmupSearch(opts = {}) {
    if (this.debugSkipRocqSearch) {
      if (!this.warmupPromise) {
        console.warn('[Hazel Rocq debug] JSCoq warmup is disabled.');
        this.warmupPromise = Promise.resolve({
          ok: false,
          steps: 0,
          errors: ['JSCoq execution disabled for generated-program debugging.'],
          debugSkipped: true,
        });
      }
      return this.warmupPromise;
    }

    if (!this.warmupPromise) {
      console.log('[Hazel JSCoq] initializing persistent Rocq worker');
      this.warmupPromise = this.start({code: '', show: false})
        .then(async manager => {
          await manager.when_ready.promise;
          console.log('[Hazel JSCoq] preloading Hazel Rocq libraries');
          const preload = await this.check(
            this.warmupLibraryCode,
            {show: false, advanceLimit: 20, kind: 'warmup'},
          );
          if (!preload.ok) {
            throw new Error('JSCoq failed while preloading Hazel Rocq libraries.');
          }
          manager.__hazelWarmupPrefixCode = this.warmupLibraryCode;
          manager.__hazelWarmupPrefixSentences = manager.doc.sentences.slice();
          return preload;
        })
        .then(result => {
          console.log(
            '[Hazel JSCoq]',
            result.ok
              ? 'Persistent Rocq worker and libraries ready.'
              : 'Persistent Rocq worker failed.',
            {durationMs: result.durationMs, steps: result.steps},
          );
          return result;
        })
        .catch(error => {
          this.reset();
          console.warn('[Hazel JSCoq] Rocq tactic search warmup failed to run', error);
          return {ok: false, errors: [error]};
        });
    }
    return this.warmupPromise;
  },

  checkAndReport(code, callback, opts = {}) {
    const request = opts.request || null;
    const errorText = (error, seen = new Set()) => {
      if (error == null) return '';
      if (typeof error === 'string') return error;
      if (typeof error === 'number' || typeof error === 'boolean') return String(error);
      if (seen.has(error)) return '';
      if (typeof error === 'object') seen.add(error);
      if (Array.isArray(error)) {
        return error.map(item => errorText(item, seen)).filter(Boolean).join(' ');
      }
      if (error.message) return errorText(error.message, seen);
      if (error.msg) return errorText(error.msg, seen);
      if (error.pp) return errorText(error.pp, seen);
      if (error.feedback) return errorText(error.feedback, seen);
      if (error.textContent) return String(error.textContent);
      try {
        const text = String(error);
        return text === '[object Object]' ? JSON.stringify(error) : text;
      } catch (_) {
        return '';
      }
    };
    const formatErrors = errors => {
      if (!errors || errors.length === 0) return 'no JSCoq errors reported';
      return errors.map(error => errorText(error) || 'unknown JSCoq error').join('\n');
    };
    const reportCancellation = result => {
      if (request && !request.callbackDelivered) {
        request.callbackDelivered = true;
        request.settled = true;
        callback('cancelled', request.cancelReason || 'Rocq search cancelled.');
      }
      return result;
    };

    console.log(
      '[Hazel JSCoq] checking generated Coq program',
      {codeBytes: code.length, activeChecks: this.activeChecks.size},
    );
    const timeoutMs = opts.timeoutMs || 45000;
    return this.warmupSearch().then(warmup => {
      if (!warmup || !warmup.ok) {
        throw new Error('Persistent Rocq worker failed to become ready.');
      }
      if (request && request.cancelled) {
        return reportCancellation({
          ok: false,
          steps: 0,
          errors: [request.cancelReason || 'JSCoq check was cancelled.'],
          cancelled: true,
        });
      }
      let timeoutId;
      const timeout = new Promise(resolve => {
        timeoutId = window.setTimeout(
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
        window.clearTimeout(timeoutId);
        if (result.cancelled || (request && request.cancelled)) {
          console.log('[Hazel JSCoq] obsolete check stopped', {
            durationMs: Math.round(performance.now() - startedAt),
            stats: this.stats(),
          });
          return reportCancellation(result);
        }
        if (result && result.errors && result.errors.some(error =>
          typeof error === 'string' && error.includes('timed out')
        )) {
          this.reset();
        }
        const fatalWorkerError = result && result.errors && result.errors.some(error =>
          errorText(error).includes('Stack overflow')
        );
        const message = result.ok
          ? 'JSCoq passed.'
          : `JSCoq failed:\n${formatErrors(result.errors)}`;
        console.log(
          '[Hazel JSCoq]',
          message,
          {durationMs: Math.round(performance.now() - startedAt), stats: this.stats()},
        );
        if (request) request.settled = true;
        callback(result.ok ? 'ok' : 'error', message);
        if (fatalWorkerError) {
          // A Rocq stack overflow can poison later checks in the persistent
          // document. Retire it after delivering this result.
          this.reset();
        }
        return result;
      });
    })
      .catch(error => {
        if (request && request.cancelled) {
          return reportCancellation({
            ok: false,
            errors: [request.cancelReason || 'JSCoq check was cancelled.'],
            cancelled: true,
          });
        }
        const message = `JSCoq check failed to run: ${error && error.message ? error.message : String(error)}`;
        console.error('[Hazel JSCoq]', message, error);
        this.reset();
        callback('error', message);
        return {ok: false, errors: [error]};
      });
  },

  searchAndReport(code, callback, opts = {}) {
    const kind = code.includes('hazel_profile_search')
      ? 'profile search'
      : 'equivalence fallback';
    console.log('[Hazel Rocq debug] generated program', {
      kind,
      codeBytes: code.length,
    });
    const requestId = opts.requestId;
    const request = requestId == null
      ? null
      : {
          id: requestId,
          callback,
          activeCheckId: null,
          cancelled: false,
          settled: false,
        };
    if (request) {
      this.cancelSearch(requestId, 'A newer Rocq search replaced this request.');
      this.searchRequests.set(requestId, request);
    }
    const finishRequest = promise => promise.finally(() => {
      if (!request) return;
      request.settled = true;
      if (this.searchRequests.get(requestId) === request) {
        this.searchRequests.delete(requestId);
      }
    });

    if (this.debugSkipRocqSearch) {
      const record = {
        kind,
        code,
        capturedAt: new Date().toISOString(),
      };
      this.debugPrograms.push(record);
      const message =
        `JSCoq execution disabled for debugging; captured ${kind} program in ` +
        'window.HazelJSCoq.debugPrograms.';
      console.warn('[Hazel Rocq debug]', message, record);
      const result = {
        ok: false,
        steps: 0,
        errors: [message],
        debugSkipped: true,
      };
      return finishRequest(Promise.resolve().then(() => {
        callback('error', message);
        return result;
      }));
    }

    // Warmup itself uses `check`, so it temporarily appears in activeChecks.
    // Do not mistake that background preload for an older user validation.
    const runWhenIdle = this.warmupSearch()
    .then(() => this.waitForActiveChecks({request}))
    .then(waitResult => {
      if (waitResult.cancelled) {
        return this.checkAndReport(
          code,
          callback,
          {show: false, ...opts, request},
        );
      }
      if (!waitResult.idle) {
        const message = 'The previous JSCoq check did not stop in time.';
        console.warn('[Hazel JSCoq]', message, this.stats());
        if (!request || !request.callbackDelivered) {
          if (request) request.callbackDelivered = true;
          callback('error', message);
        }
        return {ok: false, steps: 0, errors: [message]};
      }
      console.log('[Hazel JSCoq] running Rocq tactic search candidate');
      return this.recycleWarmManagerIfNeeded({request}).then(() =>
        this.checkAndReport(
          code,
          callback,
          {show: false, ...opts, request},
        )
      );
    });
    return finishRequest(runWhenIdle);
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
