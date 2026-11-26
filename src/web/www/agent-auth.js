/* Small browser-only credential store and OpenRouter PKCE flow. Kept outside
   editor state so exports, history, and Reset Hazel never contain/remove keys. */
(function (root) {
  'use strict';
  const STORAGE_KEY = 'hazel.openrouter.credential.v1';
  const OAUTH_KEY = 'hazel.openrouter.oauth.v1';
  const CALLBACK_PARAM = 'hazel_openrouter';
  const AUTH_URL = 'https://openrouter.ai/auth';
  const EXCHANGE_URL = 'https://openrouter.ai/api/v1/auth/keys';
  const MAX_AGE = 10 * 60 * 1000;

  function createAgentAuth(env) {
    const now = () => env.Date.now();
    const cleanKey = value => typeof value === 'string' ? value.trim() : '';
    function readRecord() {
      const raw = env.localStorage.getItem(STORAGE_KEY);
      if (raw === null) return null;
      const record = JSON.parse(raw);
      if (record.version !== 1 || typeof record.remember !== 'boolean' ||
          !(record.key === null || typeof record.key === 'string')) throw new Error('Invalid credential record');
      return record;
    }
    function saveBrowser(key, remember) {
      try {
        // Keep an opt-out marker so an old settings blob cannot resurrect a key.
        env.localStorage.setItem(STORAGE_KEY, JSON.stringify({
          version: 1, remember: !!remember, key: remember ? cleanKey(key) || null : null,
        }));
        return true;
      } catch { return false; }
    }
    function loadBrowser(legacyKey) {
      try {
        let record = readRecord();
        if (!record && cleanKey(legacyKey)) {
          if (!saveBrowser(legacyKey, true)) throw new Error('Migration failed');
          record = readRecord();
        }
        // The app has already loaded these legacy editor preferences; subsequent
        // settings saves go to IndexedDB with credentials stripped out.
        if (record) env.localStorage.removeItem('SETTINGS');
        return JSON.stringify({key: record?.remember ? cleanKey(record.key) || null : null,
          remember: record?.remember || false, error: false});
      } catch {
        return JSON.stringify({key: cleanKey(legacyKey) || null, remember: false, error: true});
      }
    }
    function clearEditorStorage() {
      // Do not clear-and-restore: that would briefly delete the credential and
      // could lose it if a write fails. Never touch the reserved entry.
      const store = env.localStorage;
      for (let i = store.length - 1; i >= 0; i--) {
        const key = store.key(i);
        if (key !== STORAGE_KEY) store.removeItem(key);
      }
    }
    const base64url = bytes => env.btoa(String.fromCharCode(...bytes))
      .replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
    const random = () => base64url(env.crypto.getRandomValues(new Uint8Array(32)));

    // Capture and scrub OAuth response parameters before the rest of Hazel loads.
    let callback = null;
    const current = new URL(env.location.href);
    if (current.searchParams.get(CALLBACK_PARAM) === '1') {
      callback = {code: current.searchParams.get('code'), state: current.searchParams.get('state'),
        denied: current.searchParams.has('error')};
      for (const key of [CALLBACK_PARAM, 'code', 'state', 'error', 'error_description']) current.searchParams.delete(key);
      env.history.replaceState(env.history.state, '', current.href);
    }

    async function startOAuth(rememberBrowser, rememberLocal) {
      const returnUrl = new URL(env.location.href);
      if (returnUrl.protocol !== 'https:' && !(returnUrl.protocol === 'http:' &&
          ['localhost', '127.0.0.1', '[::1]'].includes(returnUrl.hostname))) {
        throw new Error('Sign-in requires HTTPS or localhost.');
      }
      const verifier = random();
      const state = random();
      const digest = await env.crypto.subtle.digest('SHA-256', new TextEncoder().encode(verifier));
      returnUrl.searchParams.set(CALLBACK_PARAM, '1');
      const pending = {verifier, state, created: now(), returnUrl: returnUrl.href,
        rememberBrowser: !!rememberBrowser, rememberLocal: !!rememberLocal};
      env.sessionStorage.setItem(OAUTH_KEY, JSON.stringify(pending));
      const url = new URL(AUTH_URL);
      url.searchParams.set('callback_url', returnUrl.href);
      url.searchParams.set('code_challenge', base64url(new Uint8Array(digest)));
      url.searchParams.set('code_challenge_method', 'S256');
      url.searchParams.set('state', state);
      url.searchParams.set('key_label', 'Hazel');
      env.location.assign(url.href);
    }

    async function finishOAuth() {
      if (!callback) return null;
      const response = callback;
      callback = null; // Consume once, including failures.
      let pending;
      try {
        const raw = env.sessionStorage.getItem(OAUTH_KEY);
        env.sessionStorage.removeItem(OAUTH_KEY);
        pending = JSON.parse(raw);
      } catch { /* Report the same generic failure below. */ }
      const fail = error => ({key: null, remember_browser: false, remember_local: false, error});
      if (response.denied) return fail('OpenRouter connection was cancelled.');
      let expected;
      try { expected = pending && new URL(pending.returnUrl); } catch { pending = null; }
      if (!pending || typeof pending.verifier !== 'string' || !response.code ||
          !response.state || response.state !== pending.state ||
          !Number.isFinite(pending.created) || now() - pending.created < 0 || now() - pending.created > MAX_AGE ||
          expected.origin !== current.origin || expected.pathname !== current.pathname) {
        return fail('This sign-in attempt is missing or expired. Please connect again.');
      }
      const controller = new AbortController();
      const timer = env.setTimeout(() => controller.abort(), 15000);
      try {
        const result = await env.fetch(EXCHANGE_URL, {
          method: 'POST', headers: {'Content-Type': 'application/json'},
          credentials: 'omit', referrerPolicy: 'no-referrer', signal: controller.signal,
          body: JSON.stringify({code: response.code, code_verifier: pending.verifier, code_challenge_method: 'S256'}),
        });
        if (!result.ok) throw new Error('Exchange failed');
        const key = cleanKey((await result.json()).key);
        if (!key || key.length > 2048) throw new Error('Invalid key');
        return {key, remember_browser: pending.rememberBrowser === true,
          remember_local: pending.rememberLocal === true, error: null};
      } catch {
        return fail('OpenRouter connection failed. Please connect again or enter a key manually.');
      } finally { env.clearTimeout(timer); }
    }
    return {storageKey: STORAGE_KEY, loadBrowser, saveBrowser, clearEditorStorage,
      startOAuth, finishOAuth,
      // Callback bridges keep promises and secret-bearing responses out of logs.
      begin: (browser, local, done) => startOAuth(browser, local).catch(() => done('Could not open OpenRouter. Allow browser storage and use HTTPS or localhost.')),
      finish: done => finishOAuth().then(value => done(JSON.stringify(value)))
        .catch(() => done(JSON.stringify({key:null, remember_browser:false, remember_local:false,
          error:'OpenRouter connection failed. Please connect again.'}))),
    };
  }
  if (typeof module !== 'undefined' && module.exports) module.exports = {createAgentAuth, STORAGE_KEY, OAUTH_KEY};
  if (root.document) root.hazelAgentAuth = createAgentAuth(root);
})(globalThis);
