import {test} from 'node:test';
import assert from 'node:assert/strict';
import {webcrypto, createHash} from 'node:crypto';
import auth from '../../src/web/www/agent-auth.js';
const {createAgentAuth, STORAGE_KEY, OAUTH_KEY} = auth;

function storage() {
  const data = new Map();
  return {get length() {return data.size;}, key: i => [...data.keys()][i],
    getItem: key => data.get(key) ?? null,
    setItem: (key,value) => data.set(key,String(value)), removeItem: key => data.delete(key)};
}
function env(href = 'https://hazel.org/build/agent-canvas/?demo=1#editor', stores = {}) {
  const e = {localStorage: stores.localStorage || storage(), sessionStorage: stores.sessionStorage || storage(),
    Date: {now: () => 1000000}, crypto: webcrypto, btoa: text => Buffer.from(text,'binary').toString('base64'),
    setTimeout, clearTimeout, requests: [],
    location: {href, assign: url => {e.destination = url;}},
    history: {state: {}, replaceState: (_state,_title,url) => {e.location.href = url;}},
    fetch: async (...args) => {e.requests.push(args);return {ok:true,json:async()=>({key:'oauth-test-key'})};},
  };
  return e;
}
async function returning(options = [true,false]) {
  const original = env();
  await createAgentAuth(original).startOAuth(...options);
  const url = new URL(original.destination);
  const callback = new URL(url.searchParams.get('callback_url'));
  callback.searchParams.set('code','test-code');
  callback.searchParams.set('state',url.searchParams.get('state'));
  return env(callback.href,original);
}

test('browser storage is opt-in and editor reset preserves its independent entry', () => {
  const e = env(); const api = createAgentAuth(e);
  assert.deepEqual(JSON.parse(api.loadBrowser('')), {key:null,remember:false,error:false});
  assert.equal(api.saveBrowser(' test-key ',true),true);
  e.localStorage.setItem('SETTINGS','old editor data'); e.localStorage.setItem('HAZEL_THEME','theme');
  api.clearEditorStorage();
  assert.equal(e.localStorage.length,1);
  assert.deepEqual(JSON.parse(api.loadBrowser('')), {key:'test-key',remember:true,error:false});
  assert.equal(api.saveBrowser('test-key',false),true);
  assert.deepEqual(JSON.parse(api.loadBrowser('stale-settings-key')), {key:null,remember:false,error:false});
  assert.equal(e.localStorage.getItem(STORAGE_KEY).includes('test-key'),false);
});
test('migration preserves an existing saved key and new storage takes precedence', () => {
  const e = env(); const api = createAgentAuth(e);
  e.localStorage.setItem('SETTINGS','legacy key and preferences');
  assert.deepEqual(JSON.parse(api.loadBrowser('legacy-key')), {key:'legacy-key',remember:true,error:false});
  assert.equal(e.localStorage.getItem('SETTINGS'),null);
  assert.equal(JSON.parse(api.loadBrowser('older-key')).key,'legacy-key');
});
test('unavailable or corrupt browser storage reports failure without claiming persistence', () => {
  const e = env(); const api = createAgentAuth(e);
  e.localStorage.setItem = () => {throw new Error('Quota exceeded');};
  assert.equal(api.saveBrowser('test-key',true),false);
  assert.deepEqual(JSON.parse(api.loadBrowser('legacy-key')), {key:'legacy-key',remember:false,error:true});
  const broken = env(); broken.localStorage.setItem(STORAGE_KEY,'broken');
  assert.equal(JSON.parse(createAgentAuth(broken).loadBrowser('')).error,true);
});
test('PKCE uses random state and verifier, S256, and the exact hosted return path', async () => {
  const e = env(); await createAgentAuth(e).startOAuth(true,false);
  const pending = JSON.parse(e.sessionStorage.getItem(OAUTH_KEY));
  const url = new URL(e.destination);
  assert.equal(url.origin,'https://openrouter.ai');
  assert.equal(url.pathname,'/auth');
  assert.equal(url.searchParams.get('code_challenge_method'),'S256');
  assert.equal(pending.verifier.length,43);
  assert.notEqual(pending.verifier,pending.state);
  assert.equal(url.searchParams.get('code_challenge'), createHash('sha256').update(pending.verifier).digest('base64url'));
  assert.equal(url.searchParams.get('state'),pending.state);
  assert.equal(url.searchParams.get('callback_url'),'https://hazel.org/build/agent-canvas/?demo=1&hazel_openrouter=1#editor');
  assert.equal(e.localStorage.getItem(STORAGE_KEY),null);
});
test('callback scrubs the URL, exchanges once, and returns both explicit storage choices', async () => {
  const e = await returning([false,true]); const api = createAgentAuth(e);
  assert.equal(e.location.href,'https://hazel.org/build/agent-canvas/?demo=1#editor');
  assert.deepEqual(await api.finishOAuth(),{key:'oauth-test-key',remember_browser:false,remember_local:true,error:null});
  assert.equal(e.requests.length,1);
  const [url, options] = e.requests[0];
  assert.equal(url,'https://openrouter.ai/api/v1/auth/keys');
  assert.equal(options.method,'POST'); assert.equal(options.credentials,'omit');
  assert.equal(JSON.parse(options.body).code_challenge_method,'S256');
  assert.equal(e.sessionStorage.getItem(OAUTH_KEY),null);
  assert.equal(e.localStorage.getItem(STORAGE_KEY),null);
  assert.equal(await api.finishOAuth(),null); assert.equal(e.requests.length,1);
});
for (const scenario of ['state','missing','expired','future','path','denied','malformed','malformed-return']) {
  test(`rejects ${scenario} OAuth callbacks before exchanging credentials`, async () => {
    const e = await returning(); const url = new URL(e.location.href);
    const pending = JSON.parse(e.sessionStorage.getItem(OAUTH_KEY));
    if (scenario === 'state') url.searchParams.set('state','wrong');
    if (scenario === 'missing') e.sessionStorage.removeItem(OAUTH_KEY);
    if (scenario === 'expired') {pending.created -= 600001; e.sessionStorage.setItem(OAUTH_KEY,JSON.stringify(pending));}
    if (scenario === 'future') {pending.created += 1; e.sessionStorage.setItem(OAUTH_KEY,JSON.stringify(pending));}
    if (scenario === 'path') url.pathname = '/other-build/';
    if (scenario === 'denied') url.searchParams.set('error','access_denied');
    if (scenario === 'malformed') e.sessionStorage.setItem(OAUTH_KEY,'broken');
    if (scenario === 'malformed-return') {pending.returnUrl = 'invalid'; e.sessionStorage.setItem(OAUTH_KEY,JSON.stringify(pending));}
    e.location.href = url.href;
    const result = await createAgentAuth(e).finishOAuth();
    assert.equal(result.key,null); assert.ok(result.error); assert.equal(e.requests.length,0);
    assert.equal(new URL(e.location.href).searchParams.has('code'),false);
    assert.equal(e.sessionStorage.getItem(OAUTH_KEY),null);
  });
}
test('exchange errors expose no server response and allow retry', async () => {
  const e = await returning(); e.fetch = async()=>({ok:false,json:async()=>({error:'provider diagnostic'})});
  const result = await createAgentAuth(e).finishOAuth();
  assert.equal(result.key,null); assert.match(result.error,/connect again/);
  assert.equal(result.error.includes('provider diagnostic'),false);
});
test('localhost supports arbitrary ports, while insecure hosted URLs are rejected', async () => {
  const e = env('http://localhost:8687/'); await createAgentAuth(e).startOAuth(false,true);
  assert.equal(new URL(new URL(e.destination).searchParams.get('callback_url')).port,'8687');
  await assert.rejects(createAgentAuth(env('http://hazel.org/')).startOAuth(true,false),/HTTPS/);
});
