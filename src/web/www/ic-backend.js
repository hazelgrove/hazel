// The backend canister's HTTP calls, for HazelDB (src/web/HazelDB.re).
//
// Sent as text/plain, a "simple" request, so the browser needs no CORS
// preflight; the canister allows any origin. A failure is logged and not
// retried: stage 1 of the demo (docs/internet-computer-backend.md).
//
// SPACES: the canister keeps a space per front end or shared document set
// (fumola_canister: /s/<space>/kv ...). window.hazelSpace names this page's
// (none: the canister's default, Hazel's own); window.hazelSpaceKeys, the
// key prefixes kept there -- the rest stay in this browser's IndexedDB --
// or, unset, every key. A page's URL may choose both, for trying spaces
// out: ?space=alice&spaceKeys=doc:,scratch:
(function () {
  var q = new URLSearchParams(location.search);
  if (q.get("space")) window.hazelSpace = q.get("space");
  if (q.get("spaceKeys") !== null)
    window.hazelSpaceKeys = q.get("spaceKeys").split(",").filter(function (k) {
      return k.length > 0;
    });
})();

window.hazelBackendCall = function (method, path, body, onText) {
  var init = { method: method, headers: { "Content-Type": "text/plain" } };
  if (body !== null && body !== undefined) init.body = body;
  var base =
    window.hazelBackend +
    (window.hazelSpace ? "/s/" + encodeURIComponent(window.hazelSpace) : "");
  fetch(base + path, init)
    .then(function (r) {
      if (!r.ok) throw new Error(r.status + " " + r.statusText);
      return r.text();
    })
    .then(function (t) {
      if (onText) onText(t);
    })
    .catch(function (e) {
      console.error("hazel backend: " + method + " " + path + " failed:", e);
    });
};
