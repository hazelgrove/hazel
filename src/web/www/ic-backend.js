// The backend canister's HTTP calls, for HazelDB (src/web/HazelDB.re).
//
// Sent as text/plain, a "simple" request, so the browser needs no CORS
// preflight; the canister allows any origin. A failure is logged and not
// retried: stage 1 of the demo (docs/internet-computer-backend.md).
window.hazelBackendCall = function (method, path, body, onText) {
  var init = { method: method, headers: { "Content-Type": "text/plain" } };
  if (body !== null && body !== undefined) init.body = body;
  fetch(window.hazelBackend + path, init)
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
