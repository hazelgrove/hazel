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

// `space`, when given, names the space outright: a shared deck's
// (HazelDB.Backend.shared_decks), which every page uses whatever its own.
window.hazelBackendCall = function (method, path, body, onText, space) {
  var init = { method: method, headers: { "Content-Type": "text/plain" } };
  if (body !== null && body !== undefined) init.body = body;
  var named = typeof space === "string" ? space : window.hazelSpace;
  var base =
    window.hazelBackend + (named ? "/s/" + encodeURIComponent(named) : "");
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

// A Fumola instance that lives on the canister (FumolaRun.remote_reply):
// declare its mode if the program declares one, run the program there, and
// hand back the canister's reply, the same JSON a run in the page answers.
// Every reply -- an error included -- is announced as fumola-remote-reply,
// which re-runs the page's programs, so the one that asked now finds it.
window.hazelFumolaRemote = function (instance, mode, program, onText) {
  var post = function (op, body) {
    return fetch(
      window.hazelBackend + "/i/" + encodeURIComponent(instance) + "/" + op,
      { method: "POST", headers: { "Content-Type": "text/plain" }, body: body }
    ).then(function (r) {
      if (!r.ok) throw new Error(r.status + " " + r.statusText);
      return r.text();
    });
  };
  var answer = function (text) {
    onText(text);
    window.dispatchEvent(new Event("fumola-remote-reply"));
  };
  if (!window.hazelBackend) {
    answer(JSON.stringify({ ok: false, error: "no canister is configured for this page" }));
    return;
  }
  (mode ? post("ensure_mode", mode) : Promise.resolve())
    .then(function () {
      return post("eval_top", program);
    })
    .then(answer)
    .catch(function (e) {
      answer(JSON.stringify({ ok: false, error: "the canister did not answer: " + e.message }));
    });
};

// Asked of a canister instance on the side, not as its program
// (FumolaRun.remote_query): a watch pane reading the instance's history, a
// stats readout. Any op of POST /i/<instance>/<op>. The answer is announced
// as fumola-remote-query, which only redraws: nothing in the page's programs
// changed, so nothing is run again.
window.hazelFumolaRemoteQuery = function (instance, op, body, onText) {
  var answer = function (text) {
    onText(text);
    window.dispatchEvent(new Event("fumola-remote-query"));
  };
  if (!window.hazelBackend) {
    answer(JSON.stringify({ ok: false, error: "no canister is configured for this page" }));
    return;
  }
  fetch(window.hazelBackend + "/i/" + encodeURIComponent(instance) + "/" + op, {
    method: "POST",
    headers: { "Content-Type": "text/plain" },
    body: body,
  })
    .then(function (r) {
      if (!r.ok) throw new Error(r.status + " " + r.statusText);
      return r.text();
    })
    .then(answer)
    .catch(function (e) {
      answer(JSON.stringify({ ok: false, error: "the canister did not answer: " + e.message }));
    });
};

// GET /stats: the canister's heap, its store's DCG and each instance,
// counted from tallies the DCG keeps. A query, so it is quick and free.
window.hazelBackendStats = function (onText) {
  var answer = function (text) {
    onText(text);
    window.dispatchEvent(new Event("fumola-remote-query"));
  };
  if (!window.hazelBackend) {
    answer(JSON.stringify({ ok: false, error: "no canister is configured for this page" }));
    return;
  }
  fetch(window.hazelBackend + "/stats")
    .then(function (r) {
      if (!r.ok) throw new Error(r.status + " " + r.statusText);
      return r.text();
    })
    .then(answer)
    .catch(function (e) {
      answer(JSON.stringify({ ok: false, error: "the canister did not answer: " + e.message }));
    });
};
