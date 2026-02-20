open Js_of_ocaml;

/* Typed js_of_ocaml bindings for the automerge repo and documents.
   - repo:    the document store; lives on globalThis.repo
   - handle:  a live reference to a document; supports .doc(),
              .on("change", cb), and .off("change", cb) */

// A document handle obtained after repo.find() resolves.
class type handle = {
  pub doc: Js.meth(Js.Unsafe.any);
  // handle##on_(event, callback): subscribe to document events.
  pub on_: (Js.t(Js.js_string), Js.Unsafe.any) => Js.meth(unit);
  // handle##off_(event, callback): unsubscribe from document events.
  pub off_: (Js.t(Js.js_string), Js.Unsafe.any) => Js.meth(unit);
};
// handle##doc: returns the current document state.

// The promise returned by repo.find(), resolving to a handle.
class type promise = {
  pub then_: Js.callback(Js.t(handle) => unit) => Js.meth(unit);
};

// The automerge-repo Repo instance (expected at globalThis.repo).
class type repo = {
  pub find: Js.t(Js.js_string) => Js.meth(Js.t(promise));
};
// repo##find(url): look up a document by its automerge URL.

// Retrieve the global Repo instance (globalThis.repo).
let get_repo = (): Js.t(repo) =>
  Js.Unsafe.coerce(Js.Unsafe.get(Js.Unsafe.global, "repo"));

// Call JSON.stringify on a JS value and return an OCaml string.
let json_stringify = (value: Js.Unsafe.any): string => {
  let json_obj = Js.Unsafe.get(Js.Unsafe.global, "JSON");
  Js.to_string(
    Js.Unsafe.meth_call(json_obj, "stringify", [|Js.Unsafe.inject(value)|]),
  );
};

// Read the current document from a handle, JSON-stringify it,
// and parse into a Hazel expression via the JsonADT codec.
let doc_to_exp = (handle: Js.t(handle)): result(Language.Exp.t, string) => {
  let doc = handle##doc;
  let json_str = json_stringify(doc);
  let yojson = Yojson.Safe.from_string(json_str);
  HazelProtocol.JsonADT.yojson_to_exp(yojson);
};
