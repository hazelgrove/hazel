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
  pub then_: Js.callback(Js.t(handle) => unit) => Js.meth(Js.Unsafe.any);
};

// The automerge-repo Repo instance (expected at globalThis.repo).
class type repo = {
  pub find: Js.t(Js.js_string) => Js.meth(Js.t(promise));
};
// repo##find(url): look up a document by its automerge URL.

// Retrieve the global Repo instance (globalThis.repo).
let get_repo = (): Js.t(repo) =>
  Js.Unsafe.coerce(Js.Unsafe.get(Js.Unsafe.global, "repo"));

// Check if the global Repo instance is available.
let repo_is_ready = (): bool =>
  Js.to_bool(
    Js.Unsafe.pure_js_expr("typeof globalThis.repo !== 'undefined'"),
  );

// Call JSON.stringify on a JS value and return an OCaml string.
let json_stringify = (value: Js.Unsafe.any): string => {
  let json_obj = Js.Unsafe.get(Js.Unsafe.global, "JSON");
  Js.to_string(
    Js.Unsafe.meth_call(json_obj, "stringify", [|Js.Unsafe.inject(value)|]),
  );
};

// Read the current document from a handle, JSON-stringify it,
// and parse into a Hazel expression via the JsonADT codec.
// Returns Error if the document is not yet available (null/undefined)
// or if JSON parsing/conversion fails.
let doc_to_exp = (handle: Js.t(handle)): result(Language.Exp.t, string) =>
  try({
    let doc = handle##doc;
    let json_str = json_stringify(doc);
    let yojson = Yojson.Safe.from_string(json_str);
    HazelProtocol.JsonADT.yojson_to_exp(yojson);
  }) {
  | exn => Error("Document not available: " ++ Printexc.to_string(exn))
  };

// Convert a Hazel expression (JSON ADT) to a JSON string.
// Inverse of the doc_to_exp path.
let exp_to_json_string = (exp: Language.Exp.t): result(string, string) =>
  switch (HazelProtocol.JsonADT.exp_to_yojson(exp)) {
  | Ok(yojson) => Ok(Yojson.Safe.to_string(yojson))
  | Error(msg) => Error(msg)
  };

// Call window.hazelWriteToDoc(url, json_string) to write
// data back to an automerge document. Fire-and-forget.
// Silently does nothing if the JS helper isn't loaded yet.
let write_to_doc = (url: string, json_string: string): unit => {
  let fn: Js.Optdef.t(Js.Unsafe.any) =
    Js.Unsafe.get(Js.Unsafe.global, "hazelWriteToDoc");
  if (Js.Optdef.test(fn)) {
    ignore(
      Js.Unsafe.fun_call(
        fn,
        [|
          Js.Unsafe.inject(Js.string(url)),
          Js.Unsafe.inject(Js.string(json_string)),
        |],
      ),
    );
  };
};
