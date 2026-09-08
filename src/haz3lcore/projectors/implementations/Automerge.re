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
    HazelJson.JsonADT.yojson_to_exp(yojson);
  }) {
  | exn => Error("Document not available: " ++ Printexc.to_string(exn))
  };

// Convert a Hazel expression (JSON ADT) to a JSON string.
// Inverse of the doc_to_exp path.
let exp_to_json_string = (exp: Language.Exp.t): result(string, string) =>
  switch (HazelJson.JsonADT.exp_to_yojson(exp)) {
  | Ok(yojson) => Ok(Yojson.Safe.to_string(yojson))
  | Error(msg) => Error(msg)
  };

/* Capture the patchwork-view element at module load time. Guarded for
   non-browser hosts (Node test runner) where document is undefined. */
let captured_patchwork_view: option(Js.Unsafe.any) = {
  let doc = Js.Unsafe.get(Js.Unsafe.global, "document");
  if (!Js.Optdef.test(doc)) {
    None;
  } else {
    let cs = Js.Unsafe.get(doc, "currentScript");
    if (Js.Opt.test(cs)) {
      let pv =
        Js.Unsafe.meth_call(
          cs,
          "closest",
          [|Js.Unsafe.inject(Js.string("patchwork-view"))|],
        );
      if (Js.Opt.test(pv)) {
        Some(pv);
      } else {
        None;
      };
    } else {
      None;
    };
  };
};

/* Find the repo instance: prefer element.repo (from the patchwork-view
   element), then fall back to window.repo */
let find_repo = (): option(Js.t(repo)) => {
  switch (captured_patchwork_view) {
  | Some(el) =>
    let r = Js.Unsafe.get(el, "repo");
    if (Js.Optdef.test(r)) {
      Some(Js.Unsafe.coerce(r));
    } else if (Js.Optdef.test(Js.Unsafe.global##.repo)) {
      Some(get_repo());
    } else {
      None;
    };
  | None =>
    if (Js.Optdef.test(Js.Unsafe.global##.repo)) {
      Some(get_repo());
    } else {
      None;
    }
  };
};

/* Write JSON data to an automerge document.
   When running in an iframe (patchworkWriteToDoc injected by hazel-tool.js),
   delegates to the parent-realm helper to avoid cross-realm object issues.
   Falls back to direct repo write for standalone mode. */
let write_to_doc = (url: string, json_string: string): unit => {
  /* Prefer the parent-realm helper (avoids cross-realm proxy errors) */
  let helper = Js.Unsafe.get(Js.Unsafe.global, "patchworkWriteToDoc");
  if (Js.Optdef.test(helper) && Js.typeof(helper) == Js.string("function")) {
    Js.Unsafe.fun_call(
      helper,
      [|
        Js.Unsafe.inject(Js.string(url)),
        Js.Unsafe.inject(Js.string(json_string)),
      |],
    );
  } else {
    /* Standalone fallback: write directly via repo */
    switch (find_repo()) {
    | None => ()
    | Some(repo) =>
      let promise: Js.t(promise) = repo##find(Js.string(url));
      ignore(
        promise##then_(
          Js.wrap_callback((handle: Js.t(handle)) => {
            let parsed =
              Js.Unsafe.meth_call(
                Js.Unsafe.global##._JSON,
                "parse",
                [|Js.Unsafe.inject(Js.string(json_string))|],
              );
            Js.Unsafe.meth_call(
              handle,
              "change",
              [|
                Js.Unsafe.inject(
                  Js.wrap_callback((doc: Js.Unsafe.any) =>
                    if (Js.typeof(parsed) == Js.string("object")
                        && !Js.equals(parsed, Js.Unsafe.inject(Js.null))) {
                      let doc_keys =
                        Js.to_array(
                          Js.Unsafe.meth_call(
                            Js.Unsafe.global##._Object,
                            "keys",
                            [|Js.Unsafe.inject(doc)|],
                          ),
                        );
                      Array.iter(
                        key => {
                          let k = Js.to_string(key);
                          if (!
                                Js.to_bool(
                                  Js.Unsafe.meth_call(
                                    parsed,
                                    "hasOwnProperty",
                                    [|Js.Unsafe.inject(key)|],
                                  ),
                                )) {
                            Js.Unsafe.delete(doc, Js.string(k));
                          };
                        },
                        doc_keys,
                      );
                      let parsed_keys =
                        Js.to_array(
                          Js.Unsafe.meth_call(
                            Js.Unsafe.global##._Object,
                            "keys",
                            [|Js.Unsafe.inject(parsed)|],
                          ),
                        );
                      Array.iter(
                        key => {
                          let k = Js.to_string(key);
                          Js.Unsafe.set(
                            doc,
                            Js.string(k),
                            Js.Unsafe.get(parsed, key),
                          );
                        },
                        parsed_keys,
                      );
                    }
                  ),
                ),
              |],
            )
            |> ignore;
          }),
        ),
      );
    };
  };
};
