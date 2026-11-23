open Js_of_ocaml;

/* Gives the checker Z3 (`Language.Z3.eval`) in this JS context. Z3 is a
 * large download, so none of it is loaded before the first script is
 * evaluated: a program with no `algebra` step never loads it.
 *
 * Z3's scripts define the global `initZ3`, which starts an instance whose
 * `eval` runs one script. */

type instance = Js.Unsafe.any;

let init = (): option(Js.Unsafe.any) => {
  let init = Js.Unsafe.global##.initZ3;
  String.equal(Js.to_string(Js.typeof(init)), "undefined")
    ? None : Some(Js.Unsafe.fun_call(init, [||]));
};

/* A script that exhausts Z3's memory ends the instance with a JS error.
 * `on_failure` lets the caller drop the instance, and the script has no
 * answer. */
let eval =
    (~on_failure: unit => unit, z3: instance, script: string): option(string) =>
  try(
    Some(
      Js.to_string(
        Js.Unsafe.meth_call(
          z3,
          "eval",
          [|Js.Unsafe.inject(Js.string(script))|],
        ),
      ),
    )
  ) {
  | Js_error.Exn(_) =>
    on_failure();
    None;
  };

/* For a worker and for Node.js, where an instance starts synchronously.
 * `initZ3` comes from `script`, loaded at the first use, or else is
 * already a global of this context. */
let install = (~script: option(string)=?, ()): unit => {
  let script = ref(script);
  let z3: ref(option(instance)) = ref(None);
  Language.Z3.eval :=
    (
      input => {
        switch (script^) {
        | Some(url) =>
          script := None;
          Js.Unsafe.global##importScripts(Js.string(url));
        | None => ()
        };
        if (Option.is_none(z3^)) {
          z3 := init();
        };
        Option.bind(z3^, ~f=eval(~on_failure=() => z3 := None, _, input));
      }
    );
};

/* For the page, where an instance can only start through a promise. The
 * first use begins loading `script`; a script evaluated before the
 * instance is ready has no answer. */
module Page = {
  type state =
    | Unloaded
    | Loading
    | Ready(instance);

  let state = ref(Unloaded);

  /* Whether an answer that is missing now may be there shortly. */
  let loading = (): bool =>
    switch (state^) {
    | Loading => true
    | Unloaded
    | Ready(_) => false
    };

  let load = (script: string): unit => {
    state := Loading;
    let document = Js.Unsafe.global##.document;
    let element = document##createElement(Js.string("script"));
    element##.src := Js.string(script);
    element##.onload :=
      Js.wrap_callback(() =>
        switch (init()) {
        | None => state := Unloaded
        | Some(starting) =>
          let ready = Js.wrap_callback((z3: instance) => state := Ready(z3));
          let failed = Js.wrap_callback(() => state := Unloaded);
          ignore(
            Js.Unsafe.meth_call(
              starting,
              "then",
              [|Js.Unsafe.inject(ready), Js.Unsafe.inject(failed)|],
            ),
          );
        }
      );
    element##.onerror := Js.wrap_callback(() => state := Unloaded);
    document##.head##appendChild(element);
  };

  let install = (~script: string): unit =>
    Language.Z3.eval :=
      (
        input =>
          switch (state^) {
          | Unloaded =>
            load(script);
            None;
          | Loading => None
          | Ready(z3) => eval(~on_failure=() => state := Unloaded, z3, input)
          }
      );
};
