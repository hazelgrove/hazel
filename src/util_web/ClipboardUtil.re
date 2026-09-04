/* SPIKE (wasm-eval-bench): split out of JsUtil so that JsUtil itself is
   js_of_ocaml-only and can live in the Bonsai-free [util] library. Only the
   pieces that need Virtual_dom (the shim node and the Effect-based clipboard
   API) are here. */
open Js_of_ocaml;
open Util;
open Virtual_dom.Vdom;

let clipboard_shim = {
  Node.textarea(~attrs=[Attr.id(JsUtil.clipboard_shim_id)], []);
};

let copy = JsUtil.copy;

/* Clipboard access as Effects. Both directions go through the async
   Clipboard API, because the editor's key handlers run with focus on a
   non-editable div and Firefox refuses to dispatch native copy/paste
   events there.

   Defined with Ui_effect.Define1 — the same mechanism Bonsai builds
   Effect.of_deferred_fun from — so callers compose these like any other
   Effect rather than side-effecting on their own and scheduling the
   result by hand. Both must be dispatched from an event handler: the
   Clipboard API only grants access under a user gesture. */
let has_clipboard_api = (): bool =>
  Js.to_bool(
    Js.Unsafe.fun_call(
      Js.Unsafe.pure_js_expr(
        "(function(){return typeof navigator.clipboard !== 'undefined';})",
      ),
      [||],
    ),
  );

module ClipboardHandler = {
  module Action = {
    type t(_) =
      | Read_text: t(string)
      | Write_text(string): t(unit);
  };
  let handle = (type a, action: Action.t(a), ~on_response: a => unit) =>
    switch (action) {
    | Read_text =>
      let cb = Js.wrap_callback(text => on_response(Js.to_string(text)));
      Js.Unsafe.fun_call(
        Js.Unsafe.pure_js_expr(
          "(function(cb){navigator.clipboard.readText().then(cb);})",
        ),
        [|Js.Unsafe.inject(cb)|],
      );
    | Write_text(str) =>
      /* Older browsers with no Clipboard API fall through to the
         execCommand shim. */
      if (has_clipboard_api()) {
        Js.Unsafe.fun_call(
          Js.Unsafe.pure_js_expr(
            "(function(s){navigator.clipboard.writeText(s);})",
          ),
          [|Js.Unsafe.inject(Js.string(str))|],
        );
      } else {
        copy(str);
      };
      on_response();
    };
};
module Clipboard = Ui_effect.Define1(ClipboardHandler);

let write_clipboard = (str: string): Effect.t(unit) =>
  Clipboard.inject(Write_text(str));

/* Never completes when the browser has no Clipboard API — there is no
   text to deliver, so there is no Paste to dispatch. */
let read_clipboard = (): Effect.t(string) =>
  has_clipboard_api() ? Clipboard.inject(Read_text) : Effect.never;
