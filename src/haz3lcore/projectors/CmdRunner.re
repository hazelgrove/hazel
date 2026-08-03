open Virtual_dom.Vdom;
open Util;
open Language;
open IdTagged.FreshGrammar;
open MvuShape;

// CmdRunner: Interprets Hazel Cmd values as Ui_effect.t
//
// Cmd is a sum type defined in BuiltinsADT.re:
//   | CmdNone
//   | CmdBatch(List(Cmd))
//   | Focus(String)
//   | Blur(String)
//   | ScrollIntoView(String)
//   | ScrollTo(String, Float, Float)
//   | CopyToClipboard(String)
//   | Delay(Float, Msg)
//   | PlayTone(Float, Float)
//   | Say(String)
//   | Random(Float -> Msg)
//   | Log(String)
//
// Delay's payload is always a msg, dispatched via ctx.inject when the timer
// fires (in syntax-commit mode a msg is an Html -> Html transform, so Delay
// works there uniformly). Random draws here at the boundary and applies its
// handler to the draw, so evaluation itself stays deterministic; the
// handler's result is a msg, dispatched like Delay's.

type context = {inject: DHExp.t => Ui_effect.t(unit)};

// Run a single command, returning an effect
let rec run = (ctx: context, cmd: DHExp.t): Ui_effect.t(unit) => {
  switch (of_constructor_raw(cmd)) {
  | None =>
    prerr_endline("CmdRunner: not a Cmd constructor");
    Effect.Ignore;

  | Some(("CmdNone", _)) => Effect.Ignore

  | Some(("CmdBatch", body)) =>
    switch (of_list(body)) {
    | Some(cmds) => Effect.Many(List.map(run(ctx), cmds))
    | None => Effect.Ignore
    }

  | Some(("Focus", body)) =>
    switch (of_string(body)) {
    | Some(id) =>
      Effect.of_sync_fun(
        () => {
          switch (JsUtil.get_elem_by_id_opt(id)) {
          | Some(elem) => elem##focus
          | None => ()
          }
        },
        (),
      )
    | None => Effect.Ignore
    }

  | Some(("Blur", body)) =>
    switch (of_string(body)) {
    | Some(id) =>
      Effect.of_sync_fun(
        () => {
          switch (JsUtil.get_elem_by_id_opt(id)) {
          | Some(elem) => elem##blur
          | None => ()
          }
        },
        (),
      )
    | None => Effect.Ignore
    }

  | Some(("ScrollIntoView", body)) =>
    switch (of_string(body)) {
    | Some(id) =>
      Effect.of_sync_fun(
        () => {
          switch (JsUtil.get_elem_by_id_opt(id)) {
          | Some(elem) => elem##scrollIntoView(Js_of_ocaml.Js._true)
          | None => ()
          }
        },
        (),
      )
    | None => Effect.Ignore
    }

  | Some(("ScrollTo", body)) =>
    switch (of_tuple(body)) {
    | Some([id_exp, x_exp, y_exp]) =>
      switch (of_string(id_exp), of_float(x_exp), of_float(y_exp)) {
      | (Some(id), Some(x), Some(y)) =>
        Effect.of_sync_fun(
          () => {
            switch (JsUtil.get_elem_by_id_opt(id)) {
            | Some(elem) =>
              elem##.scrollLeft := int_of_float(x);
              elem##.scrollTop := int_of_float(y);
            | None => ()
            }
          },
          (),
        )
      | _ => Effect.Ignore
      }
    | _ => Effect.Ignore
    }

  | Some(("CopyToClipboard", body)) =>
    switch (of_string(body)) {
    | Some(text) => Effect.of_sync_fun(() => {JsUtil.copy(text)}, ())
    | None => Effect.Ignore
    }

  | Some(("Delay", body)) =>
    switch (of_tuple(body)) {
    | Some([ms_exp, msg]) =>
      switch (of_float(ms_exp)) {
      | Some(ms) =>
        // Schedule the msg after the delay
        Effect.of_sync_fun(
          () => {
            let _ =
              Js_of_ocaml.Dom_html.window##setTimeout(
                Js_of_ocaml.Js.wrap_callback(() =>
                  Bonsai.Effect.Expert.handle(ctx.inject(msg))
                ),
                ms,
              );
            ();
          },
          (),
        )
      | None => Effect.Ignore
      }
    | _ => Effect.Ignore
    }

  | Some(("PlayTone", body)) =>
    switch (of_tuple(body)) {
    | Some([freq_exp, ms_exp]) =>
      switch (of_float(freq_exp), of_float(ms_exp)) {
      | (Some(freq), Some(ms)) =>
        Effect.of_sync_fun(() => JsUtil.play_tone(~freq, ~ms), ())
      | _ => Effect.Ignore
      }
    | _ => Effect.Ignore
    }

  | Some(("Say", body)) =>
    switch (of_string(body)) {
    | Some(text) => Effect.of_sync_fun(() => JsUtil.say(text), ())
    | None => Effect.Ignore
    }

  | Some(("Random", handler)) =>
    Effect.of_sync_fun(
      () => {
        let draw = Js_of_ocaml.Js.math##random;
        switch (safe_evaluate(Exp.ap(Forward, handler, Exp.float(draw)))) {
        | Ok(msg) => Bonsai.Effect.Expert.handle(ctx.inject(msg))
        | Error(err) =>
          prerr_endline("CmdRunner: Random handler error: " ++ err)
        };
      },
      (),
    )

  | Some(("Log", body)) =>
    switch (of_string(body)) {
    | Some(msg) =>
      Effect.of_sync_fun(
        () => {Js_of_ocaml.Firebug.console##log(Js_of_ocaml.Js.string(msg))},
        (),
      )
    | None => Effect.Ignore
    }

  | Some((name, _)) =>
    prerr_endline("CmdRunner: unknown command: " ++ name);
    Effect.Ignore;
  };
};
