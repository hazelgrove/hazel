open Virtual_dom.Vdom;
open Util;
open Language;
open IdTagged.FreshGrammar;

// CmdRunner: Interprets Hazel Cmd values as Ui_effect.t
//
// Cmd is a sum type defined in BuiltinsADT.re:
//   | CmdNone
//   | Batch(List(Cmd))
//   | Focus(String)
//   | Blur(String)
//   | ScrollIntoView(String)
//   | ScrollTo(String, Float, Float)
//   | CopyToClipboard(String)
//   | Delay(Float, Html -> Html)
//   | Log(String)

type context = {
  model: DHExp.t,
  inject: DHExp.t => Ui_effect.t(unit),
};

// Parse a constructor from a DHExp
let of_constructor = (d: DHExp.t): option((string, DHExp.t)) =>
  switch (d.term) {
  | Ap(Forward, {term: Constructor(name, _), _}, body) =>
    Some((name, body))
  | Constructor(name, _) =>
    Some((
      name,
      {
        ...d,
        term: Tuple([]),
      },
    ))
  | _ => None
  };

// Extract string from DHExp
let of_string = (d: DHExp.t): option(string) =>
  switch (d.term) {
  | Atom(String(s)) => Some(s)
  | Parens({term: Atom(String(s)), _}) => Some(s)
  | _ => None
  };

// Extract float from DHExp
let of_float = (d: DHExp.t): option(float) =>
  switch (d.term) {
  | Atom(Float(f)) => Some(f)
  | Parens({term: Atom(Float(f)), _}) => Some(f)
  | _ => None
  };

// Extract list from DHExp
let of_list = (d: DHExp.t): option(list(DHExp.t)) =>
  switch (d.term) {
  | ListLit(items) => Some(items)
  | Parens({term: ListLit(items), _}) => Some(items)
  | _ => None
  };

// Extract tuple components
let of_tuple = (d: DHExp.t): option(list(DHExp.t)) =>
  switch (d.term) {
  | Tuple(items) => Some(items)
  | Parens({term: Tuple(items), _}) => Some(items)
  | _ => None
  };

// Evaluate a Hazel expression
let evaluate = exp =>
  fst(
    Evaluator.evaluate(
      ~env=Builtins.env_init,
      fst(
        Elaborator.elaborate(
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp),
          exp,
        ),
      ),
    ),
  );

// Run a single command, returning an effect
let rec run = (ctx: context, cmd: DHExp.t): Ui_effect.t(unit) => {
  switch (of_constructor(cmd)) {
  | None =>
    Js_of_ocaml.Firebug.console##log("CmdRunner: not a constructor");
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
    | Some([ms_exp, transform]) =>
      switch (of_float(ms_exp)) {
      | Some(ms) =>
        // Schedule the transform to run after delay
        Effect.of_sync_fun(
          () => {
            let _ =
              Js_of_ocaml.Dom_html.window##setTimeout(
                Js_of_ocaml.Js.wrap_callback(() => {
                  let new_model =
                    evaluate(Exp.ap(Forward, transform, ctx.model));
                  // Inject the new model from the callback using Expert.handle
                  Bonsai.Effect.Expert.handle(ctx.inject(new_model));
                }),
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
    Js_of_ocaml.Firebug.console##log(
      Js_of_ocaml.Js.string("CmdRunner: unknown command: " ++ name),
    );
    Effect.Ignore;
  };
};
