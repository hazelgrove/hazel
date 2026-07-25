open Util;
open ProjectorBase;
open Language;
open IdTagged.FreshGrammar;

// The HTML projector: one projector, two commit modes, picked by what the
// projected expression's live value turns out to be.
//
// - Elm app — an (init, update, view, subs) 4-tuple — commits to STATE: the
//   app's model lives in the web-side AppStore, reached through AppBridge,
//   and a msg goes to update(msg, model). An app can sit anywhere in a
//   program (and, docked with Alt+S, anywhere on screen).
// - bare HTML commits to SYNTAX (update = apply): msgs are Html -> Html
//   transforms, and committing evaluates msg(model) and splices the result
//   back into the document via SetSyntax.
//
// Detection is two-phase, because `init` only ever sees pre-evaluation
// syntax: `init` is permissive and syntactic (anything that could evaluate
// to HTML or to an app), while `view` is authoritative and reads the live
// value recorded by this projector's probe (`dynamics = true`).

// Refs for resize drag state
let wrapper_ref: ref(option(Js_of_ocaml.Js.Unsafe.any)) = ref(None);
let resize_cols = ref(40);
let resize_rows = ref(12);
// Whether this drag gesture has dispatched its first (undoable) tick.
// First tick goes through `local` so undo restores the pre-drag size;
// later ticks stream through `local_quiet` (no undo entry per tick).
let resize_committed = ref(false);
// Pixel-per-char ratios computed on pointerdown, used during drag
let px_per_col = ref(10.0);
let px_per_row = ref(18.0);

// Checkpoint writes are debounced per app: a model is persisted once the app
// has been idle this long, never per message.
let checkpoint_delay_ms = 2000.0;
let checkpoint_timers: Hashtbl.t(Id.t, Js_of_ocaml.Dom_html.timeout_id) =
  Hashtbl.create(4);

/* (Re)arm the idle timer for `id`. When it fires, ask the store for a
 * checkpoint (None if the model holds closures) and, if it differs from
 * what the projector model already carries, save it quietly — a checkpoint
 * is not an edit and must not land in the undo history. */
let schedule_checkpoint =
    (
      ~id: Id.t,
      ~current: option(string),
      ~save: option(string) => Ui_effect.t(unit),
    )
    : unit => {
  switch (Hashtbl.find_opt(checkpoint_timers, id)) {
  | Some(timer) => Js_of_ocaml.Dom_html.window##clearTimeout(timer)
  | None => ()
  };
  let timer =
    Js_of_ocaml.Dom_html.window##setTimeout(
      Js_of_ocaml.Js.wrap_callback(() => {
        Hashtbl.remove(checkpoint_timers, id);
        let checkpoint = AppBridge.checkpoint^(id);
        if (checkpoint != current) {
          Bonsai.Effect.Expert.handle(save(checkpoint));
        };
      }),
      checkpoint_delay_ms,
    );
  Hashtbl.replace(checkpoint_timers, id, timer);
};

/* The live value of the projected syntax, if the probe has recorded one.
 * Sample-selection rule: the latest sample by `seq`. For an app or an HTML
 * value that's the value produced by the most recent evaluation of this
 * expression — and the only sample at all, for the common case of an
 * expression evaluated once per run. */
let live_value = (info: ProjectorBase.info): option(DHExp.t) =>
  switch (info.dynamics) {
  | None => None
  | Some({samples, _}) =>
    List.fold_left(
      (acc, s: Sample.t) =>
        switch (acc) {
        | Some(best: Sample.t) when best.seq >= s.seq => acc
        | _ => Some(s)
        },
      None,
      samples,
    )
    |> Option.map((s: Sample.t) => MvuShape.strip_wrappers(s.value))
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type ui_state = {
    cols: int,
    rows: int,
  };

  let default_ui: ui_state = {
    cols: 40,
    rows: 12,
  };

  // Shadow derived deserializers to handle migration from old format
  let ui_state_of_sexp = sexp =>
    try(ui_state_of_sexp(sexp)) {
    | _ => default_ui
    };
  let ui_state_of_yojson = json =>
    try(ui_state_of_yojson(json)) {
    | _ => default_ui
    };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    exp: Grammar.exp_t(IdTagged.IdTag.t),
    ui: ui_state,
    /* State-commit mode only: the app model, serialized, when it is
       closure-free. Defaulted so models persisted before checkpoints
       existed still load. */
    [@sexp.default None] [@yojson.default None]
    checkpoint: option(string),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetDimensions(int, int)
    | ResetSize
    | SetCheckpoint(option(string));

  /* Permissive and syntactic: accept bare HTML, plus anything that could
     evaluate to an app. `view` has the live value and makes the call. */
  let init = (any: Any.t) => {
    let accept = exp =>
      Some({
        exp,
        ui: default_ui,
        checkpoint: None,
      });
    switch (any) {
    // HTML constructor applied to arguments: Div(...), Button(...), etc.
    | Exp({term: Ap(_, {term: Constructor(name, _), _}, _), _} as exp)
        when MvuShape.is_html_constructor(name) =>
      accept(exp)
    // Nullary HTML constructor: Br
    | Exp({term: Constructor("Br", _), _} as exp) => accept(exp)
    // A literal 4-tuple, or an expression whose value we can't know yet
    | Exp({term: Tuple([_, _, _, _]), _} as exp)
    | Exp({term: Parens({term: Tuple([_, _, _, _]), _}), _} as exp)
    | Exp({term: Var(_), _} as exp)
    | Exp({term: Ap(_), _} as exp) => accept(exp)
    | _ => None
    };
  };

  let focusable = Focusable.non;
  /* Instruments this projector with a probe, so `info.dynamics` carries the
     live value of the syntax it replaces (see live_value). */
  let dynamics = true;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;

  let placeholder = (m: model, _) =>
    ProjectorCore.Shape.{
      horizontal: m.ui.cols,
      vertical: Block(m.ui.rows - 1),
    };

  let update = (m: model, _, action: action) => {
    switch (action) {
    | SetDimensions(cols, rows) => {
        ...m,
        ui: {
          cols: max(8, cols),
          rows: max(3, rows),
        },
      }
    | ResetSize => {
        ...m,
        ui: default_ui,
      }
    | SetCheckpoint(checkpoint) => {
        ...m,
        checkpoint,
      }
    };
  };

  let view =
      (
        {model, info, parent, local, local_quiet, view_seg, _}:
          View.args(model, action),
      ) => {
    open Virtual_dom.Vdom;

    // Get current expression from syntax or fall back to model
    let current_exp =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(Exp(term)) => term
      | _ => model.exp
      };

    // Splice a new expression into the underlying syntax
    let set_syntax = (new_exp: DHExp.t) =>
      parent(
        SetSyntax(Exp(new_exp) |> info.utility.term_to_seg(~inline=true)),
      );

    // Commit a msg: evaluate msg(model) and splice the result. A
    // (Html, Cmd) result also runs the Cmd — Delay msgs are transforms
    // too, so they re-enter through this same inject.
    let rec inject_msg = (msg: DHExp.t): Ui_effect.t(unit) =>
      switch (MvuShape.safe_evaluate(Exp.ap(Forward, msg, current_exp))) {
      | Error(err) =>
        prerr_endline("HTMLProj: msg eval error: " ++ err);
        Effect.Ignore;
      | Ok(result) =>
        switch (MvuShape.strip_wrappers(result).term) {
        | Tuple([new_exp, cmd]) =>
          let cmd_ctx: CmdRunner.context = {inject: inject_msg};
          Effect.Many([set_syntax(new_exp), CmdRunner.run(cmd_ctx, cmd)]);
        | _ => set_syntax(result)
        }
      };

    // Unknown terms fall back to an embedded read-only syntax view
    let view_term = term =>
      Exp(term)
      |> info.utility.term_to_seg(~inline=true)
      |> view_seg(~background=false, Exp);

    let syntax_seed: HazelDOM.t = {
      inject: inject_msg,
      view_term,
      commit: HazelDOM.Syntax,
    };

    let message = (text: string) =>
      Node.div(
        ~attrs=[Attr.classes(["html-proj-message"])],
        [Node.text(text)],
      );

    /* State commit. The store owns the model; we hand it the evaluated app
       (plus any checkpoint to restore from) and render whatever html it
       currently holds. `ensure_app` is a no-op once the entry is current,
       which is what keeps this render-time call cheap. */
    let app_content = (app: DHExp.t) => {
      AppBridge.ensure_app^(info.id, app, model.checkpoint);
      switch (AppBridge.current_html^(info.id)) {
      | None => message("starting app…")
      | Some(html) =>
        let seed: HazelDOM.t = {
          inject: msg => {
            schedule_checkpoint(
              ~id=info.id, ~current=model.checkpoint, ~save=c =>
              local_quiet(SetCheckpoint(c))
            );
            AppBridge.dispatch^(info.id, msg);
          },
          view_term,
          commit: HazelDOM.State,
        };
        HazelDOM.go(seed, html);
      };
    };

    /* What this projector is showing is decided by the live value: an app
       renders from the store, HTML renders (and edits) the syntax, and
       anything else says so instead of dumping a term. Without a value
       (dynamics off, or not evaluated yet) we fall back to the syntax,
       which is exactly the pre-app behavior for bare HTML. */
    let content =
      switch (live_value(info)) {
      | Some(value) when Option.is_some(MvuShape.detect_app_kind(value)) =>
        app_content(value)
      | Some(value)
          when !MvuShape.is_html(value) && !MvuShape.is_html(current_exp) =>
        message("not an HTML value or app")
      | None when !MvuShape.is_html(current_exp) => message("no value yet")
      | _ => HazelDOM.go(syntax_seed, current_exp)
      };

    // Corner resize handle with pointer capture for drag.
    // On pointerdown: compute px-per-char ratios from the .projector container.
    // On mousemove: convert cursor position to char units; dispatch when changed.
    // The framework handles visual resizing via placeholder recomputation.
    let resize_handle =
      Node.div(
        ~attrs=[
          Attr.classes(["html-proj-resize-handle"]),
          Attr.on_pointerdown(
            (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
            resize_cols := model.ui.cols;
            resize_rows := model.ui.rows;
            resize_committed := false;
            let target =
              evt##.currentTarget
              |> Js_of_ocaml.Js.Opt.get(_, _ => failwith("no target"));
            JsUtil.setPointerCapture(target, evt##.pointerId);
            // wrapper = .html-proj-wrapper, container = .projector
            let wrapper = Js_of_ocaml.Js.Unsafe.coerce(target)##.parentNode;
            wrapper_ref := Some(wrapper);
            let container = wrapper##.parentNode;
            let cw: float = max(1.0, float_of_int(container##.offsetWidth));
            let ch: float = max(1.0, float_of_int(container##.offsetHeight));
            px_per_col := cw /. float_of_int(model.ui.cols);
            px_per_row := ch /. float_of_int(model.ui.rows);
            Effect.Ignore;
          }),
          Attr.on_mousemove(evt => {
            switch (wrapper_ref^) {
            | Some(wrapper) =>
              let container =
                Js_of_ocaml.Js.Unsafe.coerce(wrapper)##.parentNode;
              let rect = container##getBoundingClientRect();
              let left: float = rect##.left;
              let top: float = rect##.top;
              let e = Js_of_ocaml.Js.Unsafe.coerce(evt);
              let client_x: float = float_of_int(e##.clientX);
              let client_y: float = float_of_int(e##.clientY);
              let new_cols =
                max(
                  8,
                  int_of_float(floor((client_x -. left) /. px_per_col^)),
                );
              let new_rows =
                max(
                  3,
                  int_of_float(floor((client_y -. top) /. px_per_row^)),
                );
              if (new_cols != resize_cols^ || new_rows != resize_rows^) {
                resize_cols := new_cols;
                resize_rows := new_rows;
                if (resize_committed^) {
                  local_quiet(SetDimensions(new_cols, new_rows));
                } else {
                  resize_committed := true;
                  local(SetDimensions(new_cols, new_rows));
                };
              } else {
                Effect.Ignore;
              };
            | None => Effect.Ignore
            }
          }),
          Attr.on_pointerup(
            (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
            let target =
              evt##.currentTarget
              |> Js_of_ocaml.Js.Opt.get(_, _ => failwith("no target"));
            if (JsUtil.hasPointerCapture(target, evt##.pointerId)) {
              JsUtil.releasePointerCapture(target, evt##.pointerId);
            };
            wrapper_ref := None;
            // Final dispatch in case last mousemove was skipped; quiet so
            // it doesn't add a duplicate undo entry at the gesture's end
            local_quiet(SetDimensions(resize_cols^, resize_rows^));
          }),
        ],
        [],
      );

    let wrapper_classes = ["html-proj-wrapper"];
    let wrapped =
      Node.div(
        ~attrs=[Attr.classes(wrapper_classes)],
        [content, resize_handle],
      );

    View.mk(wrapped);
  };
};
