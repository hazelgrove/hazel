open Util;
open ProjectorBase;
open Language;
open IdTagged.FreshGrammar;

// The HTML projector: one projector, two commit modes, picked by what the
// projected expression's live value turns out to be.
//
// - Elm app — an (init, update, view, subs) 4-tuple — commits to STATE: the
//   app's model lives in the web-side AppStore, reached through AppBridge,
//   and a msg goes to update(model, msg). An app can sit anywhere in a
//   program (and, docked with Alt+S, anywhere on screen).
// - bare HTML commits to SYNTAX (update = apply): msgs are Html -> Html
//   transforms, and committing evaluates msg(model) and splices the result
//   back into the document via SetSyntax.
//
// Detection is two-phase, because `init` only ever sees pre-evaluation
// syntax: `init` is permissive and syntactic (anything that could evaluate
// to HTML or to an app), while `view` is authoritative and reads the live
// value recorded by this projector's probe (`dynamics = true`).

/* RESIZE DRAG. Size is `ui.cols` x `ui.rows` character cells, so a drag just
 * converts a pixel delta into a cell delta. Everything needed is sampled once
 * at pointerdown into `resize_anchor` and the new size is always
 * `anchor + round(delta / cell)` — nothing is re-read from the DOM mid-drag,
 * so a reflow caused by the resize can't move the frame of reference. */

/* Which of the two dimensions a given handle drives. */
type resize_axes = {
  horizontal: bool,
  vertical: bool,
};

type resize_anchor = {
  start_x: float,
  start_y: float,
  start_cols: int,
  start_rows: int,
  axes: resize_axes,
  /* The editor's true cell size, from FontMetrics — not a ratio derived
   * from the projector's own rendered box. */
  col_width: float,
  row_height: float,
};

/* Only one drag can be live at a time, so this state is global. */
let resize_anchor: ref(option(resize_anchor)) = ref(None);
let resize_cols = ref(40);
let resize_rows = ref(12);
// Whether this drag gesture has dispatched its first (undoable) tick.
// First tick goes through `local` so undo restores the pre-drag size;
// later ticks stream through `local_quiet` (no undo entry per tick).
let resize_committed = ref(false);

/* Cursor lock: while dragging, every element shows the drag cursor, so
 * passing over app content doesn't flicker back to a text or pointer
 * cursor. One class per axis pair; see proj-html.css. */
let drag_cursor_classes = [
  "html-proj-resizing-x",
  "html-proj-resizing-y",
  "html-proj-resizing-xy",
];

let set_drag_cursor = (cls: option(string)): unit => {
  let body = Js_of_ocaml.Dom_html.document##.body;
  List.iter(
    c => body##.classList##remove(Js_of_ocaml.Js.string(c)),
    drag_cursor_classes,
  );
  switch (cls) {
  | Some(c) => body##.classList##add(Js_of_ocaml.Js.string(c))
  | None => ()
  };
};

let drag_cursor_class = (axes: resize_axes): string =>
  switch (axes) {
  | {horizontal: true, vertical: true} => "html-proj-resizing-xy"
  | {horizontal: true, vertical: false} => "html-proj-resizing-x"
  | _ => "html-proj-resizing-y"
  };

/* Pointer position in client coordinates, as floats: the drag is
 * delta-based, so sub-pixel precision keeps it smooth under zoom. */
let client_pos = (evt): (float, float) => {
  let e = Js_of_ocaml.Js.Unsafe.coerce(evt);
  let x: float = e##.clientX;
  let y: float = e##.clientY;
  (x, y);
};

/* virtual_dom 0.16 has Attr.on_pointerdown/on_pointerup but no
 * on_pointermove. Assigning the IDL event-handler property is equivalent,
 * and being an ordinary vdom attr it is reapplied on every render, so the
 * handler never closes over a stale model. */
let on_pointer_prop =
    (
      name: string,
      handler:
        Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent) =>
        Virtual_dom.Vdom.Effect.t(unit),
    ) =>
  Virtual_dom.Vdom.Attr.property(
    name,
    Js_of_ocaml.Js.Unsafe.inject(
      Js_of_ocaml.Js.wrap_callback(evt =>
        Bonsai.Effect.Expert.handle(handler(evt))
      ),
    ),
  );

let target_of = evt =>
  evt##.currentTarget |> Js_of_ocaml.Js.Opt.get(_, _ => failwith("no target"));

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
  /* GUI state only. The projected expression is NOT kept here: it already
     lives in the projector's syntax, and a copy would go stale (nothing
     updates it) and would embed ids, which stops the projector round-tripping
     through text. */
  type model = {
    ui: ui_state,
    /* State-commit mode only: the app model, serialized, when it is
       checkpointable (MvuShape.is_checkpointable). Defaulted so models
       persisted before checkpoints existed still load. */
    [@sexp.default None] [@yojson.default None]
    checkpoint: option(string),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetDimensions(int, int)
    | ResetSize
    | SetCheckpoint(option(string));

  /* Permissive and syntactic: accept bare HTML, plus anything that could
     evaluate to an app. `view` has the live value and makes the call.
     Parenthesized syntax is unwrapped first — the auto-display path in
     ExpToSegment has to parenthesize a printed element to get the single
     piece a projector replaces, and a user can equally well point this at
     `(Div(...))`. */
  let init = (any: Any.t) => {
    let accept =
      Some({
        ui: default_ui,
        checkpoint: None,
      });
    let rec unwrap = (e: Exp.t): Exp.t =>
      switch (e.term) {
      | Parens(inner) => unwrap(inner)
      | _ => e
      };
    switch (any) {
    | Exp(e) =>
      switch (unwrap(e).term) {
      // HTML constructor applied to arguments: Div(...), Button(...), etc.
      | Ap(_, {term: Constructor(name, _), _}, _)
          when MvuShape.is_html_constructor(name) => accept
      // Nullary HTML constructor: Br
      | Constructor("Br", _) => accept
      // A literal 4-tuple, or an expression whose value we can't know yet
      | Tuple([_, _, _, _])
      | Var(_)
      | Ap(_) => accept
      | _ => None
      }
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
        {
          model,
          info,
          parent,
          local,
          local_quiet,
          view_seg,
          status,
          col_width,
          row_height,
          _,
        }:
          View.args(model, action),
      ) => {
    open Virtual_dom.Vdom;

    /* The projector's syntax is the only source for the expression. If it
       won't convert (mid-edit, malformed), there is nothing honest to render
       and the `content` switch below falls through to a message. */
    let current_exp: option(DHExp.t) =
      switch (info.syntax |> info.utility.seg_to_term) {
      | Some(Exp(term)) => Some(term)
      | _ => None
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
      switch (current_exp) {
      /* Syntax commit needs an expression to transform; without one there is
         nothing to apply the msg to. */
      | None => Effect.Ignore
      | Some(exp) =>
        switch (MvuShape.safe_evaluate(Exp.ap(Forward, msg, exp))) {
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
    let syntax_is_html =
      switch (current_exp) {
      | Some(exp) => MvuShape.is_html(exp)
      | None => false
      };
    let content =
      switch (live_value(info), current_exp) {
      | (Some(value), _)
          when Option.is_some(MvuShape.detect_app_kind(value)) =>
        app_content(value)
      | (Some(value), _) when !MvuShape.is_html(value) && !syntax_is_html =>
        message("not an HTML value or app")
      | (None, _) when !syntax_is_html => message("no value yet")
      | (_, Some(exp)) => HazelDOM.go(syntax_seed, exp)
      | (_, None) => message("no value yet")
      };

    /* One drag tick: the size is always measured from the anchor, never
       from the previous tick or from the element's current geometry. */
    let resize_move = evt =>
      switch (resize_anchor^) {
      | None => Effect.Ignore
      | Some(anchor) =>
        let (x, y) = client_pos(evt);
        let delta = (d: float, cell: float) =>
          int_of_float(Float.round(d /. cell));
        let cols =
          anchor.axes.horizontal
            ? max(
                8,
                anchor.start_cols
                + delta(x -. anchor.start_x, anchor.col_width),
              )
            : anchor.start_cols;
        let rows =
          anchor.axes.vertical
            ? max(
                3,
                anchor.start_rows
                + delta(y -. anchor.start_y, anchor.row_height),
              )
            : anchor.start_rows;
        if (cols != resize_cols^ || rows != resize_rows^) {
          resize_cols := cols;
          resize_rows := rows;
          if (resize_committed^) {
            local_quiet(SetDimensions(cols, rows));
          } else {
            resize_committed := true;
            local(SetDimensions(cols, rows));
          };
        } else {
          Effect.Ignore;
        };
      };

    /* Ends the gesture, from pointerup or from losing capture. Idempotent:
       releasing capture in pointerup fires lostpointercapture right after. */
    let resize_end = evt =>
      switch (resize_anchor^) {
      | None => Effect.Ignore
      | Some(_) =>
        let target = target_of(evt);
        if (JsUtil.hasPointerCapture(target, evt##.pointerId)) {
          JsUtil.releasePointerCapture(target, evt##.pointerId);
        };
        let committed = resize_committed^;
        let (cols, rows) = (resize_cols^, resize_rows^);
        resize_anchor := None;
        resize_committed := false;
        set_drag_cursor(None);
        /* Settle on the last computed size, in case a move was dropped.
           Quiet, so the gesture stays one undo step. */
        committed ? local_quiet(SetDimensions(cols, rows)) : Effect.Ignore;
      };

    let resize_handle = (~clss: list(string), axes: resize_axes) =>
      Node.div(
        ~attrs=[
          Attr.classes(["html-proj-resize", ...clss]),
          Attr.on_pointerdown(
            (evt: Js_of_ocaml.Js.t(Js_of_ocaml.Dom_html.pointerEvent)) => {
            let target = target_of(evt);
            JsUtil.setPointerCapture(target, evt##.pointerId);
            let (x, y) = client_pos(evt);
            resize_anchor :=
              Some({
                start_x: x,
                start_y: y,
                start_cols: model.ui.cols,
                start_rows: model.ui.rows,
                axes,
                col_width: Float.max(1.0, col_width),
                row_height: Float.max(1.0, row_height),
              });
            resize_cols := model.ui.cols;
            resize_rows := model.ui.rows;
            resize_committed := false;
            set_drag_cursor(Some(drag_cursor_class(axes)));
            /* Don't let the grab start a text selection in the app below */
            Effect.Prevent_default;
          }),
          on_pointer_prop("onpointermove", resize_move),
          on_pointer_prop("onlostpointercapture", resize_end),
          Attr.on_pointerup(resize_end),
        ],
        [],
      );

    /* Inline, the projector owns both dimensions: right edge, bottom edge,
       and corner. Docked, the panel owns the width, so only the bottom
       edge is grabbable. */
    let inline_placement = status.placement == ProjectorCore.Placement.Inline;
    let handles =
      (
        inline_placement
          ? [
            resize_handle(
              ~clss=["edge-x"],
              {
                horizontal: true,
                vertical: false,
              },
            ),
          ]
          : []
      )
      @ [
        resize_handle(
          ~clss=["edge-y"],
          {
            horizontal: false,
            vertical: true,
          },
        ),
      ]
      @ (
        inline_placement
          ? [
            resize_handle(
              ~clss=["corner"],
              {
                horizontal: true,
                vertical: true,
              },
            ),
          ]
          : []
      );

    /* The handles are siblings of the scrolling wrapper, not children of
       it: inside an `overflow: auto` box they would anchor to the content,
       not to the visible edge, and drift as the app content grew. */
    let wrapped =
      Node.div(~attrs=[Attr.classes(["html-proj-wrapper"])], [content]);
    let frame =
      Node.div(
        ~attrs=[
          Attr.classes(
            ["html-proj-frame", "resizable-y"]
            @ (inline_placement ? ["resizable-x"] : []),
          ),
        ],
        [wrapped, ...handles],
      );

    View.mk(frame);
  };
};
