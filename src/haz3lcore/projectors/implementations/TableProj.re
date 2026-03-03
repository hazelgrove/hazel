open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

open Js_of_ocaml;

let max_column_length = 12;

let rec extract_labeled_tuple_entries =
        (exp: Exp.t): option(list((LabeledTuple.label, DHExp.t))) => {
  switch (exp.term) {
  | Parens(e) => extract_labeled_tuple_entries(e)
  | Tuple(es) =>
    OptUtil.traverse(
      (e: Exp.t) => {
        switch (e.term) {
        | TupLabel({term: Label(l), _}, inner) => Some((l, inner))
        | _ => None
        }
      },
      es,
    )
  | _ => None
  };
};

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp({term: ListLit(es), _}) =>
    switch (
      OptUtil.traverse(
        e => extract_labeled_tuple_entries(e) |> Option.map(List.split),
        es,
      )
    ) {
    | Some(data: list((list(string), list(TermBase.exp_t)))) =>
      let (headers: list(list(string)), rows: list(list(TermBase.exp_t))) =
        List.split(data);

      // If all the headers aren't the same return None
      switch (headers) {
      | [] => None
      | [h, ..._] when List.for_all(x => x == h, headers) =>
        let headers = h;
        Some((headers, rows));

      | _ => None
      };
    | None => None
    }
  | _ => None
  };

let get =
    (info: info): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) => table_of(s)
  | None => None
  };

let len_seg = (utility: utility, seg: Segment.t): int =>
  seg |> utility.seg_to_string |> String.length;

let seg_of_exp = (utility: utility, exp: Exp.t): (Segment.t, int) => {
  let seg = utility.term_to_seg(~inline=true, Exp(exp));
  (seg, len_seg(utility, seg));
};

let abbreviated_seg_of =
    (utility: utility, available: int, exp: Exp.t): (Segment.t, int) => {
  let (abbr_exp, _length) =
    exp |> DHExp.strip_ascriptions |> Abbreviate.abbreviate_exp(~available);
  seg_of_exp(utility, abbr_exp);
};
let length_cls = (length: int): string =>
  if (length > 10) {
    "extra";
  } else if (length > 9) {
    "s6";
  } else if (length > 8) {
    "s5";
  } else if (length > 7) {
    "s4";
  } else if (length > 6) {
    "s3";
  } else if (length > 5) {
    "s2";
  } else if (length > 4) {
    "s1";
  } else {
    "s0";
  };
let value_view = (_info: info, utility: utility, view_seg, exp) => {
  let (seg, length) = abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(
    ~attrs=[Attr.classes(["value", length_cls(length)])],
    [view_seg(Sort.Exp, seg)],
  );
};

let table =
    (
      info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) =>
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([
        Node.tr(List.map(h => Node.th([Node.text(h)]), headers)),
      ]),
      Node.tbody(
        List.map(
          row =>
            Node.tr(
              List.map(
                e => Node.td([value_view(info, info.utility, view_seg, e)]),
                row,
              ),
            ),
          rows,
        ),
      ),
    ],
  );

/* Resize constants and helpers */
let min_width_blocks = 20;
let min_height_blocks = 1;

let col_width = () => 10.0;
let row_height = () => 10.0;

let clamp_width_blocks = (blocks: int): int => max(min_width_blocks, blocks);
let clamp_height_blocks = (blocks: int): int =>
  max(min_height_blocks, blocks);

let compute_default_size = (any: Any.t): option((int, int)) =>
  switch (table_of(any)) {
  | None => None
  | Some((header, rows)) =>
    let max_header_length =
      header |> List.map(String.length) |> List.fold_left((+), 0);
    let max_row_length =
      rows
      |> List.map(row =>
           row
           |> List.map(e =>
                Abbreviate.abbreviate_exp(~available=max_column_length, e)
                |> snd
              )
           |> List.fold_left((+), 0, _)
         )
      |> List.fold_left(max, 0, _);
    let max_length = max(max_header_length, max_row_length);
    let num_rows = List.length(rows);
    let num_cols = List.length(header);
    let width = 4 + max_length * 1 + num_cols * 2;
    let height = min(num_rows, 10);
    Some((clamp_width_blocks(width), clamp_height_blocks(height)));
  };

/* ResizeState: mutable refs for active drag state */
module ResizeState = {
  type t = {
    pointer_id: int,
    capture_target: Js.t(Dom_html.element),
    start_client_x: float,
    start_client_y: float,
    start_width_blocks: int,
    start_height_blocks: int,
  };

  let active: ref(option(t)) = ref(None);
  let last_sent: ref(option((int, int))) = ref(None);
  let dispatch: ref(option((int, int) => Effect.t(unit))) = ref(None);

  let reset = (): unit => {
    active := None;
    last_sent := None;
    dispatch := None;
  };
};

let resize_pointermove = (event: Js.t(Dom_html.pointerEvent)) => {
  switch (ResizeState.active^, ResizeState.dispatch^) {
  | (Some(state), Some(dispatch)) when state.pointer_id == event##.pointerId =>
    let delta_x = float_of_int(event##.clientX) -. state.start_client_x;
    let delta_y = float_of_int(event##.clientY) -. state.start_client_y;
    let start_width_f = float_of_int(state.start_width_blocks);
    let start_height_f = float_of_int(state.start_height_blocks);
    let desired_width = start_width_f +. delta_x /. col_width();
    let desired_height = start_height_f +. delta_y /. row_height();
    let new_width_blocks =
      if (desired_width >= start_width_f) {
        int_of_float(Float.ceil(desired_width));
      } else {
        int_of_float(Float.floor(desired_width));
      };
    let new_height_blocks =
      if (desired_height >= start_height_f) {
        int_of_float(Float.ceil(desired_height));
      } else {
        int_of_float(Float.floor(desired_height));
      };
    let clamped_width = clamp_width_blocks(new_width_blocks);
    let clamped_height = clamp_height_blocks(new_height_blocks);
    let pair = (clamped_width, clamped_height);
    if (ResizeState.last_sent^ == Some(pair)) {
      Effect.Ignore;
    } else {
      ResizeState.last_sent := Some(pair);
      Effect.Many([
        dispatch(clamped_width, clamped_height),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ]);
    };
  | _ => Effect.Ignore
  };
};

let finish_resize = (event: Js.t(Dom_html.pointerEvent)): Effect.t(unit) => {
  switch (ResizeState.active^) {
  | Some(state) when state.pointer_id == event##.pointerId =>
    if (JsUtil.hasPointerCapture(state.capture_target, state.pointer_id)) {
      JsUtil.releasePointerCapture(state.capture_target, state.pointer_id);
    };
    ResizeState.reset();
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
  | _ => Effect.Ignore
  };
};

let resize_listeners_attached = ref(false);

let setup_resize_listeners = (): unit =>
  if (! resize_listeners_attached^) {
    resize_listeners_attached := true;
    let dom_event_target: Js.t(Dom_html.eventTarget) =
      Js.Unsafe.coerce(Dom_html.document);
    let _ =
      Dom_html.addEventListener(
        dom_event_target,
        Dom_html.Event.make("pointermove"),
        Dom.full_handler((_, event) => {
          Virtual_dom.Vdom.Effect.Expert.handle(
            event,
            resize_pointermove(event),
          );
          Js._false;
        }),
        Js._false,
      );
    let _ =
      Dom_html.addEventListener(
        dom_event_target,
        Dom_html.Event.make("pointerup"),
        Dom.full_handler((_, event) => {
          Virtual_dom.Vdom.Effect.Expert.handle(event, finish_resize(event));
          Js._false;
        }),
        Js._false,
      );
    ();
  };

let resize_pointerdown =
    (
      ~dispatch: (int, int) => Effect.t(unit),
      ~width_blocks: int,
      ~height_blocks: int,
      event: Js.t(Dom_html.pointerEvent),
    ) =>
  if (!Js.to_bool(event##.metaKey)) {
    ResizeState.reset();
    Effect.Ignore;
  } else {
    setup_resize_listeners();
    let target = Js.Opt.get(event##.currentTarget, () => failwith("resize"));
    let element: Js.t(Dom_html.element) = Js.Unsafe.coerce(target);
    JsUtil.setPointerCapture(element, event##.pointerId);
    ResizeState.dispatch := Some(dispatch);
    ResizeState.active :=
      Some({
        pointer_id: event##.pointerId,
        capture_target: element,
        start_client_x: float_of_int(event##.clientX),
        start_client_y: float_of_int(event##.clientY),
        start_width_blocks: width_blocks,
        start_height_blocks: height_blocks,
      });
    ResizeState.last_sent := None;
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    width_blocks: int,
    height_blocks: int,
  };
  let default_model = {
    width_blocks: 40,
    height_blocks: 10,
  };
  let model_of_sexp = (sexp: Sexplib.Sexp.t): model =>
    switch (model_of_sexp(sexp)) {
    | exception _ => default_model
    | m => m
    };
  let model_of_yojson = (json: Yojson.Safe.t): model =>
    switch (model_of_yojson(json)) {
    | exception _ => default_model
    | m => m
    };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ResizeTo(int, int);

  let init = (any: Any.t) =>
    switch (compute_default_size(any)) {
    | Some((w, h)) =>
      Some({
        width_blocks: w,
        height_blocks: h,
      })
    | None => None
    };

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = false;
  let placeholder = (model, info) =>
    switch (get(info)) {
    | None =>
      ProjectorCore.Shape.{
        vertical: Inline,
        horizontal: 3,
      }
    | Some(_) =>
      ProjectorCore.Shape.{
        vertical: Block(model.height_blocks),
        horizontal: model.width_blocks,
      }
    };
  let update = (_model, info, action) =>
    switch (action) {
    | ResizeTo(w, h) =>
      let max_h =
        switch (get(info)) {
        | None => h
        | Some((_, rows)) => List.length(rows)
        };
      {
        width_blocks: clamp_width_blocks(w),
        height_blocks: clamp_height_blocks(min(h, max_h)),
      };
    };

  let view =
      ({model, info, local, parent, view_seg, _}: View.args(model, action))
      : View.t =>
    switch (get(info)) {
    | None =>
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [
            Node.div(
              ~attrs=[Attr.classes(["table", "error"])],
              [Node.text("\xe2\x9a\xa0")],
            ),
          ],
        ),
      )
    | Some(data) =>
      let dispatch = (w, h) => local(ResizeTo(w, h));
      let handle =
        Node.div(
          ~attrs=[
            Attr.classes(["table-resize-handle"]),
            Attr.on_pointerdown(
              resize_pointerdown(
                ~dispatch,
                ~width_blocks=model.width_blocks,
                ~height_blocks=model.height_blocks,
              ),
            ),
          ],
          [],
        );
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [table(info, ~view_seg, ~parent, data), handle],
        ),
      );
    };
};
