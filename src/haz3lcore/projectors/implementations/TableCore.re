open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open Js_of_ocaml;

/* TableCore: Shared table rendering and resize utilities for TableProj
   and TableRenderer. */

/* --- Cell Rendering --- */

let max_column_length = 12;

let value_view = (utility: utility, view_seg, exp) => {
  let (seg, _length) =
    ProbeUtil.abbreviated_seg_of(utility, max_column_length, exp);

  Node.div(~attrs=[Attr.classes(["value"])], [view_seg(Sort.Exp, seg)]);
};

/* --- Table Assembly --- */

let row_cells = (utility: utility, view_seg, row: list(Exp.t)): list(Node.t) =>
  List.map(e => Node.td([value_view(utility, view_seg, e)]), row);

let table_view =
    (~header_cells: list(Node.t), ~rows: list(list(Node.t))): Node.t =>
  Node.table(
    ~attrs=[Attr.classes(["table"])],
    [
      Node.thead([Node.tr(header_cells)]),
      Node.tbody(List.map(r => Node.tr(r), rows)),
    ],
  );

/* --- Table Parsing --- */

type table_data = (list(option(string)), list(list(Exp.t)));

let rec extract_entry = (e: Exp.t): option((option(string), Exp.t)) =>
  switch (e.term) {
  | Parens(inner) => extract_entry(inner)
  | TupLabel({term: Label(l), _}, v) => Some((Some(l), v))
  | TupLabel({term: EmptyHole, _}, v) => Some((None, v))
  | _ => None
  };

/* Peel Parens and push outer Asc wrappers into the tuple so labeled
 * entries surface in their normal shape. Revisit if elaboration changes
 * how it adds ascriptions to list rows. */
let rec normalize_row = (e: Exp.t): Exp.t =>
  switch (e.term) {
  | Parens(inner) => normalize_row(inner)
  | Asc(_, _) =>
    let stepped = Ascriptions.transition_multiple(e);
    stepped === e ? e : normalize_row(stepped);
  | _ => e
  };

/* --- Resize Machinery (shared between TableProj and TableRenderer) --- */

/* These are the px-per-block ratios used to translate pointermove deltas
   into block-count deltas during a drag. They're a heuristic — the actual
   rendered block size depends on font metrics, but the user's perception
   of "drag distance vs blocks added" tolerates a small mismatch and this
   keeps the drag handler font-independent. */
let drag_col_width_px = 10.0;
let drag_row_height_px = 10.0;

let min_width_blocks = 20;
let min_height_blocks = 1;

let clamp_width_blocks = (blocks: int): int => max(min_width_blocks, blocks);
let clamp_height_blocks = (blocks: int): int =>
  max(min_height_blocks, blocks);

/* Pick a sensible default size for a freshly-rendered table — wide enough
   to fit headers/values, tall enough to show every row (header + data)
   up to a soft cap. */
let default_size_for_table =
    (header: list(option(string)), rows: list(list(Exp.t))): (int, int) => {
  let header_chars =
    header
    |> List.map(h => Option.fold(~none=1, ~some=String.length, h))
    |> List.fold_left((+), 0);
  let widest_row =
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
  let max_length = max(header_chars, widest_row);
  let num_rows = List.length(rows);
  let num_cols = List.length(header);
  let header_rows = header == [] ? 0 : 1;
  let width = 4 + max_length + num_cols * 2;
  let height = min(num_rows + header_rows, 10);
  (clamp_width_blocks(width), clamp_height_blocks(height));
};

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
    let start_w = float_of_int(state.start_width_blocks);
    let start_h = float_of_int(state.start_height_blocks);
    let desired_w = start_w +. delta_x /. drag_col_width_px;
    let desired_h = start_h +. delta_y /. drag_row_height_px;
    let new_w =
      desired_w >= start_w
        ? int_of_float(Float.ceil(desired_w))
        : int_of_float(Float.floor(desired_w));
    let new_h =
      desired_h >= start_h
        ? int_of_float(Float.ceil(desired_h))
        : int_of_float(Float.floor(desired_h));
    let pair = (clamp_width_blocks(new_w), clamp_height_blocks(new_h));
    if (ResizeState.last_sent^ == Some(pair)) {
      Effect.Ignore;
    } else {
      ResizeState.last_sent := Some(pair);
      let (w, h) = pair;
      Effect.Many([
        dispatch(w, h),
        Effect.Stop_propagation,
        Effect.Prevent_default,
      ]);
    };
  | _ => Effect.Ignore
  };
};

let finish_resize = (event: Js.t(Dom_html.pointerEvent)): Effect.t(unit) =>
  switch (ResizeState.active^) {
  | Some(state) when state.pointer_id == event##.pointerId =>
    if (JsUtil.hasPointerCapture(state.capture_target, state.pointer_id)) {
      JsUtil.releasePointerCapture(state.capture_target, state.pointer_id);
    };
    ResizeState.reset();
    Effect.Many([Effect.Stop_propagation, Effect.Prevent_default]);
  | _ => Effect.Ignore
  };

let resize_listeners_attached = ref(false);

let setup_resize_listeners = (): unit =>
  if (! resize_listeners_attached^) {
    resize_listeners_attached := true;
    let target: Js.t(Dom_html.eventTarget) =
      Js.Unsafe.coerce(Dom_html.document);
    let _ =
      Dom_html.addEventListener(
        target,
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
        target,
        Dom_html.Event.make("pointerup"),
        Dom.full_handler((_, event) => {
          Virtual_dom.Vdom.Effect.Expert.handle(event, finish_resize(event));
          Js._false;
        }),
        Js._false,
      );
    ();
  };

/* Begin a resize drag. Gated on metaKey (Cmd on Mac, Win on Windows) so a
   plain click in the corner doesn't accidentally start a resize. */
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

/* Builds the bottom-right resize handle. */
let resize_handle =
    (
      ~dispatch: (int, int) => Effect.t(unit),
      ~width_blocks: int,
      ~height_blocks: int,
    )
    : Node.t =>
  Node.div(
    ~attrs=[
      Attr.classes(["table-resize-handle"]),
      Attr.title("Cmd-drag to resize"),
      Attr.on_pointerdown(
        resize_pointerdown(~dispatch, ~width_blocks, ~height_blocks),
      ),
    ],
    [],
  );

let parse_table = (exp: Exp.t): option(table_data) =>
  switch (exp.term) {
  | ListLit(es) =>
    let data =
      List.map(
        (e: Exp.t) =>
          switch (normalize_row(e).term) {
          | Tuple(ds) =>
            OptUtil.traverse(extract_entry, ds) |> Option.map(List.split)
          | _ => None
          },
        es,
      );

    let data_opt = OptUtil.sequence(data);
    switch (data_opt) {
    | Some(data) =>
      let (headers, rows) = List.split(data);
      switch (headers) {
      | [] => None
      | [h, ..._]
          when
            List.for_all(List.equal(Option.equal(String.equal), h), headers) =>
        Some((h, rows))
      | _ => None
      };
    | None => None
    };
  | _ => None
  };
