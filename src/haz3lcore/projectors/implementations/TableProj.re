open Util;
open Virtual_dom.Vdom;
open ProjectorBase;
open Language;
open TableCore;

let error_message = "Elaborated syntax is not a table: list of labeled tuples with consistent labels.";

let table_of =
    (any: Any.t): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (any) {
  | Exp(exp) =>
    parse_table(exp)
    |> Option.bind(_, ((headers, rows)) =>
         OptUtil.traverse(Fun.id, headers) |> Option.map(hs => (hs, rows))
       )
  | _ => None
  };

let get =
    (info: info): option((list(LabeledTuple.label), list(list(Exp.t)))) =>
  switch (info.elaborated) {
  | Some(elab_exp) => table_of(Exp(elab_exp))
  | None =>
    switch (info.syntax |> info.utility.seg_to_term) {
    | Some(s) => table_of(s)
    | None => None
    }
  };

let table =
    (
      info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
    ) =>
  table_view(
    ~header_cells=List.map(h => Node.th([Node.text(h)]), headers),
    ~rows=List.map(row_cells(info.utility, view_seg), rows),
  );

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
    switch (table_of(any)) {
    | Some((header, rows)) =>
      let (w, h) =
        default_size_for_table(List.map(h => Some(h), header), rows);
      Some({
        width_blocks: w,
        height_blocks: h,
      });
    | None => None
    };

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = false;
  let elaborate_syntax = true;
  let placeholder = (model, info) =>
    switch (get(info)) {
    | None =>
      let s = info.utility.seg_to_string(info.syntax);
      let lines = String.split_on_char('\n', s);
      let n_lines = List.length(lines);
      let max_width =
        List.fold_left(
          (acc, line) => max(acc, String.length(line)),
          0,
          lines,
        );
      ProjectorCore.Shape.{
        vertical: Block(n_lines),
        horizontal: max(max_width, String.length(error_message)),
      };
    | Some(_) =>
      ProjectorCore.Shape.{
        vertical: Block(model.height_blocks),
        horizontal: model.width_blocks,
      }
    };
  let update = (_model, info, action) =>
    switch (action) {
    | ResizeTo(w, h) =>
      /* Cap height at header + data rows so the user can resize all the
         way to fitting the whole table (header included) without a
         scrollbar. */
      let max_h =
        switch (get(info)) {
        | None => h
        | Some((_, rows)) => 1 + List.length(rows)
        };
      {
        width_blocks: clamp_width_blocks(w),
        height_blocks: clamp_height_blocks(min(h, max_h)),
      };
    };
  let error = (_, info) =>
    switch (get(info)) {
    | Some(_) => None
    | None => Some(ProjectorBase.{message: error_message})
    };

  let view =
      ({model, info, parent, view_seg, local, _}: View.args(model, action))
      : View.t =>
    switch (get(info)) {
    | None =>
      let seg = Segment.unparenthesize(info.syntax);
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      let banner =
        Node.div(
          ~attrs=[Attr.classes(["table-error-banner"])],
          [Node.text(error_message)],
        );
      View.mk(
        ~error=true,
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [banner, view_seg(sort, seg)],
        ),
      );
    | Some(data) =>
      let dispatch = (w, h) => local(ResizeTo(w, h));
      let handle =
        resize_handle(
          ~dispatch,
          ~width_blocks=model.width_blocks,
          ~height_blocks=model.height_blocks,
        );
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [table(info, ~view_seg, ~parent, data), handle],
        ),
      );
    };
};
