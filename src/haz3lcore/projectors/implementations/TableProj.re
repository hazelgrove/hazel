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

/* Map a cell expression to the splice hosting it in the projector's
 * syntax, if any. [splices] are this projector's own splice children;
 * checking membership guards against stale elaborated forms and
 * against splices belonging to a projector nested inside a cell. */
let cell_splice_id = (splices: list(Base.splice), e: Exp.t): option(Id.t) =>
  switch (first_splice_id(e)) {
  | Some(id) when List.exists((s: Base.splice) => s.id == id, splices) =>
    Some(id)
  | _ => None
  };

let table =
    (
      info,
      ~parent as _: external_action => Ui_effect.t(unit),
      (headers, rows): (list(LabeledTuple.label), list(list(Exp.t))),
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~splice_view: View.splice_view,
      ~splices: list(Base.splice),
    ) => {
  let splice_cell = (e: Exp.t) =>
    cell_splice_id(splices, e)
    |> Option.map(id =>
         Node.div(
           ~attrs=[Attr.classes(["cell-splice"])],
           [splice_view(id)],
         )
       );
  table_view(
    ~header_cells=List.map(h => Node.th([Node.text(h)]), headers),
    ~rows=List.map(row_cells(info.utility, view_seg, ~splice_cell), rows),
  );
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t, seg: Base.segment) =>
    switch (table_of(any)) {
    | None => None
    | Some(_) =>
      /* Editable cells: wrap each cell value of the (raw) selected
       * syntax in a splice. Splices are transparent to statics, so
       * headers can still be inferred from the elaborated form. If the
       * syntax isn't literally a list of tuples, keep it unchanged and
       * render read-only as before. */
      let override =
        splice_table_cells(seg) |> Option.map(seg => Syntax(seg));
      Some(((), override));
    };

  let focusable =
    Focusable.{
      pointer: None,
      keyboard: None,
    };
  let dynamics = false;
  let elaborate_syntax = true;
  let placeholder = (_, info, splice_size: View.splice_size) =>
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
      /* +1 vertical line reserved for the inline error banner
       * rendered above the raw syntax in the error view. */
      ProjectorCore.Shape.{
        vertical: Block(n_lines),
        horizontal: max(max_width, String.length(error_message)),
      };
    | Some((header, rows)) =>
      /* Outer space reserved for the table frame itself (border + the
       * .table-inner wrapper's 5px horizontal padding, approximated). */
      let outer_padding_chars = 4;
      /* Approximate per-column cell padding, in characters. */
      let per_column_padding_chars = 2;
      /* Beyond this row count the table switches to scrolled mode with
       * sticky headers — see proj-table.css's `:has(tbody tr:nth-child(10))`
       * selector. Must stay in sync with that threshold. */
      let scroll_threshold_rows = 10;

      let splices = Segment.direct_splices(info.syntax);
      /* Intrinsic size of one cell, in character-grid units: splice
       * cells size around their sub-editor content, other cells around
       * their abbreviated value text. */
      let cell_size = (e: Exp.t): Point.t =>
        switch (cell_splice_id(splices, e)) {
        | Some(id) =>
          let size = splice_size(id);
          Point.{
            row: max(1, size.row + 1),
            /* Room for the sub-editor's horizontal cell margin. */
            col: size.col + 2,
          };
        | None =>
          Point.{
            row: 1,
            col:
              Abbreviate.abbreviate_exp(~available=max_column_length, e)
              |> snd,
          }
        };
      let sizes = List.map(List.map(cell_size), rows);

      let header_row_chars =
        header |> List.map(String.length) |> List.fold_left((+), 0);
      let widest_row_chars =
        sizes
        |> List.map(row =>
             row |> List.map((p: Point.t) => p.col) |> List.fold_left((+), 0)
           )
        |> List.fold_left(max, 0, _);
      let content_chars = max(header_row_chars, widest_row_chars);

      let row_heights =
        List.map(
          row =>
            row
            |> List.map((p: Point.t) => p.row)
            |> List.fold_left(max, 1, _),
          sizes,
        );
      let total_rows = List.fold_left((+), 0, row_heights);
      let num_rows = List.length(rows);
      let num_cols = List.length(header);
      ProjectorCore.Shape.{
        vertical:
          /* Single-line rows scroll past the threshold (matching the
           * CSS); multi-line splice content just makes the table taller. */
          Block(
            total_rows == num_rows
              ? min(num_rows, scroll_threshold_rows) : total_rows,
          ),
        horizontal:
          outer_padding_chars
          + content_chars
          + num_cols
          * per_column_padding_chars,
      };
    };
  let update = (model, _, _) => model;
  let error = (_, info) =>
    switch (get(info)) {
    | Some(_) => None
    | None => Some(ProjectorBase.{message: error_message})
    };

  /* A fresh column label colliding with neither the syntactic labels
   * nor the (possibly type-derived) rendered headers. */
  let fresh_column_label = (info: info): string => {
    let taken =
      List.filter_map(Fun.id, TableCore.row_labels(info.syntax, 0))
      @ (
        switch (get(info)) {
        | Some((headers, _)) => headers
        | None => []
        }
      );
    let rec go = n => {
      let candidate = "col" ++ string_of_int(n);
      List.mem(candidate, taken) ? go(n + 1) : candidate;
    };
    go(1);
  };

  /* Row/column operations offered in the context menu inside a cell
   * splice. Each precomputes the resulting syntax; operations that
   * don't apply (single row/column, unexpected shape) are omitted. */
  let context_actions = ((), info: info, ~splice: Id.t) =>
    switch (TableCore.find_cell(info.syntax, splice)) {
    | None => []
    | Some({row, col, n_rows, n_cols}) =>
      let mk = (label, result: option(Base.segment)) =>
        switch (result) {
        | Some(seg) => [
            ProjectorBase.{
              label,
              action: SetSyntax(seg),
            },
          ]
        | None => []
        };
      /* New columns are labeled only when the clicked row's cells are:
       * auto-labeled tables get their headers from the type, and a
       * mixed labeled/unlabeled row would change the elaborated order. */
      let labeled =
        TableCore.row_labels(info.syntax, row)
        |> List.for_all(Option.is_some);
      let col_label = () => labeled ? Some(fresh_column_label(info)) : None;
      let syntax = info.syntax;
      mk(
        "Insert row above",
        TableCore.insert_row(syntax, ~at=row, ~template=row),
      )
      @ mk(
          "Insert row below",
          TableCore.insert_row(syntax, ~at=row + 1, ~template=row),
        )
      @ (
        n_rows > 1
          ? mk("Delete row", TableCore.remove_row(syntax, ~at=row)) : []
      )
      @ mk(
          "Insert column left",
          TableCore.insert_col(syntax, ~at=col, ~label=col_label()),
        )
      @ mk(
          "Insert column right",
          TableCore.insert_col(syntax, ~at=col + 1, ~label=col_label()),
        )
      @ (
        n_cols > 1
          ? mk("Delete column", TableCore.remove_col(syntax, ~at=col)) : []
      );
    };

  let view =
      (
        {info, parent, view_seg, splice_view, splices, _}:
          View.args(model, action),
      )
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
      View.mk(
        Node.div(
          ~attrs=[Attr.classes(["table-inner"])],
          [table(info, ~view_seg, ~splice_view, ~splices, ~parent, data)],
        ),
      )
    };
};
