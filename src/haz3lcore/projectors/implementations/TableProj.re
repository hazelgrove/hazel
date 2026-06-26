open Util;
open ProjectorBase;
open Language;
open TableCore;

/* Table projector logic: projects a list of labeled tuples as a table.
   The web view (table rendering, error banner) lives in
   src/web/projectors/TableProjView.re, reusing the helpers below. */

let error_message = "Elaborated syntax is not a table: list of labeled tuples with consistent labels.";

/* The placeholder must reserve as much editor space as the frontend's
 * view occupies. The web view is a CSS overlay whose size can only be
 * approximated; the terminal view is an exact box-drawing grid
 * (TermProjector.table). Terminal frontends install TextGrid at
 * startup, mirroring ProjectorBase.focusables; it carries the
 * frontend's cell renderer so reserved widths measure the very strings
 * the view draws (the renderer can't live here: it needs ExpToSegment,
 * which depends back on this module via ProjectorInit). */
type sizing =
  | WebApprox
  | TextGrid(Exp.t => string);
let sizing: ref(sizing) = ref(WebApprox);

/* Beyond this row count the web table switches to scrolled mode with
 * sticky headers — see proj-table.css's `:has(tbody tr:nth-child(10))`
 * selector. Must stay in sync with that threshold. The terminal view
 * can't scroll, so it elides rows past it instead. */
let scroll_threshold_rows = 10;

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

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Any.t) =>
    switch (table_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let dynamics = false;
  let elaborate_syntax = true;
  let placeholder = (_, info) =>
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
      switch (sizing^) {
      | TextGrid(cell_text) =>
        /* Exact size of the terminal view's box-drawing grid: each
         * column is `│ cell ` (3 chrome chars) at its widest cell, plus
         * the closing `│`; vertically a top border, header, header
         * separator, the (threshold-capped) data rows, and a bottom
         * border. */
        let col_w = (i, h) =>
          List.fold_left(
            (acc, row) =>
              switch (List.nth_opt(row, i)) {
              | Some(c) => max(acc, String.length(cell_text(c)))
              | None => acc
              },
            String.length(h),
            rows,
          );
        let widths = List.mapi(col_w, header);
        ProjectorCore.Shape.{
          vertical:
            Block(min(List.length(rows), scroll_threshold_rows) + 3),
          horizontal:
            List.fold_left((+), 1, widths) + 3 * List.length(header),
        };
      | WebApprox =>
        /* Outer space reserved for the table frame itself (border + the
         * .table-inner wrapper's 5px horizontal padding, approximated). */
        let outer_padding_chars = 4;
        /* Approximate per-column cell padding, in characters. */
        let per_column_padding_chars = 2;

        let header_row_chars =
          header |> List.map(String.length) |> List.fold_left((+), 0);
        let widest_row_chars =
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
        let content_chars = max(header_row_chars, widest_row_chars);

        let num_rows = List.length(rows);
        let num_cols = List.length(header);
        ProjectorCore.Shape.{
          vertical: Block(min(num_rows, scroll_threshold_rows)),
          horizontal:
            outer_padding_chars
            + content_chars
            + num_cols
            * per_column_padding_chars,
        };
      }
    };
  let update = (model, _, _) => model;
  let error = (_, info) =>
    switch (get(info)) {
    | Some(_) => None
    | None => Some(ProjectorBase.{message: error_message})
    };
  let initialize = None;
};
