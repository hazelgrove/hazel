open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */
module Model = CodeWithStatics.Model;

/* Walk the elaboration once and collect rows that contain at least one
 * `Ap` term whose id is in `pending_set`. We index by row so the gutter
 * can render one spinner per affected line, even if the line has many
 * dirty `Ap`s.
 *
 * Function applications are the main slow site in user programs, so
 * surfacing them as the granularity of the in-flight indicator is far
 * less noisy than per-id spinners on every dirty subterm. */
let dirty_ap_rows =
    (
      ~pending_set: Id.Set.t,
      ~measured: Measured.t,
      elab: option(Language.Exp.t),
    )
    : list(int) =>
  switch (elab) {
  | _ when Id.Set.is_empty(pending_set) => []
  | None => []
  | Some(e) =>
    let dirty_ids = ref([]);
    let f_exp = (continue, e: Language.Exp.t) => {
      let id = Language.Exp.rep_id(e);
      switch (Language.Exp.term_of(e)) {
      | Ap(_, _, _) when Id.Set.mem(id, pending_set) =>
        dirty_ids := [id, ...dirty_ids^]
      | _ => ()
      };
      continue(e);
    };
    let _ = Language.TermBase.Exp.map_term(~f_exp, e);
    let rows = ref([]);
    List.iter(
      id =>
        switch (Id.Map.find_opt(id, measured.tiles)) {
        | Some([(_, {origin: {row, _}, _}), ..._]) =>
          if (!List.mem(row, rows^)) {
            rows := [row, ...rows^];
          }
        | _ => ()
        },
      dirty_ids^,
    );
    rows^;
  };

module View = {
  let view =
      (
        ~pending_set: Id.Set.t=Id.Set.empty,
        ~elab: option(Language.Exp.t)=None,
        model: Model.t,
        show_relative_numbers: bool,
        selected: bool,
      ) => {
    let {editor: {syntax: {measured, _}, state: {zipper, _}, _}, _}: Model.t = model;
    let dirty_rows = dirty_ap_rows(~pending_set, ~measured, elab);
    let num_rows = List.length(measured.piece_rows);
    let empty_row = row => {
      let result = List.nth_opt(List.rev(measured.piece_rows), row);
      switch (result) {
      | Some(value) =>
        switch (value) {
        | [] => true
        | _ => false
        }
      | None => true // The row doesn't actually exist, hence it's empty
      };
    };
    let Point.{row, _} = Zipper.Caret.point(measured, zipper);
    let cursor_row_index = row;
    /*
     Recursively builds a list of line numbers for display, skipping empty rows.

     Parameters:
       - row_index: Current row being processed (0-indexed)
       - line_count: The line number to assign to the current non-empty row

     Returns: (line_numbers, cursor_line_number) where:
       - line_numbers: List where 0 indicates skip this row, non-zero is the display line number
       - cursor_line_number: The line number where the cursor is located (0 if not found yet)
     */
    let rec processed_line_numbers =
            (row_index: int, line_count: int): (list(int), int) =>
      if (row_index == num_rows) {
        ([], 0);
      } else {
        let is_row_empty = empty_row(row_index);
        let (returned_processed_list, returned_cursor_line_number) =
          is_row_empty
            ? processed_line_numbers(row_index + 1, line_count)
            : processed_line_numbers(row_index + 1, line_count + 1);
        let current_line_number = is_row_empty ? 0 : line_count;
        let cursor_line_number =
          if (returned_cursor_line_number == 0 && row_index == cursor_row_index) {
            line_count;
          } else {
            returned_cursor_line_number;
          };
        (
          [current_line_number] @ returned_processed_list,
          cursor_line_number,
        );
      };
    let (processed_list, cursor_line_number) = processed_line_numbers(0, 1);
    /*
     Converts a row index to its display text.
     Returns "\n" for empty rows, or the line number (absolute or relative) with newline.
     */
    let index_to_text = (i): string => {
      let line_number = List.nth(processed_list, i);
      line_number == 0
        ? "\n"  // if this is a line we want to skip
        : {
          (
            if (show_relative_numbers && selected) {
              string_of_int(
                abs(line_number - cursor_line_number) == 0
                  ? line_number : abs(line_number - cursor_line_number),
              );
            } else {
              string_of_int(line_number);
            }
          )
          ++ (i == num_rows ? "" : "\n"); // Add a line break if this is not the last row
        };
    };
    let index_to_span = (i): Node.t => {
      let dirty = List.mem(i, dirty_rows);
      Node.span(
        ~attrs=
          (
            i == row && selected ? [Attr.classes(["line-numbers-bold"])] : []
          )
          @ (dirty ? [Attr.classes(["line-numbers-dirty-ap"])] : []),
        [Text(index_to_text(i))],
      );
    };
    /* Spinner overlay: one element per dirty row. Positioned via a CSS
     * grid pinned to the gutter so we can place each spinner over the
     * correct row independent of the line-number text flow. */
    let dirty_spinners =
      List.map(
        (r: int): Node.t =>
          Node.div(
            ~attrs=[
              Attr.classes(["line-numbers-spinner"]),
              Attr.create(
                "style",
                Printf.sprintf("top: %.2fch", float_of_int(r) *. 1.0 *. 1.0)
                ++ "em;",
              ),
              Attr.title("Re-evaluating function call(s) on this line"),
            ],
            [],
          ),
        dirty_rows,
      );
    [
      Node.div(
        ~attrs=[
          Attr.classes(
            [
              "code",
              "line-numbers",
              selected ? "line-numbers-selected" : "",
            ]
            @ (dirty_rows == [] ? [] : ["line-numbers-has-dirty"]),
          ),
        ],
        [
          Node.span(
            ~attrs=[Attr.classes(["code-text", "line-numbers-text"])],
            List.init(num_rows, (i): Node.t => {index_to_span(i)}),
          ),
          ...dirty_spinners,
        ],
      ),
    ];
  };
};
