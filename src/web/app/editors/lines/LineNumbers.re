open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */
module Model = CodeWithStatics.Model;

module View = {
  let view = (model: Model.t, show_relative_numbers: bool, selected: bool) => {
    let {editor: {syntax: {measured, _}, state: {zipper, _}, _}, _}: Model.t = model;
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
      Node.span(
        ~attrs=
          i == row && selected ? [Attr.classes(["line-numbers-bold"])] : [],
        [Text(index_to_text(i))],
      );
    };
    [
      Node.div(
        ~attrs=[
          Attr.classes([
            "code",
            "line-numbers",
            selected ? "line-numbers-selected" : "",
          ]),
        ],
        [
          Node.span(
            ~attrs=[Attr.classes(["code-text", "line-numbers-text"])],
            List.init(num_rows, (i): Node.t => {index_to_span(i)}),
          ),
        ],
      ),
    ];
  };
};
