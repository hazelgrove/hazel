open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */

module Model = CodeWithStatics.Model;

/*
 There are two types of row numbers
 1. Row numbers directly from rows, multiline projectors count as multiple rows
 2. Processed row numbers, which skip the extra rows of multiline projectors
 */
module View = {
  let view = (model: Model.t, show_relative_numbers: bool, selected: bool) => {
    let {editor: {syntax: {measured, _}, state: {zipper, _}, _}, _}: Model.t = model;
    let num_rows = List.length(measured.piece_rows);
    let skip_row_generic = (row: int, map) =>
      Id.Map.fold(
        (_id, measurement: Measured.measurement, acc) => {
          acc
          || measurement.origin.row < row
          && measurement.last.row > row
          && measurement.last.row > measurement.origin.row
        },
        map,
        false,
      );
    let skip_row = (row: int) => {
      skip_row_generic(row, measured.secondary)
      || skip_row_generic(row, measured.projectors);
    };
    let Point.{row, _} = Zipper.Caret.point(measured, zipper);
    /*
     Returns the processed line numbers, with 0 being a line to skip
     and 1 being the initial line
     i is the index of the row we are on
     acc is the accumulator variable for line numbers
     */
    // Takes in the current row index and accumulator
    // Outputs the processed line number list and the cursor row
    let rec processed_line_numbers = (i: int, acc: int): (list(int), int) =>
      if (i == num_rows) {
        ([], 0);
      } else {
        let skip_this_row = skip_row(i);
        let (returned_processed_list, returned_cursor_row) =
          skip_this_row
            ? processed_line_numbers(i + 1, acc)
            : processed_line_numbers(i + 1, acc + 1);
        let current_line_number = skip_this_row ? 0 : acc;
        let cursor_row =
          if (returned_cursor_row == 0 && i == row) {
            acc;
          } else {
            returned_cursor_row;
          };
        ([current_line_number] @ returned_processed_list, cursor_row);
      };
    let (processed_list, cursor_row) = processed_line_numbers(0, 1);
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
            List.init(num_rows, (i): Node.t =>
              Node.span(
                ~attrs=
                  i == row && selected
                    ? [Attr.classes(["line-numbers-bold"])] : [],
                [
                  Text(
                    {
                      let processed_line = List.nth(processed_list, i);
                      processed_line == 0
                        ? "\n"
                        : {
                          show_relative_numbers && selected
                            ? string_of_int(
                                abs(processed_line - cursor_row) == 0
                                  ? processed_line
                                  : abs(processed_line - cursor_row),
                              )
                              ++ (i == num_rows ? "" : "\n")
                            : string_of_int(processed_line)
                              ++ (i == num_rows ? "" : "\n");
                        };
                    },
                  ),
                ],
              )
            ): list(Node.t),
          ),
        ],
      ),
    ];
  };
};
