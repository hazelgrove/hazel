open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open Util.Sets;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */
module Model = CodeWithStatics.Model;

/*
 Line numbering works by:
 1. Having a skip_rows function to check if a certain row should not be displayed (in the cases of multiline GUIs)
 2. Having a processed_line_numbers function to return a list of line numbers that each row should display (either a number or 0, indicating this row should not be display)
 3. processed_line_numbers also returns the current display row of the cursor
  */
module View = {
  let view = (model: Model.t, show_relative_numbers: bool, selected: bool) => {
    let {editor: {syntax: {measured, _}, state: {zipper, _}, _}, _}: Model.t = model;
    let num_rows = List.length(measured.piece_rows);

    /*
     Multiline projects are either Tab or Block, so they either
     1. Defer linebreaks (hence checking for secondary)
     2. Or are multiline themselves
     */
    // This will be used for having a set of line numbers that have prev. been reached
    let skip_set_generic = (init, map): SIntSet.t =>
      Id.Map.fold(
        (_id, measurement: Measured.measurement, acc) =>
          //measurement.last.row is 1 larger than the actual last row of the measurement
          if (measurement.origin.row + 1 < measurement.last.row) {
            List.fold_left(
              (acc, i: int) => {SIntSet.add(i, acc)},
              acc,
              List.init(
                measurement.last.row - measurement.origin.row - 1, (i: int) => {
                measurement.origin.row + 1 + i
              }),
            );
          } else {
            acc;
          },
        map,
        init,
      );

    let skip_set = {
      skip_set_generic(
        skip_set_generic(SIntSet.empty, measured.secondary),
        measured.projectors,
      );
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
        let skip_this_row =
          SIntSet.exists((iter: int) => {i == iter}, skip_set);
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
    let index_to_text = (i): string => {
      let processed_line = List.nth(processed_list, i);
      processed_line == 0
        ? "\n"
        : {
          show_relative_numbers && selected
            ? string_of_int(
                abs(processed_line - cursor_row) == 0
                  ? processed_line : abs(processed_line - cursor_row),
              )
              ++ (i == num_rows ? "" : "\n")
            : string_of_int(processed_line) ++ (i == num_rows ? "" : "\n");
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
