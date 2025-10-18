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
    let skip_row = (row: int) =>
      Id.Map.fold(
        (_id, measurement: Measured.measurement, acc) => {
          acc
          || measurement.origin.row < row
          && measurement.last.row > row
          && measurement.last.row > measurement.origin.row
        },
        measured.secondary,
        false,
      );
    let row_counter = ref(0);
    let Point.{row, _} = Zipper.Caret.point(measured, zipper);
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
                    skip_row(i)
                      ? "\n"
                      : {
                        row_counter := row_counter^ + 1;
                        show_relative_numbers && selected
                          ? string_of_int(
                              abs(i - row) == 0
                                ? row_counter^ : abs(i - row),
                            )
                            ++ (row_counter^ == num_rows ? "" : "\n")
                          : string_of_int(row_counter^)
                            ++ (row_counter^ == num_rows ? "" : "\n");
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
