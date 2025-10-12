open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;
open Zipper;

/*
 Used to display line numbering alongside cells
 */

module Model = CodeWithStatics.Model;

module View = {
  let view = (model: Model.t) => {
    let {editor: {syntax: {measured, _}, state: {zipper, _}, _}, _}: Model.t = model;
    let num_rows =
      IntMap.fold(
        /* The folding function: takes the key, value, and accumulator. */
        (_, _, count) => count + 1,
        measured.rows,
        /* The initial value of the accumulator (count). */
        0,
      );
    let Point.{row, _} = Zipper.Caret.point(measured, zipper);
    [
      Node.div(
        //replace with Node.div
        ~attrs=[Attr.classes(["code", "line-numbers"])],
        [
          Node.span(
            ~attrs=[Attr.classes(["code-text", "line-numbers-text"])],
            List.init(num_rows, (i): Node.t =>
              Text(
                string_of_int(abs(i - row))
                ++ (i + 1 == num_rows ? "" : "\n"),
              )
            ): list(Node.t),
          ),
        ],
      ),
    ];
  };
};
