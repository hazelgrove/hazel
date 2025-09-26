open Haz3lcore;
//open Virtual_dom.Vdom;
open Util;
open WebUtil;

/*
 Used to display line numbering alongside cells
 */

module Model = CodeWithStatics.Model;

module View = {
  let view = (model: Model.t) => {
    let {
      editor:
        {
          syntax: {measured, selection_ids, segment, shape_map, _},
          state: {zipper: z, _},
          _,
        },
      _,
    }: Model.t = model;
    let num_rows = 19;
    [
      Node.div(
        //replace with Node.div
        ~attrs=[Attr.classes(["code", "line-numbers"])],
        [
          Node.span(
            ~attrs=[Attr.classes(["code-text", "line-numbers-text"])],
            List.init(num_rows, (i): Node.t =>
              Text(string_of_int(i + 1) ++ (i + 1 == num_rows ? "" : "\n"))
            ): list(Node.t),
          ),
        ],
      ),
    ];
  };
};
