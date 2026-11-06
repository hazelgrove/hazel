open Virtual_dom.Vdom;

/* The spacers the text layer inserts for block padding (Grid): an empty
   inline-block before a piece, and before a line break one as tall as the
   row plus the gap below it, aligned to the top so the row's text stays put
   and the next row moves down. */

let before_piece = (px: float): Node.t =>
  Node.span(
    ~attrs=[
      Attr.classes(["gadget"]),
      Attr.create("style", Printf.sprintf("width: %fpx", px)),
    ],
    [],
  );

let below_row = (~grid: Grid.t, row: int): list(Node.t) =>
  switch (Grid.row_gap(grid, row)) {
  | 0. => []
  | gap => [
      Node.span(
        ~attrs=[
          Attr.classes(["gadget", "vertical"]),
          Attr.create(
            "style",
            Printf.sprintf(
              "height: %fpx",
              Grid.font_metrics(grid).row_height +. gap,
            ),
          ),
        ],
        [],
      ),
    ]
  };
