open Virtual_dom.Vdom;
open Node;
open Core;
open Util;

let span_c = cls => span([Attr.class_(cls)]);

module Text = (M: {
                 let map: Measured.t;
                 let settings: Model.settings;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (~no_sorts=false, ~sort=Sort.root, seg: Segment.t): list(Node.t) => {
    //note: no_sorts flag is used for backback
    let expected_sorts =
      no_sorts
        ? List.init(List.length(seg), i => (i, Sort.Any))
        : Segment.expected_sorts(sort, seg);
    let sort_of_p_idx = idx =>
      switch (List.assoc_opt(idx, expected_sorts)) {
      | None => Sort.Any
      | Some(sort) => sort
      };
    seg |> List.mapi((i, p) => of_piece(sort_of_p_idx(i), p)) |> List.concat;
  }
  and of_piece = (expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(expected_sort, t)
    | Grout(_) => [Node.text(Unicode.nbsp)]
    | Whitespace({content, _}) =>
      if (content == Whitespace.linebreak) {
        let str = M.settings.whitespace_icons ? Whitespace.linebreak : "";
        [
          span_c("linebreak", [text(str)]),
          Node.br([]),
          Node.text(StringUtil.repeat(m(p).last.col, Unicode.nbsp)),
        ];
      } else if (content == Whitespace.space) {
        let str = M.settings.whitespace_icons ? "·" : Unicode.nbsp;
        [span_c("whitespace", [text(str)])];
      } else {
        [Node.text(content)];
      }
    };
  }
  and of_tile = (expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          //TODO(andrew): more subtle logic about sort acceptability
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(of_delim(t.mold.out, is_consistent, t), ((seg, sort)) =>
         of_segment(~sort, seg)
       )
    |> List.concat;
  }
  and of_delim =
      (sort: Sort.t, is_consistent, t: Piece.tile, i: int): list(Node.t) => {
    let cls =
      List.length(t.label) == 1
        ? is_consistent ? "single" : "mono-sort-inconsistent"
        : is_consistent
            ? Tile.is_complete(t) ? "delim" : "delim-incomplete"
            : "delim-sort-inconsistent";
    [
      span(
        [Attr.classes([cls, "text-" ++ Sort.to_string(sort)])],
        [Node.text(List.nth(t.label, i))],
      ),
    ];
  };
};

let rec holes =
        (~font_metrics, ~map: Measured.t, seg: Segment.t): list(Node.t) =>
  seg
  |> List.map(
       fun
       | Piece.Whitespace(_) => []
       | Tile(t) =>
         t.children |> List.map(holes(~map, ~font_metrics)) |> List.concat
       | Grout(g) => [
           EmptyHoleDec.view(
             ~font_metrics, // TODO(d) fix sort
             {
               measurement: Measured.find_g(g, map),
               mold: Mold.of_grout(g, Any),
             },
           ),
         ],
     )
  |> List.concat;

let view =
    (
      ~font_metrics,
      //~show_backpack_targets,
      ~sel_seg,
      ~unsel_seg,
      ~map,
      ~settings: Model.settings,
    )
    : Node.t => {
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [
      span_c("code-text", Text.of_segment(unsel_seg)),
      span_c("code-text-shards", Text.of_segment(sel_seg)),
    ]
    @ holes(~map, ~font_metrics, sel_seg),
  );
};
