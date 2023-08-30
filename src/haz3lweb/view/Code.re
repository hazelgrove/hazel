open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

//  (
//    M: {
//      [@warning "-32"]
//      let map: Measured.t;
//      [@warning "-32"]
//      let settings: Model.settings;
//    },
module Txt = {
  open Tylr;

  let of_space = (s: Space.t) =>
    Space.length(s) == 0
      ? []
      : [
        s
        |> Space.map(c =>
             switch (c.shape) {
             | Space => text(Unicode.nbsp)
             | Newline => br()
             }
           )
        |> span,
      ];
  let of_tile = (t: Tile.t) => {
    // let s_cls =
    //   switch (t.mold.sort) {
    //   | None => "unsorted"
    //   | Some(s) => Sort.to_string(s)
    //   };
    span(
      // todo: add back delim vs mono distinction
      ~attr=Attr.classes(["tile"]),
      [text(t.token)],
    );
  };
  // todo
  let of_grout = (g: Grout.t) =>
    span(
      ~attr=Attr.classes(["grout"]),
      [text(g.sugg == "" ? "•" : g.sugg)],
    );
  let of_piece = (p: Piece.t) =>
    switch (p.shape) {
    | T(t) => of_tile(t)
    | G(g) => of_grout(g)
    };

  let rec of_meld = (mel: Meld.t) => {
    let (l, r) = mel.space;
    let (of_l, of_r) = (of_space(l), of_space(r));
    let of_c =
      switch (mel.chain) {
      | None => []
      | Some(c) => Chain.to_list(of_meld, of_piece, c)
      };
    span(List.concat([of_l, of_c, of_r]));
  };
  let of_wald = ret => ret |> Chain.to_list(of_piece, of_meld) |> span;

  let of_terr_l = (terr: Terrace.L.t) =>
    span([of_meld(terr.mel), of_wald(terr.wal)]);
  let of_terr_r = (terr: Terrace.R.t) =>
    span([of_wald(terr.wal), of_meld(terr.mel)]);

  let of_slope_dn = (slope: Slope.Dn.t) =>
    List.rev_map(of_terr_r, slope.terrs) @ of_space(slope.space);
  let of_slope_up = (slope: Slope.Up.t) =>
    of_space(slope.space) @ List.map(of_terr_l, slope.terrs);

  let of_zigg = ({up, top, dn}: Ziggurat.t) => {
    let of_top =
      top |> Option.map(wal => [of_wald(wal)]) |> Option.value(~default=[]);
    of_slope_up(up) @ of_top @ of_slope_dn(dn);
  };
};

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=100000,
    ((sort, is_consistent, is_complete, label, i)) => {
      let cls =
        switch (label) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when Form.is_string(s) => "string-lit"
        | _ => "default"
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      [
        span(
          ~attr=
            Attr.classes(["token", cls, Sort.to_string(sort), plurality]),
          [Node.text(List.nth(label, i))],
        ),
      ];
    },
  );
let of_delim =
    (sort: Sort.t, is_consistent, t: Piece.tile, i: int): list(Node.t) =>
  of_delim'((sort, is_consistent, Tile.is_complete(t), t.label, i));

let of_grout = [Node.text(Unicode.nbsp)];

let of_secondary =
  Core.Memo.general(
    ~cache_size_bound=1000000, ((secondary_icons, indent, content)) =>
    if (String.equal(Secondary.get_string(content), Form.linebreak)) {
      let str = secondary_icons ? Form.linebreak : "";
      [
        span_c("linebreak", [text(str)]),
        Node.br(),
        Node.text(StringUtil.repeat(indent, Unicode.nbsp)),
      ];
    } else if (String.equal(Secondary.get_string(content), Form.space)) {
      let str = secondary_icons ? "·" : Unicode.nbsp;
      [span_c("secondary", [text(str)])];
    } else if (Secondary.content_is_comment(content)) {
      [span_c("comment", [Node.text(Secondary.get_string(content))])];
    } else {
      [span_c("secondary", [Node.text(Secondary.get_string(content))])];
    }
  );

module Text = (M: {
                 let map: Measured.t;
                 let settings: ModelSettings.t;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment = (~no_sorts=false, ~sort, seg: Segment.t): list(Node.t) => {
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
    seg
    |> List.mapi((i, p) => (i, p))
    |> List.concat_map(((i, p)) => of_piece(sort_of_p_idx(i), p));
  }
  and of_piece = (expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(expected_sort, t)
    | Grout(_) => of_grout
    | Secondary({content, _}) =>
      of_secondary((M.settings.secondary_icons, m(p).last.col, content))
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
  };
};

let rec holes =
        (~font_metrics, ~map: Measured.t, seg: Segment.t): list(Node.t) =>
  seg
  |> List.concat_map(
       fun
       | Piece.Secondary(_) => []
       | Tile(t) => List.concat_map(holes(~map, ~font_metrics), t.children)
       | Grout(g) => [
           EmptyHoleDec.view(
             ~font_metrics, // TODO(d) fix sort
             {
               measurement: Measured.find_g(g, map),
               mold: Mold.of_grout(g, Any),
             },
           ),
         ],
     );

let simple_view = (~unselected, ~map, ~settings: ModelSettings.t): Node.t => {
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  div(
    ~attr=Attr.class_("code"),
    [span_c("code-text", Text.of_segment(~sort=Sort.Any, unselected))],
  );
};

let view =
    (
      ~sort: Sort.t,
      ~font_metrics,
      ~segment,
      ~unselected,
      ~measured,
      ~settings: ModelSettings.t,
      ~tylr,
    )
    : Node.t => {
  module Text =
    Text({
      let map = measured;
      let settings = settings;
    });
  let unselected =
    TimeUtil.measure_time("Code.view/unselected", settings.benchmark, () =>
      Text.of_segment(~sort, unselected)
    );
  let holes =
    TimeUtil.measure_time("Code.view/holes", settings.benchmark, () =>
      holes(~map=measured, ~font_metrics, segment)
    );
  div(
    ~attr=Attr.class_("code"),
    [
      span_c("code-text", unselected),
      // TODO restore (already regressed so no loss in commenting atm)
      // span_c("code-text-shards", Text.of_segment(segment)),
      ...holes,
    ]
    @ [
      div(
        ~attr=Attr.class_("tylr"),
        [Txt.of_meld(Tylr.Zipper.zip(tylr))],
      ),
    ],
  );
};
