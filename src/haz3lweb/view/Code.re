open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
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
    ~cache_size_bound=10000, ((secondary_icons, indent, content)) =>
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

/* PERF: Tile memoization makes a >2X difference. I've left
   the memoization in place for delims and secondary above as it still
   seems like a marginal positive (5-10% difference).

   WARNING: Note that this the table is stored outside the Text functor.
   This means that if there are data dependencies on the functor argument
   values, they will need to be explictly encoded in the key.

   TODO: Consider setting a limit for the hashtbl size */
let piece_hash: Hashtbl.t((Sort.t, Piece.t, int, ModelSettings.t), list(t)) =
  Hashtbl.create(10000);

module Text = (M: {
                 let map: Measured.t;
                 let settings: ModelSettings.t;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (no_sorts, sort, seg: Segment.t, ~inject, ~font_metrics, ~folded)
          : list(Node.t) => {
    /* note: no_sorts flag is used for backback view;
       otherwise Segment.expected_sorts call crashes for some reason */
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
    |> List.concat_map(((i, p)) =>
         of_piece(sort_of_p_idx(i), p, ~inject, ~font_metrics, ~folded)
       );
  }
  and of_piece' =
      (expected_sort: Sort.t, p: Piece.t, ~inject, ~font_metrics, ~folded)
      : list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(expected_sort, t, ~inject, ~font_metrics, ~folded)
    | Grout(_) => of_grout
    | Secondary({content, _}) =>
      of_secondary((M.settings.secondary_icons, m(p).last.col, content))
    };
  }
  and of_piece =
      (expected_sort: Sort.t, p: Piece.t, ~inject, ~font_metrics, ~folded)
      : list(Node.t) => {
    /* Last two elements of arg track the functorial args which
       can effect the code layout; without these the first,
       indentation can get out of sync */
    let arg = (expected_sort, p, m(p).last.col, M.settings);
    try(Hashtbl.find(piece_hash, arg)) {
    | _ =>
      let res = of_piece'(expected_sort, p, ~inject, ~font_metrics, ~folded);
      Hashtbl.add(piece_hash, arg, res);
      res;
    };
  }
  and of_tile =
      (expected_sort: Sort.t, t: Tile.t, ~inject, ~font_metrics, ~folded)
      : list(Node.t) => {
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
         of_segment(false, sort, seg, ~inject, ~font_metrics, ~folded)
       )
    |> {
      x => {
        switch (x) {
        | [a, b, c, _, ...xs] when List.mem(t.id, folded) => [
            a,
            b,
            c,
            Fold.view,
            ...xs,
          ]
        | _ => x
        };
      };
    }
    |> List.concat;
  };
};

let rec holes =
        (~font_metrics, ~map: Measured.t, seg: Segment.t, ~folded)
        : list(Node.t) =>
  seg
  |> List.concat_map(
       fun
       | Piece.Secondary(_) => []
       | Tile(t) => {
           let children =
             switch (t.children) {
             | [a, _, ...xs] when List.mem(t.id, folded) => [a, ...xs]
             | _ => t.children
             };
           List.concat_map(holes(~map, ~font_metrics, ~folded), children);
         }
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

let fold_buttons =
    (~font_metrics, ~map: Measured.t, seg: Segment.t, ~folded, ~inject)
    : list(Node.t) => {
  let rec fold_buttons_with_row = seg =>
    List.concat_map(
      fun
      | Piece.Secondary(_)
      | Grout(_) => []
      | Tile(t) when Module.foldable(t) => {
          let row = Measured.find_t(t, map).origin.row;
          let button =
            Fold.button_view(font_metrics, inject, folded, t.id, row);
          let children =
            switch (t.children) {
            | [a, _, ...xs] when List.mem(t.id, folded) => [a, ...xs]
            | _ => t.children
            };
          [
            (button, row),
            ...List.concat_map(fold_buttons_with_row, children),
          ];
        }
      | Tile(t) => {
          let children =
            switch (t.children) {
            | [a, _, ...xs] when List.mem(t.id, folded) => [a, ...xs]
            | _ => t.children
            };
          List.concat_map(fold_buttons_with_row, children);
        },
      seg,
    );
  let remove_duplicates = res_with_row => {
    let (res, _) =
      List.fold_left(
        ((buttons, rows), (new_button, new_row)) =>
          if (List.mem(new_row, rows)) {
            (buttons, rows);
          } else {
            ([new_button, ...buttons], [new_row, ...rows]);
          },
        ([], []),
        res_with_row,
      );
    res;
  };
  seg |> fold_buttons_with_row |> remove_duplicates;
};
let simple_view =
    (~unselected, ~map, ~settings: ModelSettings.t, ~inject, ~font_metrics)
    : Node.t => {
  module Text =
    Text({
      let map = map;
      let settings = settings;
    });
  div(
    ~attr=Attr.class_("code"),
    [
      span_c(
        "code-text",
        Text.of_segment(
          false,
          Sort.Any,
          unselected,
          ~inject,
          ~font_metrics,
          ~folded=[],
        ),
      ),
    ],
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
      ~inject,
      ~folded,
    )
    : Node.t => {
  module Text =
    Text({
      let map = measured;
      let settings = settings;
    });
  let unselected =
    TimeUtil.measure_time("Code.view/unselected", settings.benchmark, () =>
      Text.of_segment(
        false,
        sort,
        unselected,
        ~font_metrics,
        ~inject,
        ~folded,
      )
    );
  let holes =
    TimeUtil.measure_time("Code.view/holes", settings.benchmark, () =>
      holes(~map=measured, ~font_metrics, segment, ~folded)
    );
  let buttons =
    TimeUtil.measure_time("Code.view/buttons", settings.benchmark, () =>
      fold_buttons(~map=measured, ~font_metrics, segment, ~folded, ~inject)
    );
  div(
    ~attr=Attr.class_("code"),
    buttons
    @ [
      span_c("code-text", unselected),
      // TODO restore (already regressed so no loss in commenting atm)
      // span_c("code-text-shards", Text.of_segment(segment)),
      ...holes,
    ],
  );
};
