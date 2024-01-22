open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    ((label, is_in_buffer, sort, is_consistent, is_complete, i)) => {
      let cls =
        switch (label) {
        | _ when is_in_buffer => "in-buffer"
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when s == Form.explicit_hole => "explicit-hole"
        | [s] when Form.is_string(s) => "string-lit"
        | _ => "default"
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      let label = is_in_buffer ? AssistantExpander.mark(label) : label;
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
    (is_in_buffer, is_consistent, t: Piece.tile, i: int): list(Node.t) =>
  of_delim'((
    t.label,
    is_in_buffer,
    t.mold.out,
    is_consistent,
    Tile.is_complete(t),
    i,
  ));

let of_grout =
    (
      ~font_metrics,
      ~global_inference_info: InferenceResult.global_inference_info,
      id: Id.t,
    ) => {
  let suggestion =
    InferenceView.get_suggestion_ui_for_id(
      ~font_metrics,
      id,
      global_inference_info,
      false,
    );
  switch (suggestion) {
  | (NoSuggestion(SuggestionsDisabled), _)
  | (NoSuggestion(NotSuggestableHoleId), _)
  | (NoSuggestion(OnlyHoleSolutions), _)
  | (_, None) => [Node.text(Unicode.nbsp)]
  | (Solvable(suggestion_node), TypHole)
  | (NestedInconsistency(suggestion_node), TypHole) => [
      [suggestion_node] |> span_c("solved-annotation"),
    ]
  | (Solvable(_), ExpHole)
  | (NestedInconsistency(_), ExpHole) => [
      [Node.text("?")] |> span_c("prompt-ci"),
    ]
  | (NoSuggestion(OccursFailed), _)
  | (NoSuggestion(InconsistentSet), _) => [
      [Node.text("!")] |> span_c("unsolved-annotation"),
    ]
  };
};

let of_secondary =
  Core.Memo.general(
    ~cache_size_bound=10000, ((content, secondary_icons, indent)) =>
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
let piece_hash:
  Hashtbl.t(
    (
      Sort.t,
      Piece.t,
      int,
      Settings.t,
      FontMetrics.t,
      InferenceResult.global_inference_info,
    ),
    list(t),
  ) =
  Hashtbl.create(10000);

module Text = (M: {
                 let map: Measured.t;
                 let settings: Settings.t;
               }) => {
  let m = p => Measured.find_p(p, M.map);
  let rec of_segment =
          (
            buffer_ids,
            no_sorts,
            sort,
            font_metrics: FontMetrics.t,
            global_inference_info: InferenceResult.global_inference_info,
            seg: Segment.t,
          )
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
         of_piece(
           buffer_ids,
           font_metrics,
           global_inference_info,
           sort_of_p_idx(i),
           p,
         )
       );
  }
  and of_piece' =
      (
        buffer_ids: list(Uuidm.t),
        font_metrics: FontMetrics.t,
        global_inference_info: InferenceResult.global_inference_info,
        expected_sort: Sort.t,
        p: Piece.t,
      )
      : list(Node.t) => {
    switch (p) {
    | Tile(t) =>
      of_tile(
        buffer_ids,
        font_metrics,
        global_inference_info,
        expected_sort,
        t,
      )
    | Grout(g) => of_grout(~font_metrics, ~global_inference_info, g.id)
    | Secondary({content, _}) =>
      of_secondary((content, M.settings.secondary_icons, m(p).last.col))
    };
  }
  and of_piece =
      (
        buffer_ids: list(Uuidm.t),
        font_metrics: FontMetrics.t,
        global_inference_info: InferenceResult.global_inference_info,
        expected_sort: Sort.t,
        p: Piece.t,
      )
      : list(Node.t) => {
    /* Last two elements of arg track the functorial args which
       can effect the code layout; without these the first,
       indentation can get out of sync */
    let arg = (
      expected_sort,
      p,
      m(p).last.col,
      M.settings,
      font_metrics,
      global_inference_info,
    );
    try(Hashtbl.find(piece_hash, arg)) {
    | _ =>
      let res =
        of_piece'(
          buffer_ids,
          font_metrics,
          global_inference_info,
          expected_sort,
          p,
        );
      Hashtbl.add(piece_hash, arg, res);
      res;
    };
  }
  and of_tile =
      (
        buffer_ids,
        font_metrics,
        global_inference_info,
        expected_sort: Sort.t,
        t: Tile.t,
      )
      : list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          //TODO(andrew): more subtle logic about sort acceptability
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    let is_in_buffer = List.mem(t.id, buffer_ids);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(of_delim(is_in_buffer, is_consistent, t), ((seg, sort)) =>
         of_segment(
           buffer_ids,
           false,
           sort,
           font_metrics,
           global_inference_info,
           seg,
         )
       )
    |> List.concat;
  };
};

let rec holes =
        (
          ~font_metrics,
          ~global_inference_info,
          ~map: Measured.t,
          seg: Segment.t,
        )
        : list(Node.t) =>
  seg
  |> List.concat_map(
       fun
       | Piece.Secondary(_) => []
       | Tile(t) =>
         List.concat_map(
           holes(~global_inference_info, ~map, ~font_metrics),
           t.children,
         )
       | Grout(g) => {
           switch (
             InferenceView.svg_display_settings(~global_inference_info, g.id)
           ) {
           | Some(svg_style) => [
               EmptyHoleDec.view(
                 ~font_metrics, // TODO(d) fix sort
                 svg_style,
                 {
                   measurement: Measured.find_g(g, map),
                   mold: Mold.of_grout(g, Any),
                 },
               ),
             ]
           | None => []
           };
         },
     );

let simple_view =
    (
      ~unselected,
      ~map,
      ~font_metrics,
      ~global_inference_info,
      ~settings: Settings.t,
    )
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
          [],
          false,
          Sort.Any,
          font_metrics,
          global_inference_info,
          unselected,
        ),
      ),
    ],
  );
};

let view =
    (
      ~buffer_ids: list(Uuidm.t),
      ~sort: Sort.t,
      ~font_metrics: FontMetrics.t,
      ~segment,
      ~unselected,
      ~measured,
      ~global_inference_info,
      ~settings: Settings.t,
    )
    : Node.t => {
  module Text =
    Text({
      let map = measured;
      let settings = settings;
    });
  let unselected =
    Text.of_segment(
      buffer_ids,
      false,
      sort,
      font_metrics,
      global_inference_info,
      unselected,
    );
  let holes =
    holes(~font_metrics, ~global_inference_info, ~map=measured, segment);
  div(
    ~attr=Attr.class_("code"),
    [span_c("code-text", unselected), ...holes],
  );
};
