open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

/* Helpers for rendering code text with holes and syntax highlighting */

/* Tab projectors add linebreaks after the end of their line */
let deferred_linebreaks: ref(list(int)) = ref([]);

let consume_deferred_linebreaks = () => {
  let max_deferred_linebreaks = List.fold_left(max, 0, deferred_linebreaks^);
  deferred_linebreaks := [];
  max_deferred_linebreaks;
};

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    ((label, is_in_buffer, sort, is_consistent, is_complete, indent, i)) => {
      let cls =
        switch (label) {
        | _ when is_in_buffer => "in-buffer"
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when s == Form.explicit_hole => "explicit-hole"
        | [s] when Form.is_string(s) => "string-lit"
        | _ => Sort.to_string(sort)
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      //let label = is_in_buffer ? AssistantExpander.mark(label) : label;
      let token = List.nth(label, i);
      /* Add indent to multiline tokens: */
      let num_lb = StringUtil.num_linebreaks(token);
      let token =
        num_lb == 0
          ? token : token ++ StringUtil.repeat(indent, Unicode.nbsp);
      //TODO: deffered linebreaks
      [
        span(
          ~attrs=[Attr.classes(["token", cls, plurality])],
          [Node.text(token)],
        ),
      ];
    },
  );
let of_delim =
    (is_in_buffer, is_consistent, indent, t: Piece.tile, i: int)
    : list(Node.t) =>
  of_delim'((
    t.label,
    is_in_buffer,
    t.mold.out,
    is_consistent,
    Tile.is_complete(t),
    indent,
    i,
  ));

let space = " "; //Unicode.nbsp;

let of_grout = [Node.text(space)];

let of_secondary =
    //Core.Memo.general(  ~cache_size_bound=10000,
    ((content, secondary_icons, indent)) =>
  if (String.equal(Secondary.get_string(content), Form.linebreak)) {
    let str = secondary_icons ? ">" : "";
    [span_c("linebreak", [text(str)])]
    @ List.init(1 + consume_deferred_linebreaks(), _ => Node.text("\n"))
    @ [Node.text(StringUtil.repeat(indent, space))];
  } else if (String.equal(Secondary.get_string(content), Form.space)) {
    let str = secondary_icons ? "·" : space;
    [span_c("whitespace", [text(str)])];
  } else if (Secondary.content_is_comment(content)) {
    [span_c("comment", [Node.text(Secondary.get_string(content))])];
  } else {
    [span_c("secondary", [Node.text(Secondary.get_string(content))])];
  };
//);

let of_projector = (expected_sort, indent, shape: Base.shape) => {
  let token =
    switch (shape) {
    | Inline(_) => Projector.token_of_shape(shape)
    | NewInline({row: height, _}) =>
      let num_lb = height - 1;
      if (num_lb > 0) {
        deferred_linebreaks := [num_lb, ...deferred_linebreaks^];
      };
      Projector.token_of_shape(shape);
    | Block({row: height, _}) =>
      let num_lb = height - 1;
      num_lb == 0
        ? ""
        : String.make(consume_deferred_linebreaks(), '\n')
          ++ Projector.token_of_shape(shape);
    };
  of_delim'(([token], false, expected_sort, true, true, indent, 0));
};

module Text =
       (
         M: {
           let map: Measured.t;
           let settings: Settings.Model.t;
           let shape_of_proj: Base.projector => Base.shape;
         },
       ) => {
  deferred_linebreaks := [];

  let m = p => Measured.find_p(~msg="Text", p, M.map);
  let rec of_segment =
          (buffer_ids, no_sorts, sort, seg: Segment.t): list(Node.t) => {
    /* note: no_sorts flag is used for backpack view;
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
         of_piece(buffer_ids, sort_of_p_idx(i), p)
       );
  }
  and of_piece =
      (buffer_ids, expected_sort: Sort.t, p: Piece.t): list(Node.t) => {
    switch (p) {
    | Tile(t) => of_tile(buffer_ids, expected_sort, t)
    | Grout(_) => of_grout
    | Secondary({content, _}) =>
      of_secondary((content, M.settings.secondary_icons, m(p).last.col))
    | Projector(p) =>
      of_projector(
        expected_sort,
        m(Projector(p)).origin.col,
        M.shape_of_proj(p),
      )
    };
  }
  and of_tile = (buffer_ids, expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let is_consistent = Sort.consistent(t.mold.out, expected_sort);
    let is_in_buffer = List.mem(t.id, buffer_ids);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(
         of_delim(is_in_buffer, is_consistent, m(Tile(t)).origin.col, t),
         ((seg, sort)) =>
         of_segment(buffer_ids, false, sort, seg)
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
       | Projector(_) => []
       | Tile(t) => List.concat_map(holes(~map, ~font_metrics), t.children)
       | Grout(g) => [
           EmptyHoleDec.view(
             ~font_metrics, // TODO(d) fix sort
             {
               measurement: Measured.find_g(~msg="Code.holes", g, map),
               mold: Mold.of_grout(g, Any),
             },
           ),
         ],
     );

let simple_view = (~font_metrics, ~segment, ~settings: Settings.t): Node.t => {
  let shape_of_proj = _ => Base.Inline(0); /* Assume this doesn't contain projectors */
  let map = Measured.of_segment(segment, shape_of_proj);
  module Text =
    Text({
      let map = map;
      let settings = settings;
      let shape_of_proj = shape_of_proj;
    });
  let holes = holes(~map, ~font_metrics, segment);
  div(
    ~attrs=[Attr.class_("code")],
    [
      span_c("code-text", Text.of_segment([], false, Sort.Any, segment)),
      ...holes,
    ],
  );
};

let of_hole = (~globals: Globals.t, ~measured, g: Grout.t) =>
  // TODO(d) fix sort
  EmptyHoleDec.view(
    ~font_metrics=globals.font_metrics,
    {
      measurement: Measured.find_g(~msg="Code.of_hole", g, measured),
      mold: Mold.of_grout(g, Any),
    },
  );
