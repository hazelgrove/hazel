open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.Web;

/* Helpers for rendering code text with holes and syntax highlighting */

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    ((label, sort, is_consistent, is_complete, indent, i)) => {
      let cls =
        switch (label) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when s == Form.explicit_hole => "explicit-hole"
        | [s] when Form.is_string(s) => "string-lit"
        | _ => Sort.to_string(sort)
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      let token = List.nth(label, i);
      /* Add indent to multiline tokens: */
      let token =
        StringUtil.num_linebreaks(token) == 0
          ? token : token ++ StringUtil.repeat(indent, Unicode.nbsp);
      [
        span(
          ~attrs=[Attr.classes(["token", cls, plurality])],
          [Node.text(token)],
        ),
      ];
    },
  );
let of_delim = (is_consistent, indent, t: Piece.tile, i: int): list(Node.t) =>
  of_delim'((
    t.label,
    t.mold.out,
    is_consistent,
    Tile.is_complete(t),
    indent,
    i,
  ));

let space = " "; //Unicode.nbsp;

let of_secondary =
  Core.Memo.general(
    ~cache_size_bound=10000,
    ((content, secondary_icons, indent, is_in_buffer: bool)) =>
    if (is_in_buffer) {
      [span_c("in-buffer", [Node.text(Secondary.get_string(content))])];
    } else if (String.equal(Secondary.get_string(content), Form.linebreak)) {
      let str = secondary_icons ? ">" : "";
      [
        span_c("linebreak", [text(str)]),
        Node.text("\n"),
        Node.text(StringUtil.repeat(indent, space)),
      ];
    } else if (String.equal(Secondary.get_string(content), Form.space)) {
      let str = secondary_icons ? "·" : space;
      [span_c("whitespace", [text(str)])];
    } else if (Secondary.content_is_comment(content)) {
      [span_c("comment", [Node.text(Secondary.get_string(content))])];
    } else {
      [span_c("secondary", [Node.text(Secondary.get_string(content))])];
    }
  );

let of_projector = (expected_sort, indent, token) =>
  of_delim'(([token], expected_sort, true, true, indent, 0));

module Text =
       (
         M: {
           let map: Measured.t;
           let settings: Settings.Model.t;
           let shape_of_proj: Base.projector => ProjectorCore.shape;
           let font_metrics: FontMetrics.t;
         },
       ) => {
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
    | Grout(g) => [EmptyHoleDec.view(M.font_metrics, g.shape)]
    | Secondary({content, id}) =>
      let is_in_buffer = List.mem(id, buffer_ids);
      of_secondary((
        content,
        M.settings.secondary_icons,
        m(p).last.col,
        is_in_buffer,
      ));
    | Projector(p) =>
      of_projector(
        expected_sort,
        m(Projector(p)).origin.col,
        p |> M.shape_of_proj |> ProjectorCore.token,
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
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(
         of_delim(is_consistent, m(Tile(t)).origin.col, t), ((seg, sort)) =>
         of_segment(buffer_ids, false, sort, seg)
       )
    |> List.concat;
  };
};

let simple_view = (font_metrics, sort, segment): Node.t => {
  /* Assume this doesn't contain projectors */
  let shape_of_proj = ProjectorInfo.Shape.of_map_default;
  let map = Measured.of_segment(segment, shape_of_proj);
  module Text =
    Text({
      let map = map;
      let settings = Settings.Model.init;
      let shape_of_proj = shape_of_proj;
      let font_metrics = font_metrics;
    });
  div(
    ~attrs=[Attr.class_("code")],
    [span_c("code-text", Text.of_segment([], false, sort, segment))],
  );
};
