open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.WebUtil;

/* Helpers for rendering code text with holes and syntax highlighting */

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    (
      (label, sort, is_consistent, is_in_buffer, is_complete, is_infix_var, i),
    ) => {
      let cls =
        switch (label) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when Token.is_llm_hole(s) => "llm-waiting"
        | [s] when Token.is_explicit_hole(s) => "explicit-hole"
        | [s] when Token.is_string(s) => "string-lit"
        | _ when is_infix_var => "Any" /* Budget error deco */
        | _ => Sort.to_string(sort)
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      let token = List.nth(label, i);
      let in_buffer = is_in_buffer ? ["in-parsed-buffer"] : [];
      [
        span(
          ~attrs=[Attr.classes(["token", cls, plurality] @ in_buffer)],
          [Node.text(token)],
        ),
      ];
    },
  );
let of_delim =
    (is_consistent, is_in_buffer, t: Piece.tile, i: int): list(Node.t) =>
  of_delim'((
    t.label,
    t.mold.out,
    is_consistent,
    is_in_buffer,
    Tile.is_complete(t),
    Mold.is_infix_op(t.mold)
    && Form.is_infix_delimiter_op_prefix(List.nth(t.label, i)),
    i,
  ));

let secondary_text =
  Core.Memo.general(~cache_size_bound=10000, (cls, str) =>
    span_c(cls, [text(str)])
  );

let whitespace_token = (~row: int, ~col: int): string =>
  String.make(row, '\n') ++ String.make(col, ' ');

module Text =
       (
         M: {
           let map: Measured.t;
           let settings: Settings.Model.t;
           let shape_map: ProjectorCore.Shape.Map.t;
           let font_metrics: FontMetrics.t;
         },
       ) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let g_convex = EmptyHoleDec.view(M.font_metrics, Convex);
  let g_concave = EmptyHoleDec.view(M.font_metrics, Concave);

  let of_grout = (g: Grout.t): list(Node.t) => {
    switch (g.shape) {
    | Convex => [g_convex]
    | Concave => [g_concave]
    };
  };

  let lb_icon =
    M.settings.secondary_icons ? [secondary_text("linebreak", ">")] : [];
  let ws_icon = [
    M.settings.secondary_icons
      ? secondary_text("whitespace", "·") : Node.text(" "),
  ];

  let of_secondary =
      (
        (
          content: Secondary.secondary_content,
          indent: int,
          is_in_buffer: bool,
        ),
      ) =>
    switch (content) {
    | Whitespace(str) when str == Token.linebreak =>
      let token =
        whitespace_token(~row=DeferredLinebreaks.of_secondary(), ~col=indent);
      lb_icon @ [Node.text(token)];
    | Whitespace(str) when str == Token.space => ws_icon
    | Whitespace(_) => failwith("Code: Unrecognized Secondary")
    | Comment(str) when is_in_buffer => [
        secondary_text("in-unparsed-buffer", str),
      ]
    | Comment(str) => [secondary_text("comment", str)]
    };

  let of_projector =
      (indent, p: Base.projector, shape_map: Id.Map.t(ProjectorCore.Shape.t)) => {
    let size = DeferredLinebreaks.of_projector(p, shape_map);
    let token =
      whitespace_token(
        ~row=size.row,
        ~col=size.col + (size.row == 0 ? 0 : indent),
      );
    [Node.text(token)];
  };

  let m = p => Measured.find_p(~msg="Text", p, M.map);
  let rec of_segment = (buffer_ids, sort, seg: Segment.t): list(Node.t) => {
    let expected_sorts = Segment.expected_sorts(sort, seg);
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
    | Grout(g) => of_grout(g)
    | Secondary({content, id}) =>
      let indent = m(p).last.col;
      let is_in_buffer = List.mem(id, buffer_ids);
      of_secondary((content, indent, is_in_buffer));
    | Projector(pr) =>
      let indent = m(p).origin.col;
      of_projector(indent, pr, M.shape_map);
    };
  }
  and of_tile = (buffer_ids, expected_sort: Sort.t, t: Tile.t): list(Node.t) => {
    let children_and_sorts =
      List.mapi(
        (i, (l, child, r)) =>
          (child, l + 1 == r ? List.nth(t.mold.in_, i) : Sort.Any),
        Aba.aba_triples(Aba.mk(t.shards, t.children)),
      );
    let consistent = (s: Sort.t, s': Sort.t) =>
      switch (s, s') {
      | (Any, _)
      | (_, Any) => true
      | (Rul, Exp) => true
      | (Exp, Rul) => true
      | _ => s == s'
      };
    let is_consistent = consistent(t.mold.out, expected_sort);
    Aba.mk(t.shards, children_and_sorts)
    |> Aba.join(
         of_delim(is_consistent, List.mem(t.id, buffer_ids), t),
         ((seg, sort)) =>
         of_segment(buffer_ids, sort, seg)
       )
    |> List.concat;
  };
};
