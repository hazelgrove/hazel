open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.WebUtil;

/* Helpers for rendering code text with holes and syntax highlighting */

/* Tab projectors add linebreaks after the end of their line */
let deferred_linebreaks: ref(int) = ref(0);

let consume_deferred_linebreaks = (): int => {
  let ret = deferred_linebreaks^;
  deferred_linebreaks := 0;
  ret;
};

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    (
      (
        label,
        sort,
        is_consistent,
        is_in_buffer,
        is_complete,
        is_infix_var,
        indent,
        i,
      ),
    ) => {
      let cls =
        switch (label) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | [s] when Form.is_llm_hole(s) => "llm-waiting"
        | [s] when s == Form.explicit_hole => "explicit-hole"
        | [s] when Form.is_string(s) => "string-lit"
        | _ when is_infix_var => "Any" /* Budget error deco */
        | _ => Sort.to_string(sort)
        };
      let plurality = List.length(label) == 1 ? "mono" : "poly";
      let token = List.nth(label, i);
      /* Add indent to multiline tokens: */
      let num_lb = StringUtil.num_linebreaks(token);
      let token =
        num_lb == 0
          ? token : token ++ StringUtil.repeat(indent, Unicode.nbsp);
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
    (is_consistent, is_in_buffer, indent, t: Piece.tile, i: int)
    : list(Node.t) =>
  of_delim'((
    t.label,
    t.mold.out,
    is_consistent,
    is_in_buffer,
    Tile.is_complete(t),
    Mold.is_infix_op(t.mold)
    && Form.is_infix_delimiter_op_prefix(List.nth(t.label, i)),
    indent,
    i,
  ));

let space = " "; //Unicode.nbsp;

let secondary_text =
  Core.Memo.general(~cache_size_bound=10000, (cls, str) =>
    span_c(cls, [text(str)])
  );

let of_secondary =
    (
      (
        content: Secondary.secondary_content,
        secondary_icons: bool,
        indent: int,
        is_in_buffer: bool,
      ),
    ) =>
  switch (content) {
  | Whitespace(str) when str == Form.linebreak =>
    [secondary_text("linebreak", secondary_icons ? ">" : "")]
    @ List.init(1 + consume_deferred_linebreaks(), _ => Node.text("\n"))
    @ [Node.text(StringUtil.repeat(indent, space))]
  | Whitespace(str) when str == Form.space => [
      secondary_text("whitespace", secondary_icons ? "·" : space),
    ]
  | Whitespace(_) => failwith("Code: Unrecognized Secondary")
  | Comment(str) when is_in_buffer => [
      secondary_text("in-unparsed-buffer", str),
    ]
  | Comment(str) => [secondary_text("comment", str)]
  };

let of_projector = (expected_sort, indent, shape: ProjectorCore.Shape.t) => {
  let token =
    switch (shape.vertical) {
    | Inline
    | Tab(0)
    | Block(0) => ProjectorCore.Shape.token(shape)
    | Tab(num_lb) =>
      deferred_linebreaks := max(num_lb, deferred_linebreaks^);
      ProjectorCore.Shape.token(shape);
    | Block(_) =>
      String.make(consume_deferred_linebreaks(), '\n')
      ++ ProjectorCore.Shape.token(shape)
    };
  of_delim'(([token], expected_sort, true, false, true, false, indent, 0));
};

module Text =
       (
         M: {
           let map: Measured.t;
           let settings: Settings.Model.t;
           let shape_map: ProjectorCore.Shape.Map.t;
           let font_metrics: FontMetrics.t;
         },
       ) => {
  deferred_linebreaks := 0;

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
    | Grout(g) => [EmptyHoleDec.view(M.font_metrics, g.shape)]
    | Secondary({content, id}) =>
      let indent = m(p).last.col;
      let is_in_buffer = List.mem(id, buffer_ids);
      of_secondary((
        content,
        M.settings.secondary_icons,
        indent,
        is_in_buffer,
      ));
    | Projector(p) =>
      of_projector(
        expected_sort,
        m(Projector(p)).origin.col,
        ProjectorCore.Shape.Map.lookup(p.id, M.shape_map),
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
         of_delim(
           is_consistent,
           List.mem(t.id, buffer_ids),
           m(Tile(t)).origin.col,
           t,
         ),
         ((seg, sort)) =>
         of_segment(buffer_ids, sort, seg)
       )
    |> List.concat;
  };
};
