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
      (
        token: string,
        plurality: int,
        sort: Sort.t,
        is_consistent: bool,
        is_in_buffer: bool,
        is_complete: bool,
        is_infix_var: bool,
      ),
    ) => {
      let base_cls =
        switch (token) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | _ when Token.is_llm_hole(token) => "llm-waiting"
        | _ when Token.is_explicit_hole(token) => "explicit-hole"
        | _ when Token.is_string(token) => "string-lit"
        | _ when is_infix_var => "Any" /* Budget error deco */
        | _ => Sort.to_string(sort)
        };
      let plurality = plurality == 1 ? "mono" : "poly";
      let in_buffer = is_in_buffer ? ["in-parsed-buffer"] : [];
      [
        span(
          ~attrs=[Attr.classes(["token", base_cls, plurality] @ in_buffer)],
          [Node.text(token)],
        ),
      ];
    },
  );

let secondary_text =
  Core.Memo.general(~cache_size_bound=10000, (cls, str) =>
    span_c(cls, [text(str)])
  );

let whitespace_token =
  Core.Memo.general(~cache_size_bound=10000, (row, col) =>
    String.make(row, '\n') ++ String.make(col, ' ')
  );

let view =
    (
      ~measured: Measured.t,
      ~settings: Settings.Model.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
      ~term_data: TermData.t,
      ~buffer_ids: list(Id.t),
      segment: Segment.t,
    ) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let g_convex = EmptyHoleDec.view(font_metrics, Convex);
  let g_concave = EmptyHoleDec.view(font_metrics, Concave);

  let of_grout = (g: Grout.t): t => {
    switch (g.shape) {
    | Convex => g_convex
    | Concave => g_concave
    };
  };

  let lb_icon = settings.secondary_icons ? "⏎" : "";
  let ws_icon = settings.secondary_icons ? "·" : " ";

  let is_consistent = (t: Tile.t) =>
    switch (Id.Map.find_opt(t.id, term_data)) {
    | None => true
    | Some(data) =>
      switch (t.mold.out, data.sort) {
      | (Any, _)
      | (_, Any) => true
      | (Rul, Exp) => true
      | (Exp, Rul) => true
      | _ => t.mold.out == data.sort
      }
    };

  let of_delim = (t: Piece.tile, i: int): list(Node.t) =>
    of_delim'((
      List.nth(t.label, i),
      List.length(t.label),
      t.mold.out,
      is_consistent(t),
      List.mem(t.id, buffer_ids),
      Tile.is_complete(t),
      Mold.is_infix_op(t.mold)
      && Form.is_infix_delimiter_op_prefix(List.nth(t.label, i)),
    ));

  let measure_of = p => Measured.find_p(~msg="Text", p, measured);

  let of_secondary = (secondary: Secondary.t) =>
    switch (secondary.content) {
    | Whitespace(str) when str == Token.linebreak =>
      let indent = measure_of(Secondary(secondary)).last.col;
      let token = whitespace_token(DeferredLinebreaks.of_secondary(), indent);
      Node.text(lb_icon ++ token);
    | Whitespace(str) when str == Token.space => Node.text(ws_icon)
    | Whitespace(_) => failwith("Code: Unrecognized Secondary")
    | Comment(str) when List.mem(secondary.id, buffer_ids) =>
      secondary_text("in-unparsed-buffer", str)
    | Comment(str) => secondary_text("comment", str)
    };

  let of_projector = (pr: Base.projector) => {
    let indent = measure_of(Projector(pr)).origin.col;
    let size = DeferredLinebreaks.of_projector(pr, shape_map);
    let token =
      whitespace_token(size.row, size.col + (size.row == 0 ? 0 : indent));
    Node.text(token);
  };

  let rec of_segment = (seg: Segment.t): list(Node.t) =>
    List.concat_map(
      fun
      | Piece.Tile(t) =>
        Aba.mk(t.shards, t.children)
        |> Aba.join(of_delim(t), of_segment)
        |> List.concat
      | Grout(g) => [of_grout(g)]
      | Secondary(s) => [of_secondary(s)]
      | Projector(pr) => [of_projector(pr)],
      seg,
    );

  of_segment(segment);
};
