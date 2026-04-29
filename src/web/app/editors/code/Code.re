open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;
open Util.WebUtil;

/* Helpers for rendering code text with holes and syntax highlighting */

let is_ref = (token: string, sort: Sort.t) =>
  sort != Pat
  && sort != TPat
  && !Token.is_keyword(token)
  && !Token.is_base_typ(token)
  && Token.is_typ_var(token);

let of_delim' =
  Core.Memo.general(
    ~cache_size_bound=10000,
    (
      token: string,
      plurality: int,
      sort: Sort.t,
      is_consistent: bool,
      is_in_buffer: bool,
      is_complete: bool,
      is_infix_var: bool,
      font_metrics: FontMetrics.t,
    ): t => {
      let base_cls =
        switch (token) {
        | _ when !is_consistent => "sort-inconsistent"
        | _ when !is_complete => "incomplete"
        | _ when Token.is_llm_hole(token) => "llm-waiting"
        | _ when Token.is_explicit_hole(token) => "explicit-hole"
        | _ when Token.is_string(token) => "string-lit"
        | _ when is_infix_var => "Any" /* Budget error deco */
        | _ => Sort.class_of(sort)
        };
      let plurality = plurality == 1 ? "mono" : "poly";
      let in_buffer = is_in_buffer ? ["in-parsed-buffer"] : [];
      let var_class = is_ref(token, sort) ? ["ref"] : [];
      let keyword_class = Token.is_keyword(token) ? ["keyword"] : [];
      span(
        ~attrs=[
          Attr.classes(
            ["token", base_cls, plurality]
            @ in_buffer
            @ var_class
            @ keyword_class,
          ),
        ],
        /* Currently only supporting emojis in strings; this is a
           conservative choice to guard against perf regressions;
           it can likely be relaxed. See also Token.bounding_box */
        base_cls == "string-lit"
          ? GraphemeView.render(~font_metrics, token) : [text(token)],
      );
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
      ~refractor_shape_map: Id.Map.t(_),
      ~font_metrics: FontMetrics.t,
      ~term_data: TermData.t,
      /* `refine_sort` lets the caller refine a tile's syntactic mold-out sort
         using information unavailable at this purely syntactic layer (e.g.
         statics refining `Drv(Exp)` to `Drv(Jdmt)`/`Drv(Ctx)`/`Drv(Prop)`).
         The default leaves the mold sort unchanged. */
      ~refine_sort: (Id.t, Sort.t) => Sort.t=(_, sort) => sort,
      ~buffer_ids: list(Id.t),
      segment: Segment.t,
    ) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let lb_icon = settings.secondary_icons ? "⏎" : "";
  let ws_icon = settings.secondary_icons ? "·" : " ";

  let sort = (t: Tile.t): Sort.t => refine_sort(t.id, t.mold.out);

  let is_consistent = (sort: Sort.t, t: Tile.t) =>
    switch (Id.Map.find_opt(t.id, term_data)) {
    | None => true
    | Some(data) =>
      switch (sort, data.sort) {
      | (Any, _)
      | (_, Any) => true
      | (Rul, Exp) => true
      | (Exp, Rul) => true
      /* All Drv(_) sub-sorts (Jdmt/Ctx/Prop/Exp) are treated as mutually
         consistent for highlighting purposes. term_data carries the sort
         the parser assigned (always the collapsed Drv(Exp) for these), so
         strict sort equality with the statics-refined sort would spuriously
         flag judgments/contexts/propositions as inconsistent. */
      | (Drv(_), _) => true
      | _ => sort == data.sort
      }
    };

  let of_delim = (t: Piece.tile, i: int): t => {
    let sort = sort(t);
    of_delim'(
      List.nth(t.label, i),
      List.length(t.label),
      sort,
      is_consistent(sort, t),
      List.mem(t.id, buffer_ids),
      Tile.is_complete(t),
      Mold.is_infix_op(t.mold)
      && Form.is_infix_delimiter_op_prefix(List.nth(t.label, i)),
      font_metrics,
    );
  };

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
    /* Read-only viewers (e.g. agent context) pass an empty shape map; folds
       would render as invisible whitespace. Show the standard fold glyph. */
    switch (pr.kind) {
    | ProjectorCore.Kind.Fold when Id.Map.is_empty(shape_map) =>
      span(
        ~attrs=[Attr.classes(["token", "fold-projector", "mono"])],
        [text({|⋱|})],
      )
    | _ =>
      let indent = measure_of(Projector(pr)).last.col;
      let size = DeferredLinebreaks.of_projector(pr, shape_map);
      let token =
        whitespace_token(size.row, size.row == 0 ? size.col : indent);
      Node.text(token);
    };
  };

  /* Hole decoration helpers */

  let thick_hole_dec = (~half_offset=false, shape: Nib.Shape.t): Node.t => {
    let dec = EmptyHoleDec.view(font_metrics, shape);
    let style =
      half_offset
        ? Printf.sprintf("left: %fpx;", -. font_metrics.col_width /. 2.) : "";
    Node.span(
      ~attrs=[Attr.classes(["virtual-grout"]), Attr.create("style", style)],
      [dec],
    );
  };

  let thin_hole_dec = (shape: Nib.Shape.t): Node.t => {
    let dec = EmptyHoleDec.view_thin(font_metrics, shape);
    Node.span(~attrs=[Attr.classes(["virtual-grout"])], [dec]);
  };

  /* Classify secondaries for placement logic */
  let is_linebreak = (s: Secondary.t) =>
    switch (s.content) {
    | Whitespace(str) when str == Token.linebreak => true
    | _ => false
    };

  let is_space = (s: Secondary.t) =>
    switch (s.content) {
    | Whitespace(str) when str == Token.space => true
    | _ => false
    };

  let is_comment = (s: Secondary.t) =>
    switch (s.content) {
    | Comment(_) => true
    | _ => false
    };

  let is_buffer = (s: Secondary.t) => List.mem(s.id, buffer_ids);

  /* Take the leading contiguous run of spaces from a secondary list */
  let rec take_spaces =
          (acc: list(Secondary.t), rest: list(Secondary.t))
          : (list(Secondary.t), list(Secondary.t)) =>
    switch (rest) {
    | [s, ...rest'] when is_space(s) => take_spaces([s, ...acc], rest')
    | _ => (List.rev(acc), rest)
    };

  /* Given a conflict and the boundary whitespace run, produce
   * the interleaved secondary nodes + decoration node.
   *
   * ~at_boundary: true when this conflict is at the leading or
   * trailing edge of the segment (start/end of program or child).
   * Boundary conflicts have "free space" beyond the segment edge,
   * so empty runs and lone linebreaks use thick deco instead of thin.
   *
   * See virtual-grout plan for placement policy. */
  /* Split leading buffer secondaries from the rest */
  let rec take_buffer =
          (acc: list(Secondary.t), rest: list(Secondary.t))
          : (list(Secondary.t), list(Secondary.t)) =>
    switch (rest) {
    | [s, ...rest'] when is_buffer(s) => take_buffer([s, ...acc], rest')
    | _ => (List.rev(acc), rest)
    };

  let place_decoration =
      (~at_boundary=false, hole_shape: Nib.Shape.t, secs: list(Secondary.t))
      : list(Node.t) => {
    /* Buffer secondaries (autocomplete suggestions) are visually part
     * of the preceding token. Strip them and emit before the decoration. */
    let (buffer_secs, secs) = take_buffer([], secs);
    let buffer_nodes = List.map(of_secondary, buffer_secs);
    let sec_nodes = () => List.map(of_secondary, secs);
    buffer_nodes
    @ (
      switch (secs) {
      /* Empty run or comment-first */
      | []
      | [{content: Comment(_), _}, ..._] =>
        /* At boundary: free space available, use thick deco.
         * Mid-segment: tiles directly adjacent, use thin deco. */
        if (at_boundary) {
          [thick_hole_dec(hole_shape), ...sec_nodes()];
        } else {
          [thin_hole_dec(hole_shape), ...sec_nodes()];
        }
      /* Linebreak-first cases */
      | [first, ...rest] when is_linebreak(first) =>
        switch (rest) {
        | [] when at_boundary =>
          /* Linebreak only at top-level trailing boundary: next line */
          [of_secondary(first), thick_hole_dec(hole_shape)]
        | [] =>
          /* Linebreak only: thick deco at end of previous line */
          [thick_hole_dec(hole_shape), of_secondary(first)]
        | [next, ..._] when is_comment(next) =>
          /* Linebreak + comment: thick deco at end of previous line */
          [thick_hole_dec(hole_shape), ...sec_nodes()]
        | [next, ..._] when is_space(next) || is_linebreak(next) =>
          /* Linebreak + space/linebreak: thick deco on next line */
          [of_secondary(first), thick_hole_dec(hole_shape)]
          @ List.map(of_secondary, rest)
        | _ =>
          /* Fallback: thick deco at end of previous line */
          [thick_hole_dec(hole_shape), ...sec_nodes()]
        }
      /* Space-first: center thick deco in contiguous space run */
      | _ when is_space(List.hd(secs)) =>
        let (spaces, non_spaces) = take_spaces([], secs);
        let n = List.length(spaces);
        let mid = n / 2;
        let is_even = n mod 2 == 0;
        let space_nodes = List.map(of_secondary, spaces);
        let non_space_nodes = List.map(of_secondary, non_spaces);
        let (before, _mid_node, after) =
          ListUtil.split_nth(mid, space_nodes);
        let deco = thick_hole_dec(~half_offset=is_even, hole_shape);
        before @ [deco, _mid_node] @ after @ non_space_nodes;
      /* Unknown: thick deco */
      | _ => [thick_hole_dec(hole_shape), ...sec_nodes()]
      }
    );
  };

  /* Emit accumulated secondaries, with optional decoration interleaved */
  let emit_pending =
      (
        ~at_boundary=false,
        conflict: option(Nib.Shape.t),
        pending_secs_rev: list(Secondary.t),
      )
      : list(Node.t) => {
    let secs = List.rev(pending_secs_rev);
    switch (conflict) {
    | None => List.map(of_secondary, secs)
    | Some(hole_shape) => place_decoration(~at_boundary, hole_shape, secs)
    };
  };

  let rec of_segment = (~top_level=false, seg: Segment.t): list(Node.t) => {
    /* Walk segment detecting shape conflicts at boundaries.
     * Secondaries are deferred until we hit a tile/projector/end,
     * so we can analyze the boundary whitespace run for placement.
     * at_boundary is only true for the trailing edge (free space
     * beyond segment end); leading conflicts always use thin/compact. */
    let boundary = Nib.Shape.concave();
    let (nodes_rev, prev_r, pending_secs_rev, _) =
      List.fold_left(
        ((nodes, prev_r, pending_secs_rev, _at_leading), p: Piece.t) =>
          switch (p) {
          | Secondary(s) => (nodes, prev_r, [s, ...pending_secs_rev], false)
          | Tile(t) =>
            let (l_shape, r_shape) = Tile.shapes(t);
            let conflict: option(Nib.Shape.t) =
              if (Nib.Shape.fits(prev_r, l_shape)) {
                None;
              } else {
                Some(Nib.Shape.flip(prev_r));
              };
            let sec_nodes = emit_pending(conflict, pending_secs_rev);
            let _ =
              switch (Id.Map.find_opt(t.id, refractor_shape_map)) {
              | Some(_) =>
                DeferredLinebreaks.update(2) |> ignore;
                ();
              | None => ()
              };
            let tile_nodes =
              Aba.mk(t.shards, t.children)
              |> Aba.join(i => [of_delim(t, i)], of_segment)
              |> List.concat;
            ([tile_nodes, sec_nodes, ...nodes], r_shape, [], false);
          | Projector(pr) =>
            let (l_shape, r_shape) = ProjectorCore.shapes(pr);
            let conflict: option(Nib.Shape.t) =
              if (Nib.Shape.fits(prev_r, l_shape)) {
                None;
              } else {
                Some(Nib.Shape.flip(prev_r));
              };
            let sec_nodes = emit_pending(conflict, pending_secs_rev);
            (
              [[of_projector(pr)], sec_nodes, ...nodes],
              r_shape,
              [],
              false,
            );
          },
        ([], boundary, [], false),
        seg,
      );
    /* Check trailing boundary for conflict */
    let trailing_conflict: option(Nib.Shape.t) =
      if (Nib.Shape.fits(prev_r, boundary)) {
        None;
      } else {
        Some(Nib.Shape.flip(prev_r));
      };
    let trailing_nodes =
      emit_pending(
        ~at_boundary=top_level,
        trailing_conflict,
        pending_secs_rev,
      );
    [trailing_nodes, ...nodes_rev] |> List.rev |> List.concat;
  };

  of_segment(~top_level=true, segment);
};
