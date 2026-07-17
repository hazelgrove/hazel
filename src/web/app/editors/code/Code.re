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
      /* ghost witness shard: render the completed-remainder styling the
         retired suggestion buffer used (fed only by ghost marks now) */
      is_in_buffer: bool,
      is_complete: bool,
      is_infix_var: bool,
      /* WITNESS sub-token: how many leading chars of `token` the user
         actually typed. -1 = not a witness (whole token). 0..len-1 =
         render the [0, typed_len) prefix normal and the remainder
         ghost (an incomplete-delimiter span continuing the token). */
      typed_len: int,
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
      /* string-lit rendering (grapheme-aware) never coincides with a
         witness (delimiters aren't strings), so the split path is
         plain text — safe for the caret overlay, which measures by
         token column, not DOM span count */
      let contents =
        if (typed_len >= 0
            && typed_len < String.length(token)
            && base_cls != "string-lit") {
          let typed = String.sub(token, 0, typed_len);
          let ghost =
            String.sub(token, typed_len, String.length(token) - typed_len);
          /* the typed prefix inherits the parent token's color (bare
             text, no wrapper); the remainder gets the ghost styling
             the retired suggestion buffer used */
          [text(typed), span_c("in-parsed-buffer", [text(ghost)])];
        } else {
          base_cls == "string-lit"
            ? GraphemeView.render(~font_metrics, token) : [text(token)];
        };
      span(
        ~attrs=[
          Attr.classes(
            ["token", base_cls, plurality]
            @ in_buffer
            @ var_class
            @ keyword_class,
          ),
        ],
        contents,
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
      /* (id, shard) marks of display-only ghost pieces spliced into the
         segment (CachedSyntax.ghost_marks); shard-precise so a ghost
         closer doesn't gray its tile's real opener */
      ~ghost_marks: list((Id.t, option(int)))=[],
      /* WITNESS sub-token styling: (tile id, shard idx) -> typed_len */
      ~typed_lens: list(((Id.t, int), int))=[],
      segment: Segment.t,
    ) => {
  module DeferredLinebreaks = Measured.MkDeferredLinebreaks();

  let g_convex = EmptyHoleDec.view(font_metrics, Convex);
  let g_concave = EmptyHoleDec.view(font_metrics, Concave);

  /* Node.t's None/Some shadow option's, so match structurally */
  let ghost_mark = (id: Id.t, shard: option(int)): bool =>
    List.exists(
      ((mid, msh): (Id.t, option(int))) =>
        Id.equal(mid, id) && msh == shard,
      ghost_marks,
    );

  let of_grout = (g: Grout.t): t => {
    let hole =
      switch (g.shape) {
      | Convex => g_convex
      | Concave => g_concave
      };
    ghost_mark(g.id, Option.none)
      ? span_c("in-parsed-buffer", [hole]) : hole;
  };

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

  /* a tile whose shards are only display-ghosts is still INCOMPLETE
     to the user — keep the incomplete-delimiter color it had before
     the ghost spliced in */
  let tile_ghosted = (t: Piece.tile): bool =>
    List.exists(
      ((mid, _): (Id.t, option(int))) => Id.equal(mid, t.id),
      ghost_marks,
    );

  let typed_len_of = (id: Id.t, i: int): int =>
    List.fold_left(
      (acc, ((tid, sh), n): ((Id.t, int), int)) =>
        Id.equal(tid, id) && sh == i ? n : acc,
      -1,
      typed_lens,
    );
  let of_delim = (t: Piece.tile, i: int): t => {
    let sort = sort(t);
    of_delim'(
      List.nth(t.label, i),
      List.length(t.label),
      sort,
      is_consistent(sort, t),
      ghost_mark(t.id, Option.some(i)),
      Tile.is_complete(t) && !tile_ghosted(t),
      Mold.is_infix_op(t.mold)
      && Form.is_infix_delimiter_op_prefix(List.nth(t.label, i)),
      typed_len_of(t.id, i),
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
    /* a ghost-marked comment is a witness-remainder ghost (spliced
       by DisplayFork), styled like the retired suggestion buffer */
    | Comment(str) when ghost_mark(secondary.id, Option.none) =>
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

  let rec of_segment = (seg: Segment.t): list(Node.t) =>
    List.concat_map(
      fun
      | Piece.Tile(t) => {
          let _ =
            switch (Id.Map.find_opt(t.id, refractor_shape_map)) {
            | Some(_) =>
              DeferredLinebreaks.update(2) |> ignore;
              ();
            | None => ()
            };
          Aba.mk(t.shards, t.children)
          |> Aba.join(i => [of_delim(t, i)], of_segment)
          |> List.concat;
        }
      | Grout(g) => [of_grout(g)]
      | Secondary(s) => [of_secondary(s)]
      | Projector(pr) => [of_projector(pr)],
      seg,
    );

  /* Trailing filler: a text layer ending in a linebreak gets no
     final line box from HTML, so an empty last line left the editor
     one row short (caret overhanging into the result area). The
     zero-width space forces the line box and is invisible (and
     harmless mid-line) otherwise. */
  of_segment(segment) @ [Node.text("\xe2\x80\x8b")];
};
