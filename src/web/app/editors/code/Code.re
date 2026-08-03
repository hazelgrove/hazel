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

let render_string_with_escapes =
    (~font_metrics: FontMetrics.t, ~is_raw: bool=false, token: string)
    : list(t) => {
  let body =
    if (is_raw) {
      Token.strip_raw_quotes(token);
    } else {
      Token.strip_quotes(token);
    };
  let len = String.length(body);

  /* For raw strings, don't parse escape sequences */
  if (is_raw) {
    let open_q = text("r\"");
    let close_q = text("\"");
    let inner_nodes = GraphemeView.render(~font_metrics, body);
    [open_q, ...inner_nodes] @ [close_q];
  } else {
    let rec split =
            (i: int, acc: list((bool, string))): list((bool, string)) =>
      if (i >= len) {
        List.rev(acc);
      } else if (body.[i] != '\\') {
        let j = ref(i);
        while (j.contents < len && body.[j.contents] != '\\') {
          j := j.contents + 1;
        };
        let piece = String.sub(body, i, j.contents - i);
        split(j.contents, [(false, piece), ...acc]);
      } else if
        /* body.[i] == '\\' */
        (i + 1 >= len) {
        split(i + 1, [(true, "\\"), ...acc]);
      } else if (body.[i + 1] == 'u' && i + 2 < len && body.[i + 2] == '{') {
        let k = ref(i + 3);
        while (k.contents < len && body.[k.contents] != '}') {
          k := k.contents + 1;
        };
        let esc =
          if (k.contents < len) {
            String.sub(body, i, k.contents - i + 1);
          } else {
            String.sub(body, i, len - i);
          };
        split(
          k.contents < len ? k.contents + 1 : len,
          [(true, esc), ...acc],
        );
      } else if (body.[i + 1] == 'x' && i + 3 < len) {
        let esc = String.sub(body, i, 4); /* \xNN */
        split(i + 4, [(true, esc), ...acc]);
      } else {
        let esc = String.sub(body, i, 2); /* backslash + next char */
        split(i + 2, [(true, esc), ...acc]);
      };

    let pieces = split(0, []);

    let open_q = text("\"");
    let close_q = text("\"");

    let inner_nodes =
      pieces
      |> List.concat_map(((is_esc, s)) =>
           if (is_esc) {
             [span(~attrs=[Attr.classes(["escape"])], [text(s)])];
           } else {
             GraphemeView.render(~font_metrics, s);
           }
         );

    [open_q, ...inner_nodes] @ [close_q];
  };
};

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
        | _ when Token.is_raw_string(token) => "raw-string-lit"
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
          ? render_string_with_escapes(~font_metrics, token)
          : base_cls == "raw-string-lit"
              ? render_string_with_escapes(
                  ~font_metrics,
                  ~is_raw=true,
                  token,
                )
              : [text(token)],
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

  of_segment(segment);
};
