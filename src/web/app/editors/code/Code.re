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
        /* Wide clusters (emoji, CJK) need an explicit cell so the glyph
           occupies the two columns Measured gave it. Pure ASCII -- nearly
           every token -- skips straight to a text node. */
        Unicode.is_simple_ascii(token)
          ? [text(token)] : GraphemeView.render(~font_metrics, token),
      );
    },
  );

let secondary_text =
  Core.Memo.general(~cache_size_bound=10000, (cls, str) =>
    span_c(cls, [text(str)])
  );

/* Comments are measured in columns like any other text, so a comment with a
   wide cluster needs the same explicit cells as a token. */
let comment_text = (~font_metrics: FontMetrics.t, cls, str) =>
  Unicode.is_simple_ascii(str)
    ? secondary_text(cls, str)
    : span_c(cls, GraphemeView.render(~font_metrics, str));

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
      /* false for non-final chunks of a chunked render: their trailing
         linebreak is a real row boundary, not a hanging last row */
      ~reserve_trailing_row: bool=true,
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
      comment_text(~font_metrics, "in-unparsed-buffer", str)
    | Comment(str) => comment_text(~font_metrics, "comment", str)
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

  let nodes = of_segment(segment);
  /* a TRAILING linebreak produces no final line box in pre flow, so
     the editor came up one row short and the caret hung below it
     (worst in stacked cells, where the next cell sits right there):
     a zero-width space reserves the last row */
  switch (List.rev(segment)) {
  | [Secondary(s), ..._]
      when reserve_trailing_row && Secondary.is_linebreak(s) =>
    nodes @ [Node.text("\xe2\x80\x8b")]
  | _ => nodes
  };
};

/* ===== PER-CHUNK CODE TEXT (plans/subeditor-dataflow.md paragraph 5a)
   One inline span per measured chunk, memoized by anchor: unchanged
   chunks return the SAME vdom node, so the virtual-dom diff skips
   them by reference and an edit re-renders one chunk's tokens.
   Inline spans in pre flow reproduce the flat render exactly (the
   text, with its embedded linebreaks, flows identically).

   The memo key is CONTENT-based where identity churns per frame:
   term_data/info_map are rebuilt wholesale each parse/statics pass,
   so we key on the per-tile RENDER-RELEVANT projection (the refined
   sort and the term-data sort actually consulted by of_delim) and
   compare structurally. c_flat identity covers pieces + projector/
   refractor shape slices (Measured.Incr guarantees slice equality
   on reuse). Eviction: tick sweep (view-side cache discipline). */
module ChunkViews = {
  type entry = {
    mutable cv_flat: Obj.t, /* Measured.flat identity */
    mutable cv_final: bool,
    mutable cv_tiles: list(Tile.t), /* chunk tiles, cached off cv_flat */
    mutable cv_sorts: array((Sort.t, option(Sort.t))),
    /* info_map identity at the last sort probe: when it matches, the
       probe is skipped entirely — for unchanged pieces go_incr shares
       term_data values, so sorts can only change via new statics */
    mutable cv_info: Obj.t,
    mutable cv_buffer: Obj.t, /* buffer_ids identity (usually []) */
    mutable cv_fm: Obj.t,
    mutable cv_settings: Obj.t,
    mutable cv_node: Node.t,
    mutable cv_tick: int,
  };
  let cache: Hashtbl.t(Id.t, entry) = Hashtbl.create(64);
  let tick = ref(0);
  let sweep = () =>
    if (tick^ mod 64 == 0) {
      let dead =
        Hashtbl.fold(
          (a, e, acc) => e.cv_tick < tick^ - 16 ? [a, ...acc] : acc,
          cache,
          [],
        );
      List.iter(Hashtbl.remove(cache), dead);
    };
};

/* every tile in the chunk subtree (of_delim consults term_data and
   refine_sort per tile; grout/secondary render from pieces alone) */
let rec chunk_tiles = (seg: Segment.t, acc: list(Tile.t)): list(Tile.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (p) {
      | Tile(t) =>
        List.fold_left(
          (acc, s) => chunk_tiles(s, acc),
          [t, ...acc],
          t.children,
        )
      | _ => acc
      },
    acc,
    seg,
  );

let view_chunked =
    (
      ~measured: Measured.t,
      ~settings: Settings.Model.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~refractor_shape_map: Id.Map.t(_),
      ~font_metrics: FontMetrics.t,
      ~term_data: TermData.t,
      ~refine_sort: (Id.t, Sort.t) => Sort.t,
      /* identity of the statics map behind refine_sort (see cv_info) */
      ~statics_ident: Obj.t,
      ~buffer_ids: list(Id.t),
    )
    : list(Node.t) => {
  incr(ChunkViews.tick);
  ChunkViews.sweep();
  let n = Array.length(measured.chunks);
  let sorts_of = (tiles: list(Tile.t)) =>
    tiles
    |> List.map((t: Tile.t) =>
         (
           refine_sort(t.id, t.mold.out),
           Option.map(
             (d: TermData.data) => d.sort,
             Id.Map.find_opt(t.id, term_data),
           ),
         )
       )
    |> Array.of_list;
  let render = (ch: Measured.chunk, final: bool): t =>
    span(
      ~attrs=[Attr.class_("code-chunk")],
      view(
        ~measured,
        ~settings,
        ~shape_map,
        ~refractor_shape_map,
        ~font_metrics,
        ~term_data,
        ~reserve_trailing_row=final,
        ~refine_sort,
        ~buffer_ids,
        ch.c_pieces,
      ),
    );
  List.init(n, i => i)
  |> List.map(i => {
       let ch = measured.chunks[i];
       let final = i == n - 1;
       let stable = (e: ChunkViews.entry) =>
         e.cv_flat === Obj.repr(ch.c_flat)
         && e.cv_final == final
         && e.cv_buffer === Obj.repr(buffer_ids)
         && e.cv_fm === Obj.repr(font_metrics)
         && e.cv_settings === Obj.repr(settings);
       switch (Hashtbl.find_opt(ChunkViews.cache, ch.c_anchor)) {
       | Some(e) when stable(e) && e.cv_info === statics_ident =>
         e.cv_tick = ChunkViews.tick^;
         e.cv_node;
       | Some(e) when stable(e) && e.cv_sorts == sorts_of(e.cv_tiles) =>
         e.cv_info = statics_ident;
         e.cv_tick = ChunkViews.tick^;
         e.cv_node;
       | _ =>
         let tiles = chunk_tiles(ch.c_pieces, []);
         let node = render(ch, final);
         let e = {
           ChunkViews.cv_flat: Obj.repr(ch.c_flat),
           cv_final: final,
           cv_tiles: tiles,
           cv_sorts: sorts_of(tiles),
           cv_info: statics_ident,
           cv_buffer: Obj.repr(buffer_ids),
           cv_fm: Obj.repr(font_metrics),
           cv_settings: Obj.repr(settings),
           cv_node: node,
           cv_tick: ChunkViews.tick^,
         };
         Hashtbl.replace(ChunkViews.cache, ch.c_anchor, e);
         node;
       };
     });
};
