open Alcotest;
open Haz3lcore;
open Js_of_ocaml;

/* the chunked code view (Code.view_chunked over Measured.Incr's chunks)
   renders what the whole Code.view does: its chunks' contents, in order,
   serialize to the whole view's nodes. Module heads and feet render from
   their token lists (plans/hierarchical-spans.md §4.4) */

module M = Measured;
module Code = Web.Code;

/* a vdom node as text: tags, then attributes and properties sorted, text
   quoted; [inner]: just its children */
let serializer: Js.Unsafe.any =
  Js.Unsafe.pure_js_expr(
    {js|(function (node, inner) {
  function ser(node) {
    switch (node.type) {
      case 'Thunk':
        return ser(node.fn(node.args));
      case 'VirtualText':
        return JSON.stringify(node.text);
      case 'VirtualNode':
        var props = node.properties || {};
        var attrs = props.attributes || {};
        var fields = Object.keys(attrs).sort().map(function (k) {
          return k + '=' + JSON.stringify(String(attrs[k]));
        });
        Object.keys(props).sort().forEach(function (k) {
          var v = props[k];
          if (typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean')
            fields.push(k + ':' + JSON.stringify(String(v)));
        });
        Object.keys(props.style || {}).sort().forEach(function (k) {
          fields.push('style.' + k + '=' + props.style[k]);
        });
        return '<' + node.tagName + (fields.length ? ' ' + fields.join(' ') : '') + '>'
          + kids(node) + '</' + node.tagName + '>';
      default:
        return '<?' + node.type + '>';
    }
  }
  function kids(node) {
    return node.children.map(ser).join('');
  }
  return inner ? kids(node) : ser(node);
})|js},
  );

let serialize = (~inner, node: Virtual_dom.Vdom.Node.t): string =>
  Js.to_string(
    Js.Unsafe.fun_call(
      serializer,
      [|
        Js.Unsafe.inject(Virtual_dom.Vdom.Node.to_raw(node)),
        Js.Unsafe.inject(Js.bool(inner)),
      |],
    ),
  );

let settings = Web.Settings.Model.init;
let font_metrics = Web.FontMetrics.init;
let refine_sort = (_, sort) => sort;
/* one statics map throughout: sorts come from term_data */
let statics_ident = Obj.repr(ref());

let term_data = (~root: Sort.t, seg: Segment.t): TermData.t =>
  MakeTerm.Incr.go_incr(~root, ~cache=MakeTerm.Incr.mk_cache(), seg).
    term_data;

let chunked_view =
    (~shapes, ~rows, ~term_data, measured: M.t)
    : list(Virtual_dom.Vdom.Node.t) =>
  Code.view_chunked(
    ~measured,
    ~settings,
    ~shape_map=shapes,
    ~refractor_rows=rows,
    ~font_metrics,
    ~term_data,
    ~refine_sort,
    ~statics_ident,
    ~buffer_ids=[],
  );

let whole_view =
    (~shapes, ~rows, ~term_data, seg: Segment.t)
    : list(Virtual_dom.Vdom.Node.t) =>
  Code.view(
    ~measured=M.of_segment(seg, shapes, rows),
    ~settings,
    ~shape_map=shapes,
    ~refractor_rows=rows,
    ~font_metrics,
    ~term_data,
    ~refine_sort,
    ~buffer_ids=[],
    seg,
  );

/* the chunked nodes: one span.code-chunk per chunk, whose contents in
   order serialize to the whole view */
let check_html =
    (
      name: string,
      measured: M.t,
      whole: list(Virtual_dom.Vdom.Node.t),
      chunks: list(Virtual_dom.Vdom.Node.t),
    ) => {
  check(
    int,
    name ++ ": a node per chunk",
    Array.length(measured.chunks),
    List.length(chunks),
  );
  List.iter(
    node =>
      if (!
            String.starts_with(
              ~prefix="<span class=\"code-chunk\">",
              serialize(~inner=false, node),
            )) {
        fail(name ++ ": a chunk is not one span.code-chunk");
      },
    chunks,
  );
  check(
    string,
    name ++ ": html",
    String.concat("", List.map(serialize(~inner=false), whole)),
    String.concat("", List.map(serialize(~inner=true), chunks)),
  );
};

/* the whole view of [seg] against the chunked one */
let same_html =
    (
      ~root: Sort.t=Exp,
      ~shapes=Id.Map.empty,
      ~rows=Id.Map.empty,
      name: string,
      seg: Segment.t,
    )
    : unit => {
  let term_data = term_data(~root, seg);
  let measured =
    M.Incr.of_segment(~root, ~cache=M.Incr.mk_cache(), seg, shapes, rows);
  check_html(
    name,
    measured,
    whole_view(~shapes, ~rows, ~term_data, seg),
    chunked_view(~shapes, ~rows, ~term_data, measured),
  );
};

let programs = (~root, programs, ()) =>
  List.iter(
    ((name, src)) => same_html(~root, name, CorpusUtil.seg_of(~root, src)),
    programs,
  );

let incomplete = () =>
  List.iter(
    ((name, src)) =>
      switch (CorpusUtil.typed_seg(src)) {
      | Some(seg) => same_html(name, seg)
      | None => fail("untypeable program: " ++ name)
      },
    CorpusUtil.incomplete_programs,
  );

/* drawers and projectors inside members, on split tiles and in heads */
let drawers = () => {
  module T = Test_MeasuredChunks;
  let root = Sort.Mod;
  let seg = CorpusUtil.seg_of(~root, T.mod_program);
  let (seg, tab) = T.project("3", seg);
  let (seg, block) = T.project("1", seg);
  let shapes =
    Id.Map.of_list([
      (tab, T.shape(3, Tab(2))),
      (block, T.shape(2, Block(1))),
    ]);
  let rows =
    Id.Map.of_list([
      (T.tile_id("2", seg), 2),
      (T.brace_after("module", seg), 1),
    ]);
  same_html(~root, ~shapes, ~rows, "mod drawers", seg);
  let root = Sort.Exp;
  let seg = CorpusUtil.seg_of(~root, T.exp_program);
  let rows =
    switch (seg) {
    | [Tile(t), ..._] => Id.Map.singleton(t.id, 2)
    | _ => fail("no module tile")
    };
  same_html(~root, ~rows, "exp drawers", seg);
};

/* a member's edit re-renders its chunk alone: every other chunk's node,
   the module's head and foot included, is the same object as before. One
   editor's caches throughout: chunk views are memoized by anchor, so
   another measurement of the same program would evict them */
let rerender = (~root, seg: Segment.t, edits: list((string, string))) => {
  let m_cache = M.Incr.mk_cache();
  let t_cache = MakeTerm.Incr.mk_cache();
  let (shapes, rows) = (Id.Map.empty, Id.Map.empty);
  let render = (name, seg) => {
    let term_data =
      MakeTerm.Incr.go_incr(~root, ~cache=t_cache, seg).term_data;
    let measured =
      M.Incr.of_segment(~root, ~cache=m_cache, seg, shapes, rows);
    let chunks = chunked_view(~shapes, ~rows, ~term_data, measured);
    check_html(
      name,
      measured,
      whole_view(~shapes, ~rows, ~term_data, seg),
      chunks,
    );
    chunks;
  };
  ignore(
    List.fold_left(
      ((seg, before), (needle, repl)) => {
        let (seg', found) =
          CorpusUtil.edit_token(
            ~at=`First,
            ~keep_id=true,
            ~needle,
            ~repl,
            seg,
          );
        check(bool, "edited " ++ needle, true, found);
        let after = render(needle, seg');
        check(
          int,
          needle ++ ": chunks",
          List.length(before),
          List.length(after),
        );
        check(
          int,
          needle ++ ": chunks re-rendered",
          1,
          List.fold_left2(
            (n, a, b) => a === b ? n : n + 1,
            0,
            before,
            after,
          ),
        );
        (seg', after);
      },
      (seg, render("before", seg)),
      edits,
    ),
  );
};

let rerenders = () => {
  module T = Test_MeasuredChunks;
  rerender(
    ~root=Mod,
    CorpusUtil.seg_of(~root=Mod, T.mod_program),
    [("2", "22"), ("3", "33"), ("more", "most")],
  );
  rerender(
    ~root=Exp,
    CorpusUtil.seg_of(~root=Exp, T.exp_program),
    [("y", "w"), ("5", "55")],
  );
  rerender(
    ~root=Mod,
    CorpusUtil.mega_seg(~root=Mod, "mega-mod-1k.hz"),
    [("180", "1800"), ("75", "750")],
  );
};

/* two editors drawing one program (a stack cell shares its program's
   ids) keep their own chunk views: rendering one doesn't evict the other */
let two_editors = () => {
  let root = Sort.Mod;
  let seg = CorpusUtil.seg_of(~root, Test_MeasuredChunks.mod_program);
  let term_data = term_data(~root, seg);
  let (shapes, rows) = (Id.Map.empty, Id.Map.empty);
  let editor = () => {
    let cache = M.Incr.mk_cache();
    () =>
      chunked_view(
        ~shapes,
        ~rows,
        ~term_data,
        M.Incr.of_segment(~root, ~cache, seg, shapes, rows),
      );
  };
  let (a, b) = (editor(), editor());
  let (a0, b0) = (a(), b());
  let (a1, b1) = (a(), b());
  let same = (xs, ys) => List.for_all2((===), xs, ys);
  check(bool, "the first editor's nodes kept", true, same(a0, a1));
  check(bool, "the second editor's nodes kept", true, same(b0, b1));
};

let corpus = (~root=Sort.Exp, file, ()) =>
  same_html(~root, file, CorpusUtil.mega_seg(~root, file));

let slides = () =>
  List.iter(
    ((path, src)) =>
      switch (CorpusUtil.parse(src)) {
      | Some(seg) => same_html(path, seg)
      | None => ()
      },
    CorpusUtil.slide_corpus(),
  );

let tests = (
  "ChunkViews",
  [
    test_case(
      "module programs",
      `Quick,
      () => {
        programs(~root=Mod, [("mod", Test_MeasuredChunks.mod_program)], ());
        programs(
          ~root=Mod,
          [("edges", Test_MeasuredChunks.body_edges_program)],
          (),
        );
        programs(~root=Exp, [("exp", Test_MeasuredChunks.exp_program)], ());
      },
    ),
    test_case(
      "item shapes, Exp root",
      `Quick,
      programs(~root=Exp, Test_ItemSpans.exp_shapes),
    ),
    test_case(
      "item shapes, Mod root",
      `Quick,
      programs(~root=Mod, Test_ItemSpans.letin_members),
    ),
    test_case(
      "edge programs",
      `Quick,
      programs(~root=Exp, CorpusUtil.edge_programs),
    ),
    test_case("incomplete programs", `Quick, incomplete),
    test_case("drawers and projectors in members", `Quick, drawers),
    test_case("a member edit re-renders one chunk", `Quick, rerenders),
    test_case("two editors on one program", `Quick, two_editors),
    /* Mega 2k and 4k repeat its shapes */
    test_case("mega-1k", `Quick, corpus("mega-1k.hz")),
    test_case("mega-mod-1k", `Quick, corpus(~root=Mod, "mega-mod-1k.hz")),
    test_case("slides", `Slow, slides),
  ],
);
