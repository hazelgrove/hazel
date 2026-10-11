open Alcotest;
open Haz3lcore;
open Language;
open CorpusUtil;

/* focus slicing: mk_entry carves header/body cells with a frozen ctx out of
   the program, and splice_entry restores it byte-identically */

module Focus = Web.ScratchMode.Focus;

let statics_of = (seg: Segment.t) => {
  let term = MakeTerm.go(seg).term;
  let (info_map, _) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    );
  (term, info_map);
};

/* focus [label]: header/body text, [bound] in the frozen ctx, exact splice */
let check_focus =
    (
      ~src: string,
      ~label: string,
      ~header: string,
      ~body: string,
      ~bound=[],
      (),
    )
    : unit => {
  let master = seg_of(src);
  let (term, info_map) = statics_of(master);
  let fid = row_by_label(term, label);
  switch (Focus.mk_entry(~info_map, fid, master)) {
  | None => failwith("mk_entry failed for " ++ label)
  | Some(e) =>
    check(
      string,
      label ++ ": header",
      header,
      text_of(Focus.zip_of_cell(e.e_header)),
    );
    check(
      string,
      label ++ ": body",
      body,
      text_of(Focus.zip_of_cell(e.e_body)),
    );
    List.iter(
      x =>
        check(
          bool,
          label ++ ": ctx binds " ++ x,
          true,
          Ctx.lookup_var(e.e_ctx, x) != None,
        ),
      bound,
    );
    check(
      string,
      label ++ ": splice round-trip",
      text_of(master),
      text_of(Focus.splice_entry(e, master)),
    );
  };
};

/* headerless items (tests, trailing bodies): symbol chip, body, splice */
let check_headless = (~src, ~label, ~sym, ~body, ()): unit => {
  let master = seg_of(src);
  let (term, info_map) = statics_of(master);
  let fid = row_by_label(term, label);
  switch (Focus.mk_entry(~info_map, fid, master)) {
  | None => failwith("mk_entry failed for " ++ label)
  | Some(e) =>
    check(bool, label ++ ": headless", true, e.e_kind == Headless(sym));
    check(
      string,
      label ++ ": body",
      body,
      text_of(Focus.zip_of_cell(e.e_body)),
    );
    check(
      string,
      label ++ ": splice round-trip",
      text_of(master),
      text_of(Focus.splice_entry(e, master)),
    );
    check(
      bool,
      label ++ ": outline sym",
      true,
      Web.SlideView.sym_of(fid, term) == Some(sym),
    );
  };
};

/* a let without its `in` can't open as a cell */
let unfinished_let = () => {
  let open_at = (typed: string): bool => {
    let seg = Zipper.unselect_and_zip(typed_zipper(typed));
    let (_, info_map) = statics_of(seg);
    let first = Option.get(List.find_map(Piece.is_tile, seg));
    Focus.mk_entry(~info_map, first.id, seg) != None;
  };
  check(bool, "unfinished let stays closed", false, open_at("let x = 1"));
  check(bool, "finished let opens", true, open_at("let x = 1 in x"));
};

/* review fixes: a typed-function header is named by its function, a run
   counts its tests, and a slide owns only its own keys */
let names_and_keys = () => {
  let master = seg_of("let add(x: Int, y: Int): Int = x + y in add(1, 2)");
  let (term, info_map) = statics_of(master);
  switch (Focus.mk_entry(~info_map, row_by_label(term, "add"), master)) {
  | None => fail("no cell for add")
  | Some(e) =>
    check(
      option(string),
      "named by its function",
      Some("add"),
      Web.ScratchCell.header_name(e),
    )
  };
  let run = seg_of("test 1 == 1 end;\ntest 2 == 2 end;\ntest 3 == 3 end;\n0");
  let (rterm, _) = statics_of(run);
  check(
    option(int),
    "three tests",
    Some(3),
    Option.map(
      ((_, _, n)) => n,
      Focus.test_run_deep(row_by_label(rterm, "1"), run),
    ),
  );
  let own = Web.ScratchPersist.slide_suffix("scratch:Week 1");
  check(
    option(string),
    "its side key",
    Some(":agent"),
    own("scratch:Week 1:agent"),
  );
  check(
    option(string),
    "its items",
    Some(":items:roster"),
    own("scratch:Week 1:items:roster"),
  );
  check(
    option(string),
    "another slide",
    None,
    own("scratch:Week 1: Lists"),
  );
  check(
    option(string),
    "another slide's key",
    None,
    own("scratch:Week 1: Lists:agent"),
  );
};

/* a headless cell's symbol has two sources: its row's outline kind, which
   a pin passes when it opens the cell, and its item span. Where both name
   one they agree, but on a module body's last item: the outline lists it
   as a statement (`;`), the span reads it as the body's tail (⇒). The
   outline's wins. Mega 2k/4k repeat 1k's shapes and would take half a
   minute: every row re-walks the program */
let header_symbols = () => {
  let survey = (~root, name, src): int => {
    let seg = seg_of(~root, src);
    let term = MakeTerm.Incr.term_of_root(~root, seg);
    let exceptions = ref(0);
    /* [in_module]: these rows are a module body's items */
    let rec walk = (~in_module, ns: list(Web.OutlineTree.node)) =>
      List.iter(
        (n: Web.OutlineTree.node) =>
          switch (n.o_kind) {
          | KTests => walk(~in_module, n.o_children)
          | kind =>
            switch (
              Option.bind(n.o_id, id => Web.SlideView.sym_of(id, term)),
              Option.bind(n.o_id, id =>
                Option.map(snd, Focus.headless_content_deep(id, seg))
              ),
            ) {
            | (Some(o), Some(s)) when o != s =>
              incr(exceptions);
              check(
                bool,
                name ++ ": only a module body's last item",
                true,
                in_module && o == {js|;|js} && s == {js|⇒|js},
              );
            | _ => ()
            };
            walk(~in_module=kind == KModule, n.o_children);
          },
        ns,
      );
    walk(~in_module=root == Sort.Mod, Web.OutlineTree.of_term(term));
    exceptions^;
  };
  let module_last = "module M = {\n  let a = 1;\n  a + 1\n} in\nM.a";
  List.iter(
    ((root, src, n)) => check(int, src, n, survey(~root, src, src)),
    [
      (Sort.Exp, module_last, 1),
      (Exp, "module M = {\n  let a = 1;\n  test a == 1 end\n} in\nM.a", 1),
      (Exp, "let m = {\n  let a = 1;\n  a + 2\n} in\nm", 1),
      (
        Exp,
        "module M = {\n  module N = {\n    let p = 1;\n    p\n  };\n  0\n} in\n1",
        2,
      ),
      (Exp, "module M = {\n  a + 1;\n  let b = 2\n} in\nM.b", 0),
      (Mod, "let a = 1;\ntest a == 1 end;\na + 1", 1),
      (Exp, "let a = 1 in\na;\ntest a == 1 end", 0),
      (Exp, "let f = fun x ->\n  test x == x end;\n  x\nin\nf(1)", 0),
      (Exp, "let f = fun x ->\n  let y = x in\n  y + 1\nin\nf(1)", 0),
    ],
  );
  List.iter(
    ((name, src)) => ignore(survey(~root=Exp, name, src)),
    incomplete_programs @ slide_corpus(),
  );
  List.iter(
    ((file, root, n)) =>
      check(int, file, n, survey(~root, file, Option.get(mega_src(file)))),
    [("mega-1k.hz", Sort.Exp, 0), ("mega-mod-1k.hz", Mod, 1)],
  );
  /* a pin opens the module's last item under the outline's symbol */
  let seg = seg_of(module_last);
  let (term, info_map) = statics_of(seg);
  let last =
    switch (
      List.find_map(
        (n: Web.OutlineTree.node) => n.o_kind == KStmt ? n.o_id : None,
        Web.OutlineTree.flatten(Web.OutlineTree.of_term(term)),
      )
    ) {
    | Some(id) => id
    | None => fail("no statement row")
    };
  switch (
    Web.SlideView.realize(
      ~info_map,
      ~term,
      Web.SlideView.pin(~term, last, Web.SlideView.init),
      Whole(Focus.cell_of_seg(seg)),
    )
  ) {
  | (_, Divided(d)) =>
    check(
      bool,
      "the outline's symbol",
      true,
      List.hd(Web.Divided.cells(d)).e_kind == Headless({js|;|js}),
    )
  | (_, Whole(_)) => fail("didn't open")
  };
};

let tests = (
  "StackFocus",
  [
    test_case("fun-style def", `Quick, () =>
      check_focus(
        ~src="let inc = fun x -> x + 1 in inc(2)",
        ~label="inc",
        ~header="inc",
        ~body="fun x -> x + 1",
        (),
      )
    ),
    test_case("funlet sugar", `Quick, () =>
      check_focus(
        ~src="let inc(x) = x + 1 in inc(2)",
        ~label="inc",
        ~header="inc(x)",
        ~body="x + 1",
        ~bound=["x"],
        (),
      )
    ),
    test_case("module member", `Quick, () =>
      check_focus(
        ~src="module M = {\n  let a = 1;\n  let b = a + 1;\n} in M.b",
        ~label="b",
        ~header="b",
        ~body="a + 1",
        ~bound=["a"],
        (),
      )
    ),
    test_case("module member test", `Quick, () =>
      check_headless(
        ~src="module M = {\n  let a = 1;\n  test a == 1 end;\n} in M.a",
        ~label="1",
        ~sym={js|;|js},
        ~body="test a == 1 end",
        (),
      )
    ),
    /* an open cell's live name keeps a non-ASCII name whole */
    test_case(
      "a non-ASCII header name",
      `Quick,
      () => {
        let master = seg_of("let café = 1 in café");
        let (term, info_map) = statics_of(master);
        switch (
          Focus.mk_entry(~info_map, row_by_label(term, "café"), master)
        ) {
        | Some(e) =>
          check(
            option(string),
            "header name",
            Some("café"),
            Web.ScratchCell.header_name(e),
          )
        | None => fail("mk_entry")
        };
      },
    ),
    /* the body's first statement starts after the `fun x ->` head */
    test_case("fn-body first statement", `Quick, () =>
      check_headless(
        ~src="let f = fun x ->\n  test x > 0 end;\n  x + 1\nin f(1)",
        ~label="1",
        ~sym={js|;|js},
        ~body="test x > 0 end",
        (),
      )
    ),
    test_case("fn-body trailing expression", `Quick, () =>
      check_headless(
        ~src="let f = fun x -> let y = x + 1 in y * 2 in f(1)",
        ~label="",
        ~sym="\xe2\x87\x92",
        ~body="y * 2",
        (),
      )
    ),
    test_case("member fn-body trailing expression", `Quick, () =>
      check_headless(
        ~src=
          "module M = {\n  let f = fun x -> let y = x + 1 in y * 2;\n  let g = 0\n} in M.f(1)",
        ~label="",
        ~sym="\xe2\x87\x92",
        ~body="y * 2",
        (),
      )
    ),
    test_case("header symbols", `Quick, header_symbols),
    test_case("names, run sizes and slide keys", `Quick, names_and_keys),
    test_case("unfinished let stays closed", `Quick, unfinished_let),
    test_case("type alias", `Quick, () =>
      check_focus(
        ~src="type T = Int in let x: T = 1 in x",
        ~label="T",
        ~header="T",
        ~body="Int",
        (),
      )
    ),
  ],
);
