open Alcotest;
open Haz3lcore;
open Language;

/* Stack-focus slicing (modular-editors): find_pat/find_def carve the
   header/body cells out of the master, mk_entry captures the frozen
   ctx, and splice_entry restores the master byte-identically — across
   the def shapes: fun-style, funlet sugar, module members, type
   aliases. Run: bash test/run_node.sh test 'StackFocus' */

module Focus = Web.ScratchMode.Focus;
module SModel = Web.ScratchMode.Model;

let parse = (src: string): Segment.t =>
  switch (
    FastParse.of_text(
      ~materialize=Triggers.invoked_projector,
      ~collect_refractors=true,
      ~root=Exp,
      src,
    )
  ) {
  | Some(seg) => seg
  | None =>
    /* FastParse's linear path bails on some shapes — recover like
       persistence load does */
    switch (MarkerParse.of_text(~root=Exp, src)) {
    | Some(z) => Zipper.unselect_and_zip(z)
    | None => failwith("parse failed: " ++ src)
    }
  };

let text_of = (seg: Segment.t): string =>
  seg |> Zipper.unzip |> MarkerParse.to_text;

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

let outline_id = (term, label: string): Id.t => {
  let rec find = (ns: list(Web.OutlineTree.node)) =>
    List.fold_left(
      (acc, n: Web.OutlineTree.node) =>
        switch (acc) {
        | Some(_) => acc
        | None => n.o_label == label ? n.o_id : find(n.o_children)
        },
      None,
      ns,
    );
  switch (find(Web.OutlineTree.of_term(term))) {
  | Some(id) => id
  | None => failwith("no outline row: " ++ label)
  };
};

/* focus [label] in [src]: check the header/body cell text, that the
   frozen ctx binds [bound], and that an unedited splice restores the
   master exactly */
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
  let master = parse(src);
  let (term, info_map) = statics_of(master);
  let fid = outline_id(term, label);
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

/* headerless items (tests, nested trailing bodies): symbol chip,
   content, and splice round-trip */
let check_headless = (~src, ~label, ~sym, ~body, ()): unit => {
  let master = parse(src);
  let (term, info_map) = statics_of(master);
  let fid = outline_id(term, label);
  switch (Focus.mk_entry(~info_map, fid, master)) {
  | None => failwith("mk_entry failed for " ++ label)
  | Some(e) =>
    check(bool, label ++ ": headless", true, e.e_sym != None);
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
      Web.ScratchMode.outline_sym(fid, term) == Some(sym),
    );
  };
};

/* member-granularity restructure: ops apply at the row's OWNING block */
let check_restructure =
    (~src, ~label, ~op, ~expect: string => bool, ~desc, ()): unit => {
  let master = parse(src);
  let (term, _) = statics_of(master);
  let fid = outline_id(term, label);
  switch (Web.ScratchMode.Restructure.apply(op, fid, master)) {
  | None => failwith("apply failed: " ++ desc)
  | Some((seg', _)) =>
    let txt = text_of(seg');
    if (!expect(txt)) {
      Printf.printf("RESTRUCTURE %s =>\n%s\n<<<END\n", desc, txt);
    };
    check(bool, desc, true, expect(txt));
  };
};

let contains = (needle, hay) => {
  let nl = String.length(needle)
  and hl = String.length(hay);
  let rec go = i =>
    i + nl <= hl && (String.sub(hay, i, nl) == needle || go(i + 1));
  go(0);
};

let member_restructure = (): unit => {
  let m_src = "module M = {\n  let a = 1;\n  let b = 2;\n} in M.a";
  check_restructure(
    ~src=m_src,
    ~label="b",
    ~op=Web.OutlineSidebar.Delete,
    ~expect=t => !contains("let b", t) && contains("let a", t),
    ~desc="member delete",
    (),
  );
  check_restructure(
    ~src=m_src,
    ~label="b",
    ~op=Web.OutlineSidebar.MoveUp,
    ~expect=
      t => {
        let ia = String.index(t, 'a');
        let ib = String.index(t, 'b');
        ib < ia && contains("in M.a", t);
      },
    ~desc="member move up",
    (),
  );
  check_restructure(
    ~src=m_src,
    ~label="a",
    ~op=Web.OutlineSidebar.NewBelow,
    ~expect=
      t =>
        contains("new_def", t)
        && !contains("new_def = ? in", t)  /* MEMBER form, not let-in */
        && contains("let b = 2", t),
    ~desc="member new-below is a 2-shard member",
    (),
  );
  check_restructure(
    ~src=m_src,
    ~label="a",
    ~op=Web.OutlineSidebar.Duplicate,
    ~expect=t => contains("let b", t),
    ~desc="member duplicate keeps the block intact",
    (),
  );
  /* fn-body block: new def below a nested let stays inside the fn */
  check_restructure(
    ~src="let f = fun x -> let y = 1 in y in f(1)",
    ~label="y",
    ~op=Web.OutlineSidebar.NewBelow,
    ~expect=t => contains("new_def = ? in", t) && contains("in f(1)", t),
    ~desc="fn-body new-below is a let-in",
    (),
  );
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
    test_case("fn-body trailing expression", `Quick, () =>
      check_headless(
        ~src="let f = fun x -> let y = x + 1 in y * 2 in f(1)",
        ~label="",
        ~sym="\xe2\x87\x92",
        ~body="y * 2",
        (),
      )
    ),
    test_case("member restructure", `Quick, member_restructure),
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
