open Alcotest;
open Haz3lcore;
open Language;

/* focus slicing: mk_entry carves header/body cells with a frozen ctx out of
   the program, and splice_entry restores it byte-identically */

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
    /* the fast path bails on some shapes; recover like persistence */
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

/* headerless items (tests, trailing bodies): symbol chip, body, splice */
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
      Web.SlideView.sym_of(fid, term) == Some(sym),
    );
  };
};

/* member-granularity restructure: ops apply at the row's OWNING block */
let check_restructure =
    (~src, ~label, ~op, ~expect: string => bool, ~desc, ()): unit => {
  let master = parse(src);
  let (term, _) = statics_of(master);
  let fid = outline_id(term, label);
  switch (Web.ItemEdit.apply(op, fid, master)) {
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
    ~expect=
      t =>
        contains("new_def", t)
        && !contains("new_def = ?", t)  /* implicit hole */
        && contains("in f(1)", t),
    ~desc="fn-body new-below is a let-in",
    (),
  );
};

/* Exercise the actual agent/cell boundary, without an LLM. Keep piece
   identities as an edit tool does; a whole reparse would merely close
   all the open cells and miss the rollback bug. */
let divided_model = () => {
  let seg = parse("let a = 1 in let b = 2 in b");
  let (term, info_map) = statics_of(seg);
  let id = outline_id(term, "a");
  let editor = Focus.cell_of_seg(seg);
  let editor = {
    ...editor,
    editor: {
      ...editor.editor,
      statics: {
        ...editor.editor.statics,
        term,
        info_map,
      },
    },
  };
  let d = Option.get(Web.Divided.split(~info_map, editor, id));
  (
    SModel.{
      current: 0,
      scratchpads: [
        {
          name: "Focus test",
          kind:
            Code({
              program: Divided(d),
              view: {
                ...Web.SlideView.init,
                pins: [
                  {
                    p_id: id,
                    p_run: false,
                  },
                ],
              },
              agent: Web.Agent.Utils.init(),
            }),
          dormant: false,
        },
      ],
    },
    id,
    outline_id(term, "b"),
  );
};

/* update only: an agent edit arms StaticsDebounce.force_on_next, which the
   app's calculate phase would consume; clear it so it does not leak into
   later tests (AgentControlFlow's dispatch gate reads it) */
let step = (action, model) => {
  let model =
    Web.ScratchMode.Update.update(
      ~schedule_action=_ => (),
      ~settings=Web.Settings.Model.init,
      ~is_documentation=false,
      action,
      model,
    ).
      model;
  Web.CodeWithStatics.StaticsDebounce.force_on_next := false;
  model;
};

let agent = a => Web.ScratchMode.Update.Workspace(AgentAction(a));
let agent_segment = seg =>
  agent(Web.Agent.Update.Action.LoadSegmentIntoEditor(seg));

let program = (model: SModel.t) =>
  switch (List.hd(model.scratchpads).kind) {
  | Code({program, _}) => program
  | _ => failwith("expected code scratchpad")
  };
let cells = model =>
  switch (program(model)) {
  | Divided(d) => Web.Divided.cells(d)
  | Whole(_) => []
  };
let master_text = model => text_of(Web.Program.document(program(model)));
let close_all = model =>
  step(Web.ScratchMode.Update.Workspace(UnfocusDef), model);

let agent_focus_sync = () => {
  let (model, a, _) = divided_model();
  /* A local edit lives only in the cell, then the agent changes b. */
  let model =
    switch (List.hd(model.scratchpads).kind) {
    | Code({program: Divided(d), _} as code) =>
      let d =
        Web.Divided.update_cell(
          a,
          e =>
            {
              ...e,
              e_body: Focus.cell_of_seg(parse("10")),
            },
          d,
        );
      {
        ...model,
        scratchpads: [
          {
            ...List.hd(model.scratchpads),
            kind:
              Code({
                ...code,
                program: Divided(d),
              }),
          },
        ],
      };
    | _ => failwith("expected a divided program")
    };
  let local = List.hd(cells(model));
  let live = Web.Program.document(program(model));
  let updated =
    step(
      agent(
        Web.Agent.Update.Action.DirectEdit(
          "update_definition",
          `Assoc([("path", `String("b")), ("code", `String("20"))]),
        ),
      ),
      model,
    );
  check(
    bool,
    "unchanged open cell keeps its editor",
    true,
    List.hd(cells(updated)).e_body === local.e_body,
  );
  let closed = close_all(updated);
  check(
    bool,
    "local edit survives",
    true,
    contains("10", master_text(closed)),
  );
  check(
    bool,
    "agent edit survives closing the cells",
    true,
    contains("20", master_text(closed)),
  );
  /* The agent may also edit the OPEN definition. Its cell must refresh. */
  let updated =
    step(agent_segment(Focus.splice_def(a, parse("30"), live)), model);
  check(
    string,
    "open cell refreshed",
    "30",
    text_of(Focus.zip_of_cell(List.hd(cells(updated)).e_body)),
  );
  check(
    bool,
    "open-cell edit survives closing the cells",
    true,
    contains("30", master_text(close_all(updated))),
  );
  /* A streaming delta must not rebuild the editor or re-cut the cells. */
  let streamed =
    step(agent(Web.Agent.Update.Action.ReplayStreamTick), model);
  check(
    bool,
    "stream tick keeps the program",
    true,
    program(streamed) === program(model),
  );
  check(
    bool,
    "stream tick stays cheap",
    false,
    Web.Agent.Update.Action.uses_program(ReplayStreamTick),
  );
};

let agent_focus_delete = () => {
  let (model, a, _) = divided_model();
  let seg = Web.Program.document(program(model));
  let (deleted, _) =
    Option.get(Web.ItemEdit.apply(Web.OutlineSidebar.Delete, a, seg));
  let updated = step(agent_segment(deleted), model);
  check(
    bool,
    "deleted open definition closes",
    true,
    !List.exists((e: Web.ScratchCell.t) => e.e_id == a, cells(updated)),
  );
  check(
    bool,
    "deleted definition stays deleted",
    false,
    contains("let a", master_text(updated)),
  );
  check(
    bool,
    "other definitions remain",
    true,
    contains("let b", master_text(updated)),
  );
};

/* a let without its `in` can't open as a cell */
let unfinished_let = () => {
  let first_tile_id = (seg: Segment.t): Id.t =>
    switch (
      List.find_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) => Some(t.id)
          | _ => None
          },
        seg,
      )
    ) {
    | Some(id) => id
    | None => failwith("no tile")
    };
  let open_at = (typed: string): bool => {
    let seg =
      switch (Parser.to_zipper(typed, ~root=Exp)) {
      | Some(z) => Zipper.unselect_and_zip(z)
      | None => failwith("typing failed: " ++ typed)
      };
    let (_, info_map) = statics_of(seg);
    Focus.mk_entry(~info_map, first_tile_id(seg), seg) != None;
  };
  check(bool, "unfinished let stays closed", false, open_at("let x = 1"));
  check(bool, "finished let opens", true, open_at("let x = 1 in x"));
};

/* review fixes: a typed-function header is named by its function, a run
   counts its tests, and a slide owns only its own keys */
let names_and_keys = () => {
  let master = parse("let add(x: Int, y: Int): Int = x + y in add(1, 2)");
  let (term, info_map) = statics_of(master);
  switch (Focus.mk_entry(~info_map, outline_id(term, "add"), master)) {
  | None => fail("no cell for add")
  | Some(e) =>
    check(
      option(string),
      "named by its function",
      Some("add"),
      Web.ScratchCell.header_name(e),
    )
  };
  let run = parse("test 1 == 1 end;\ntest 2 == 2 end;\ntest 3 == 3 end;\n0");
  let (rterm, _) = statics_of(run);
  check(
    option(int),
    "three tests",
    Some(3),
    Focus.test_run_size_deep(outline_id(rterm, "1"), run),
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

let tests = (
  "StackFocus",
  [
    test_case(
      "agent edits and focused cells stay synchronized",
      `Quick,
      agent_focus_sync,
    ),
    test_case(
      "agent deletion closes focused definition",
      `Quick,
      agent_focus_delete,
    ),
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
        let master = parse("let café = 1 in café");
        let (term, info_map) = statics_of(master);
        switch (Focus.mk_entry(~info_map, outline_id(term, "café"), master)) {
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
    test_case("member restructure", `Quick, member_restructure),
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
