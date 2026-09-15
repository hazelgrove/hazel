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
    ~expect=
      t =>
        contains("new_def", t)
        && !contains("new_def = ?", t)  /* implicit hole */
        && contains("in f(1)", t),
    ~desc="fn-body new-below is a let-in",
    (),
  );
};

/* Exercise the actual agent/focus boundary, without an LLM. Keep piece
   identities as an edit tool does; a whole reparse would merely close
   all the focused definitions and miss the rollback bug. */
let focused_model = () => {
  let seg = parse("let a = 1 in let b = 2 in b");
  let (term, info_map) = statics_of(seg);
  let id = outline_id(term, "a");
  let entry = Option.get(Focus.mk_entry(~info_map, id, seg));
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
  (
    SModel.{
      current: 0,
      scratchpads: [
        Web.ScratchModel.Scratchpad.mk_code(~name="Focus test", ~editor, ()),
      ],
      focus:
        Some({
          f_entries: [entry],
          f_master_seg: seg,
        }),
    },
    id,
    outline_id(term, "b"),
  );
};

let step = (action, model) =>
  Web.ScratchMode.Update.update(
    ~schedule_action=_ => (),
    ~settings=Web.Settings.Model.init,
    ~is_documentation=false,
    action,
    model,
  ).
    model;

let agent_segment = seg =>
  Web.ScratchMode.Update.AgentAction(
    Web.Agent.Update.Action.LoadSegmentIntoEditor(seg),
  );

let master_text = (model: SModel.t) =>
  switch (List.hd(model.scratchpads).kind) {
  | Code({editor, _}) => text_of(Focus.zip_of_cell(editor))
  | _ => failwith("expected code scratchpad")
  };

let agent_focus_sync = () => {
  let (model, a, _) = focused_model();
  let f = Option.get(model.focus);
  let e = List.hd(f.f_entries);
  /* A local edit lives only in the cell, then the agent changes b. */
  let e = {
    ...e,
    e_body: Focus.cell_of_seg(parse("10")),
  };
  let f = {
    ...f,
    f_entries: [e],
  };
  let model = {
    ...model,
    focus: Some(f),
  };
  let live = Focus.splice_all(f);
  let updated =
    step(
      Web.ScratchMode.Update.AgentAction(
        Web.Agent.Update.Action.DirectEdit(
          "update_definition",
          `Assoc([("path", `String("b")), ("code", `String("20"))]),
        ),
      ),
      model,
    );
  let f2 = Option.get(updated.focus);
  check(
    bool,
    "unchanged open cell keeps its editor",
    true,
    List.hd(f2.f_entries).e_body === e.e_body,
  );
  let closed = step(Web.ScratchMode.Update.UnfocusDef, updated);
  check(
    bool,
    "local edit survives",
    true,
    contains("10", master_text(closed)),
  );
  check(
    bool,
    "agent edit survives unfocus",
    true,
    contains("20", master_text(closed)),
  );
  /* The agent may also edit the OPEN definition. Its cell must refresh. */
  let updated =
    step(agent_segment(Focus.splice_def(a, parse("30"), live)), model);
  let e2 = List.hd(Option.get(updated.focus).f_entries);
  check(
    string,
    "open cell refreshed",
    "30",
    text_of(Focus.zip_of_cell(e2.e_body)),
  );
  check(
    bool,
    "open-cell edit survives unfocus",
    true,
    contains(
      "30",
      master_text(step(Web.ScratchMode.Update.UnfocusDef, updated)),
    ),
  );
  /* A streaming delta must not rebuild the editor or splice the stack. */
  let streamed =
    step(
      Web.ScratchMode.Update.AgentAction(
        Web.Agent.Update.Action.ReplayStreamTick,
      ),
      model,
    );
  check(
    bool,
    "stream tick keeps focus identity",
    true,
    streamed.focus === model.focus,
  );
  check(
    bool,
    "stream tick stays cheap",
    false,
    Web.Agent.Update.Action.uses_program(ReplayStreamTick),
  );
};

let agent_focus_delete = () => {
  let (model, a, _) = focused_model();
  let seg = Focus.splice_all(Option.get(model.focus));
  let (deleted, _) =
    Option.get(
      Web.ScratchMode.Restructure.apply(Web.OutlineSidebar.Delete, a, seg),
    );
  let updated = step(agent_segment(deleted), model);
  check(bool, "deleted open definition closes", true, updated.focus == None);
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
