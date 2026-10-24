open Alcotest;
open Haz3lcore;
open Language;

/* The agent's view (CompositionView): the map and open entries with their
   overlays, as golden strings; the open set by id across a rename; what a
   request records, against what the payload carries */

let settings = CoreSettings.on;

/* a program as the editor reads it: its pieces, zipper and statics */
let program = (~root=Sort.Exp, src: string) => {
  let z = PersistentZipper.from_backup_text(src, ~root);
  let seg = Zipper.unselect_and_zip(z);
  let st =
    CachedStatics.init(
      ~settings,
      ~is_dynamic_term=false,
      ~stitch=x => x,
      ~root,
      z,
    );
  (seg, z, st);
};

/* its run: test results, value and probe samples */
let run = (z: Zipper.t, st: CachedStatics.t) => {
  let targets =
    CachedStatics.compute_targets(
      ~settings,
      ~info_map=st.info_map,
      ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
      (),
    );
  let (value, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets),
      ~env=Builtins.env_init,
      st.elaborated,
    );
  (
    TestResults.mk_results(EvaluatorState.get_tests(state)),
    value,
    Sample.Map.finalize(EvaluatorState.get_probes(state)),
  );
};

let ids_of = (o: Outline.t, paths: list(string)): list(Id.t) =>
  List.map(
    p =>
      switch (Outline.resolve(o, p)) {
      | Ok(e) => e.id
      | Error(e) => Alcotest.fail(e)
      },
    paths,
  );

let render =
    (
      ~root=Sort.Exp,
      ~opened=[],
      ~evaluate=false,
      ~settings=CompositionView.default_settings,
      src,
    ) => {
  let (seg, z, st) = program(~root, src);
  let o = Outline.build(~root, seg);
  let (tests, value, probes) =
    evaluate
      ? {
        let (t, v, p) = run(z, st);
        (Some(t), Some(v), p);
      }
      : (None, None, Sample.Map.empty);
  CompositionView.render(
    ~settings,
    ~facts={
      info_map: st.info_map,
      error_ids: st.error_ids,
      tests,
      probes,
      refractors: z.refractors.manuals,
      value,
      term_data: CachedSyntax.init(~root, z).term_data,
    },
    o,
    ~opened=ids_of(o, opened),
  );
};

let golden = (name, expected, actual) =>
  if (expected != actual) {
    print_endline("== " ++ name ++ " (actual)\n" ++ actual);
    check(string, name, expected, actual);
  };

/* a golden file under test/golden, from the repo root or the test dir;
   HAZEL_GOLDEN=1 writes it instead */
let golden_file = (name: string, actual: string): unit => {
  let path = "test/golden/" ++ name;
  let path = Sys.file_exists(path) ? path : "golden/" ++ name;
  if (Sys.getenv_opt("HAZEL_GOLDEN") == Some("1")) {
    let oc = open_out_bin(path);
    output_string(oc, actual ++ "\n");
    close_out(oc);
  } else {
    switch (CorpusUtil.read_file(path)) {
    | Some(s) => golden(name, Util.StringUtil.strip_final_newline(s), actual)
    | None => Alcotest.fail("no golden " ++ path)
    };
  };
};

let example = {|type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float =
  case s
  | Circle(r) => 3.14 *. r *. r
  | Rect(w, h) => w *. h
  end
in
let total(xs: [Shape]): Float =
  case xs
  | [] => 0.
  | s :: rest => area(s) +. total(rest) +. true
  end
in
module Geo = {
  let perimeter(s: Shape): Float = 0.;
  let scale(k: Float, s: Shape): Shape = s;
  module Units = { let cm = 1.; let m = 100.; let km = 100000. }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|};

let format_tests = [
  test_case("the map: headers, ⋯ bodies, annotations", `Quick, () =>
    golden(
      "example",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { let cm = ⋯; let m = ⋯; let km = ⋯ }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|},
      render(example),
    )
  ),
  test_case("an open entry follows the map, its error marked", `Quick, () =>
    golden(
      "total open",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # open below · 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { let cm = ⋯; let m = ⋯; let km = ⋯ }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])

# open: total #
let total(xs: [Shape]): Float =
  case xs
  | [] => 0.
  | s :: rest => area(s) +. total(rest) +. true
#                                          ^^^^ Expecting type Float but got inconsistent type Bool #
  end
in|},
      render(~opened=["total"], example),
    )
  ),
  test_case("module depth is a setting", `Quick, () =>
    golden(
      "depth 1",
      {|# map · 1 error #
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  # 1 use #
let total(xs: [Shape]): Float = ⋯ in  # 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { ⋯ }  # 3 members #
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])|},
      render(
        ~settings={
          ...CompositionView.default_settings,
          module_depth: 1,
        },
        example,
      ),
    )
  ),
  test_case("test results, the value, probes", `Quick, () =>
    golden(
      "run",
      {|# map · no errors · tests: 1 of 2 pass, 1 fail #
let double(x) = ⋯ in  # open below · 3 uses #
test double(2) == 4 end;  # ✓ #
test ⋯ end;  # ✗ #
double(5)  # ⇒ 10 #

# open: double #
let double(x) =
  let y = ⟦x + x⟧ in     ≡ 4
  y
in|},
      render(
        ~evaluate=true,
        ~opened=["double"],
        {|let double(x) =
  let y = ^^probe(x + x) in
  y
in
test double(2) == 4 end;
test
  double(3)
  == 7
end;
double(5)|},
      ),
    )
  ),
  test_case("the map keeps a `fun` definition's parameters", `Quick, () =>
    golden(
      "fun heads",
      {|# map · no errors #
let pad = fun (n: Int) -> ⋯ in  # 1 use #
module M = {
  let twice = fun f -> fun x -> ⋯;
  let k = ⋯
} in
pad(1)|},
      render(
        {|let pad = fun (n: Int) ->
  if n < 10 then "0" else ""
in
module M = {
  let twice = fun f -> fun x -> f(f(x));
  let k = 3
} in
pad(1)|},
      ),
    )
  ),
  test_case("a module-rooted program; entries sharing a line", `Quick, () =>
    golden(
      "mod root",
      {|# map · 1 error #
let a = ⋯;  # 3 uses #
module N = { let b = ⋯; test b == 3 end; let c = ⋯ };  # b: 2 uses · c: 1 error: Expecting type Int but got inconsistent type Bool · N: open below #
test a == 1 end;  # open below #
print(a);
a + 1

# open: N #
module N = { let b = 2; test b == 3 end; let c = b + true }
#                                                    ^^^^ Expecting type Int but got inconsistent type Bool #

# open: test#1 #
test a == 1 end|},
      render(
        ~root=Mod,
        ~opened=["N", "test#1"],
        {|let a = 1;
module N = { let b = 2; test b == 3 end; let c = b + true };
test a == 1 end;
print(a);
a + 1|},
      ),
    )
  ),
  test_case("nested entries open by path", `Quick, () =>
    golden(
      "nested",
      {|# map · no errors #
let f(x) = ⋯ in  # 1 use #
module Geo = { module Units = { let cm = ⋯; let m = ⋯ } } in  # Units: open below #
f(1)

# open: f/y #
let y = x + 1 in

# open: Geo/Units #
module Units = { let cm = 1; let m = 100 }|},
      render(
        ~opened=["f/y", "Geo/Units"],
        {|let f(x) =
  let y = x + 1 in
  let z = y * 2 in
  z
in
module Geo = { module Units = { let cm = 1; let m = 100 } } in
f(1)|},
      ),
    )
  ),
  /* a livelit opens by its members' paths; the map lists a let-bound
     literal's members no more than other definitions' */
  test_case(
    "a livelit definition's members: listed in the map, open by path",
    `Quick,
    () =>
    golden(
      "livelit",
      {|# map · 1 error #
let ^pct = {  # 1 error: "^pct" isn't a valid token #
  type T = Int;
  let init = ⋯  # open below #
} in
let m = { let a = ⋯; let b = ⋯ } in  # a: 1 use · m: 1 use #
m.a

# open: ^pct/init #
let init = 50|},
      render(
        ~opened=["^pct/init"],
        {|let ^pct = {
  type T = Int;
  let init = 50
} in
let m = { let a = 1; let b = 2 } in
m.a|},
      ),
    )
  ),
  test_case("Mega 1k, two entries open", `Quick, () =>
    golden_file(
      "agent-view-mega-1k.txt",
      render(
        ~opened=["GroveNames/grove_name", "CropPlotter"],
        Option.get(CorpusUtil.mega_src("mega-1k.hz")),
      ),
    )
  ),
];

/* the open set keeps its entries by id: a rename keeps them open under
   their new path, and a reload's fresh ids find them by the old path */
let open_set_tests = [
  test_case(
    "an open entry survives a rename",
    `Quick,
    () => {
      let (seg, _, _) = program("let total = 1 in\nlet other = 2 in\ntotal");
      let o = Outline.build(~root=Exp, seg);
      let m =
        switch (
          AgentContext.Update.resolve(
            o,
            AgentContext.Update.OpenPaths(["total"]),
          )
        ) {
        | Ok(r) => AgentContext.Update.update(r, AgentContext.Utils.init())
        | Error(e) => Alcotest.fail(e)
        };
      let (seg', _) =
        CorpusUtil.edit_token(
          ~keep_id=true,
          ~needle="total",
          ~repl="sum",
          seg,
        );
      let o' = Outline.build(~root=Exp, seg');
      let m' = AgentContext.Utils.refresh(o', m);
      check(
        list(string),
        "open under its new name",
        ["sum"],
        List.map((e: AgentContext.Model.entry) => e.path, m'.opened),
      );
      check(
        bool,
        "the same id",
        true,
        AgentContext.Utils.ids(m) == AgentContext.Utils.ids(m'),
      );
    },
  ),
  test_case(
    "fresh ids find an entry by its path",
    `Quick,
    () => {
      let src = "module M = { let f = 1; let g = 2 } in M.f";
      let (seg, _, _) = program(src);
      let m =
        switch (
          AgentContext.Update.resolve(
            Outline.build(~root=Exp, seg),
            OpenPaths(["M/g"]),
          )
        ) {
        | Ok(r) => AgentContext.Update.update(r, AgentContext.Utils.init())
        | Error(e) => Alcotest.fail(e)
        };
      /* parsed again: new ids */
      let (seg', _, _) = program(src);
      let o' = Outline.build(~root=Exp, seg');
      let m' = AgentContext.Utils.refresh(o', m);
      check(int, "still open", 1, List.length(m'.opened));
      check(
        bool,
        "with the new id",
        true,
        AgentContext.Utils.ids(m') == ids_of(o', ["M/g"]),
      );
    },
  ),
  test_case(
    "an unknown path is an error naming the entries",
    `Quick,
    () => {
      let (seg, _, _) = program("let a = 1 in\nlet b = 2 in\na");
      switch (
        AgentContext.Update.resolve(
          Outline.build(~root=Exp, seg),
          OpenPaths(["c"]),
        )
      ) {
      | Ok(_) => Alcotest.fail("resolved")
      | Error(e) =>
        check(
          bool,
          "lists the candidates",
          true,
          Util.StringUtil.plain_search("a, b", e, 0) >= 0,
        )
      };
    },
  ),
  test_case(
    "close all closes every entry",
    `Quick,
    () => {
      let (seg, _, _) = program("let a = 1 in\nlet b = 2 in\na");
      let o = Outline.build(~root=Exp, seg);
      let step = (m, a) =>
        switch (AgentContext.Update.resolve(o, a)) {
        | Ok(r) => AgentContext.Update.update(r, m)
        | Error(e) => Alcotest.fail(e)
        };
      let m = step(AgentContext.Utils.init(), OpenPaths(["a", "b"]));
      check(int, "two open", 2, List.length(m.opened));
      let m = step(m, ClosePaths(["a"]));
      check(int, "one open", 1, List.length(m.opened));
      let m = step(m, CloseAllPaths);
      check(int, "none open", 0, List.length(m.opened));
    },
  ),
  test_case("the close tool's all flag", `Quick, () =>
    switch (
      CompositionUtils.action_of(
        ~tool_name="close",
        ~args=`Assoc([("all", `Bool(true))]),
      )
    ) {
    | Action(AgentContextAction(CloseAllPaths)) => ()
    | _ => Alcotest.fail("not CloseAllPaths")
    }
  ),
  test_case(
    "a saved open set from before ids reads",
    `Quick,
    () => {
      let m =
        AgentContext.Model.t_of_sexp(
          Sexplib.Sexp.of_string("((expanded_paths (a M/f)))"),
        );
      check(
        list(string),
        "paths kept",
        ["a", "M/f"],
        List.map((e: AgentContext.Model.entry) => e.path, m.opened),
      );
    },
  ),
];

/* what a request records is what its payload carries */
module W = Web;

let cws_of = (src: string): W.CodeWithStatics.Model.t => {
  let (_, z, st) = program(src);
  {
    ...W.CodeWithStatics.Model.mk(Editor.Model.mk(z, ~root=Exp)),
    statics: st,
  };
};

/* the context refreshed from [cws] and a request sent */
let send = (agent: W.Agent.Model.t, cws: W.CodeWithStatics.Model.t) => {
  let chat_id = agent.chat_system.current;
  let agent =
    W.AgentUtils.update_context(
      ~session_mode=W.AgentGlobals.Model.Edit,
      agent,
      cws,
      chat_id,
    );
  let (agent, messages) = W.AgentSend.main_request(chat_id, agent);
  (
    agent,
    messages,
    W.ChatSystem.Utils.find_chat(chat_id, agent.chat_system),
  );
};

let content = (m: Util.OpenRouter.Message.Model.t): string => m.content;

let record_tests = [
  test_case(
    "the record holds the context as sent",
    `Quick,
    () => {
      let agent = W.Agent.Utils.init();
      let cws = cws_of("let a = 1 in\nlet b = a + 1 in\nb");
      let (_, messages, chat) = send(agent, cws);
      let r = Option.get(Util.ListUtil.last_opt(chat.sent));
      check(
        string,
        "view = the payload's context",
        content(Util.ListUtil.last(messages)),
        r.view,
      );
      check(
        option(string),
        "the program",
        Some(W.SentView.text_of_zipper(cws.editor.state.zipper)),
        W.Chat.Utils.sent_text(chat, r),
      );
    },
  ),
  test_case(
    "an unchanged program is stored once",
    `Quick,
    () => {
      let agent = W.Agent.Utils.init();
      let cws = cws_of("let a = 1 in\na");
      let (agent, _, _) = send(agent, cws);
      let (_, _, chat) = send(agent, cws);
      switch (chat.sent) {
      | [first, second] =>
        check(bool, "first holds text", true, first.program != Unchanged);
        check(bool, "second refers back", true, second.program == Unchanged);
        check(
          option(string),
          "second reads the text",
          W.Chat.Utils.sent_text(chat, first),
          W.Chat.Utils.sent_text(chat, second),
        );
      | rs => fail(Printf.sprintf("%d records", List.length(rs)))
      };
    },
  ),
  test_case(
    "the open set is recorded with its paths",
    `Quick,
    () => {
      let agent = W.Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let cws = cws_of("let a = 1 in\nlet b = 2 in\na");
      let o =
        Outline.of_program(
          ~root=Exp,
          Zipper.unselect_and_zip(cws.editor.state.zipper),
        );
      let opened =
        switch (AgentContext.Update.resolve(o, OpenPaths(["b"]))) {
        | Ok(r) => r
        | Error(e) => fail(e)
        };
      let agent = {
        ...agent,
        chat_system:
          W.ChatSystem.Update.update(
            ChatAction(AgentContextAction(opened), chat_id),
            agent.chat_system,
          )
          |> W.ChatSystem.Update.get,
      };
      let (_, _, chat) = send(agent, cws);
      let r = Option.get(Util.ListUtil.last_opt(chat.sent));
      check(
        list(string),
        "opened",
        ["b"],
        List.map((e: AgentContext.Model.entry) => e.path, r.opened),
      );
      check(
        bool,
        "the view shows it",
        true,
        Util.StringUtil.plain_search("# open: b #", r.view, 0) >= 0,
      );
    },
  ),
  test_case(
    "records persist with the chat",
    `Quick,
    () => {
      let agent = W.Agent.Utils.init();
      let (_, _, chat) = send(agent, cws_of("let a = 1 in\na"));
      let back = W.Chat.Model.t_of_sexp(W.Chat.Model.sexp_of_t(chat));
      check(int, "records", 1, List.length(back.sent));
      check(
        string,
        "view",
        List.hd(chat.sent).view,
        List.hd(back.sent).view,
      );
    },
  ),
  test_case(
    "old records drop past the bound",
    `Quick,
    () => {
      let b: W.SentView.basis = {
        b_root: Exp,
        b_text: "x",
        b_opened: [],
        b_probes: 0,
      };
      let r = (seq): W.SentView.t => {
        seq,
        time: 0.,
        after: Id.invalid,
        view: "v",
        opened: [],
        root: Exp,
        program: Dropped,
        probes: 0,
      };
      let rs =
        List.fold_left(
          (rs, i) =>
            W.SentView.add(
              ~after_text=_ => None,
              ~last_tool=None,
              rs,
              {
                ...b,
                b_text: string_of_int(i / 10),
              },
              r(i),
            ),
          [],
          List.init(W.SentView.max_records + 50, i => i),
        );
      check(int, "kept", W.SentView.max_records, List.length(rs));
      let first = List.hd(rs);
      check(
        option(string),
        "the oldest kept has its text",
        Some(string_of_int(first.seq / 10)),
        W.SentView.text(~after_text=_ => None, ~earlier=[], first),
      );
    },
  ),
];

/* View as agent: the workspace shows a request's view, read-only, and
   gives the user's program and view back untouched */
let lens_src = "let a = 1 in\nlet b = a + 1 in\nlet c = b * 2 in\nc";

/* a slide whose agent chat sent one request with [opened] open */
let slide = (~opened: list(string)) => {
  let cws = cws_of(lens_src);
  let agent = W.Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  let o =
    Outline.of_program(
      ~root=Exp,
      Zipper.unselect_and_zip(cws.editor.state.zipper),
    );
  let agent =
    switch (
      opened == []
        ? Ok(AgentContext.Update.CloseAll)
        : AgentContext.Update.resolve(o, OpenPaths(opened))
    ) {
    | Ok(r) => {
        ...agent,
        chat_system:
          W.ChatSystem.Update.update(
            ChatAction(AgentContextAction(r), chat_id),
            agent.chat_system,
          )
          |> W.ChatSystem.Update.get,
      }
    | Error(e) => fail(e)
    };
  let (agent, _, _) = send(agent, cws);
  let editor: W.CellEditor.Model.t = {
    ...W.CellEditor.Model.mk(cws.editor),
    editor: cws,
  };
  let code: W.ScratchModel.Scratchpad.code = {
    program: Whole(editor),
    view: W.SlideView.init,
    agent,
    lens: None,
  };
  (code, chat_id);
};

let step = (a: W.Workspace.Action.t, code) =>
  W.Workspace.update(
    ~settings=W.Settings.Model.init,
    ~schedule_action=_ => (),
    ~slide_key="lens-test",
    a,
    code,
  ).
    model;

let lens_tests = [
  test_case(
    "view as agent shows the request's open entries",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b", "c"]);
      let code' = step(AgentAction(ViewAsAgent(chat_id, Some(1))), code);
      switch (code'.lens) {
      | Some(l) =>
        check(int, "the request", 1, l.seq);
        check(bool, "not live", false, l.live);
        switch (l.program) {
        | Divided(d) =>
          check(
            list(string),
            "cells for b and c",
            ["b", "c"],
            List.filter_map(
              (e: W.ScratchCell.t) => W.ScratchCell.header_name(e),
              W.Divided.cells(d),
            ),
          )
        | Whole(_) => fail("no cells")
        };
      | None => fail("no lens")
      };
      check(bool, "program untouched", true, code'.program === code.program);
      check(bool, "view untouched", true, code'.view === code.view);
    },
  ),
  test_case(
    "nothing open: the map, no cells",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=[]);
      let code' = step(AgentAction(ViewAsAgent(chat_id, Some(1))), code);
      switch (code'.lens) {
      | Some({program: Whole(_), map, _}) =>
        check(
          bool,
          "the map, as sent",
          true,
          String.starts_with(~prefix="# map", map)
          && Util.StringUtil.plain_search({js|let a = ⋯ in|js}, map, 0) >= 0,
        )
      | Some(_) => fail("cells")
      | None => fail("no lens")
      };
    },
  ),
  test_case(
    "the shown request is read-only",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b"]);
      let code' = step(AgentAction(ViewAsAgent(chat_id, Some(1))), code);
      let a =
        switch (
          Outline.resolve(
            Outline.of_program(~root=Exp, W.Program.document(code.program)),
            "a",
          )
        ) {
        | Ok(e) => e.id
        | Error(e) => fail(e)
        };
      let code'' = step(FocusToggle(a), code');
      check(bool, "program untouched", true, code''.program === code.program);
      check(bool, "view untouched", true, code''.view === code.view);
    },
  ),
  test_case(
    "leaving gives the user's view back",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b"]);
      let code' =
        code
        |> step(AgentAction(ViewAsAgent(chat_id, Some(1))))
        |> step(AgentAction(LeaveAgentView));
      check(bool, "no lens", true, Option.is_none(code'.lens));
      check(bool, "program untouched", true, code'.program === code.program);
    },
  ),
  test_case(
    "live follows the next request",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b"]);
      let code = step(AgentAction(ViewAsAgent(chat_id, None)), code);
      check(
        option(bool),
        "live",
        Some(true),
        Option.map((l: W.AgentLens.t) => l.live, code.lens),
      );
      /* the agent sends again */
      let (agent, _, _) = send(code.agent, cws_of(lens_src));
      let code = {
        ...code,
        agent,
      };
      let code = step(AgentAction(SetActiveTimelineNode(None)), code);
      check(
        option(int),
        "the second request",
        Some(2),
        Option.map((l: W.AgentLens.t) => l.seq, code.lens),
      );
    },
  ),
  test_case(
    "the map's folds: modules open, bodies closed",
    `Quick,
    () => {
      let term =
        CorpusUtil.outline_of(
          CorpusUtil.seg_of(
            "module M = { let f(x) = let y = x in y; module N = { let z = 1 } } in\nM.f(1)",
          ),
        );
      check(
        list(bool),
        "M open; f and N closed",
        [false, true, true],
        List.map(snd, W.AgentLens.map_folds(~module_depth=1, term)),
      );
    },
  ),
];

/* each of the view's switches, off, takes its mark out of the view: what
   a request sends, so ablations reproduce and View as agent shows them */
let ablated = {|type T = Int in
let f(x) =
  let y = ^^probe(x + 1) in
  test y == x + 1 end;
  y
in
let g = 1 + true in
test f(1) == 2 end;
module M = { module N = { let z = 1 } } in
f(1)|};

let toggle_case = (name, off: CompositionView.settings, mark) =>
  test_case(
    name,
    `Quick,
    () => {
      let shown = settings =>
        render(~evaluate=true, ~opened=["f", "g"], ~settings, ablated);
      let has = (s, m) => Util.StringUtil.plain_search(m, s, 0) >= 0;
      check(
        bool,
        name ++ ": on",
        true,
        has(shown(CompositionView.default_settings), mark),
      );
      check(bool, name ++ ": off", false, has(shown(off), mark));
    },
  );

let d = CompositionView.default_settings;

let toggle_tests = [
  toggle_case(
    "use counts",
    {
      ...d,
      uses: false,
    },
    "2 uses",
  ),
  toggle_case(
    "errors on the map",
    {
      ...d,
      errors: false,
    },
    "1 error: Expecting type Int but got inconsistent type Bool",
  ),
  toggle_case(
    "test status",
    {
      ...d,
      test_status: false,
    },
    "== 2 end;  # ✓ #",
  ),
  toggle_case(
    "the program's value",
    {
      ...d,
      value: false,
    },
    {js|# ⇒ 2 #|js},
  ),
  toggle_case(
    "error marks",
    {
      ...d,
      carets: false,
    },
    "#           ^^^^ Expecting type Int",
  ),
  toggle_case(
    "probe values",
    {
      ...d,
      probes: false,
    },
    {js|≡ 2|js},
  ),
  toggle_case(
    "test marks",
    {
      ...d,
      test_marks: false,
    },
    "x + 1 end;  # ✓ #",
  ),
  toggle_case(
    "module depth",
    {
      ...d,
      module_depth: 1,
    },
    "let z = ⋯",
  ),
  toggle_case(
    "types in full",
    {
      ...d,
      types_in_full: false,
    },
    "type T = Int",
  ),
  test_case(
    "a setting reaches what a request sends",
    `Quick,
    () => {
      let g = W.AgentGlobals.init();
      let agent = W.Agent.Utils.init();
      let chat_id = agent.chat_system.current;
      let cws = cws_of("let a = 1 in\nlet b = a + a in\nb");
      let sent = (g: W.AgentGlobals.Model.t) => {
        let agent =
          W.AgentUtils.update_context(
            ~session_mode=Edit,
            ~view=W.AgentGlobals.view_settings(g),
            agent,
            cws,
            chat_id,
          );
        let (agent, _) = W.AgentSend.main_request(chat_id, agent);
        Option.get(
          Util.ListUtil.last_opt(
            W.ChatSystem.Utils.find_chat(chat_id, agent.chat_system).sent,
          ),
        ).
          view;
      };
      let has = (s, m) => Util.StringUtil.plain_search(m, s, 0) >= 0;
      check(bool, "uses on", true, has(sent(g), "# 2 uses #"));
      check(
        bool,
        "uses off",
        false,
        has(
          sent(W.AgentGlobals.Update.update(ToggleView(Uses), g, _ => ())),
          "# 2 uses #",
        ),
      );
    },
  ),
];

let tests = (
  "AgentView",
  format_tests @ open_set_tests @ record_tests @ lens_tests @ toggle_tests,
);
