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
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  #› 1 use #
let total(xs: [Shape]): Float = ⋯ in  #› 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
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
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  #› 1 use #
let total(xs: [Shape]): Float = ⋯ in  #› open below · 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { let cm = ⋯; let m = ⋯; let km = ⋯ }
} in
hint "total of one circle" test total([Circle(1.)]) == 3.14 end;
total([Rect(2., 3.)])

#› open entries, in full #
#› open: total #
let total(xs: [Shape]): Float =
  case xs
  | [] => 0.
  | s :: rest => area(s) +. total(rest) +. true
#›                                         ^^^^ Expecting type Float but got inconsistent type Bool #
  end
in|},
      render(~opened=["total"], example),
    )
  ),
  test_case("module depth is a setting", `Quick, () =>
    golden(
      "depth 1",
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
type Shape = Circle(Float) + Rect(Float, Float) in
let area(s: Shape): Float = ⋯ in  #› 1 use #
let total(xs: [Shape]): Float = ⋯ in  #› 1 error: Expecting type Float but got inconsistent type Bool · 3 uses #
module Geo = {
  let perimeter(s: Shape): Float = ⋯;
  let scale(k: Float, s: Shape): Shape = ⋯;
  module Units = { ⋯ }  #› 3 members #
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
      "#› program map · no errors · 1 of 2 tests pass, 1 fails #\n"
      ++ CompositionView.legend
      ++ {|
let double(x) = ⋯ in  #› open below · 3 uses #
test double(2) == 4 end;  #› test#1 · ✓ #
test ⋯ end;  #› test#2 · ✗ #
double(5)  #› ⇒ 10 #

#› open entries, in full #
#› open: double #
let double(x) =
  let y = ⟦x + x⟧ in  #› ≡ 4 #
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
  test_case(
    "an error at column 0: its marks right after the marker", `Quick, () =>
    golden(
      "column 0",
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
let g = ⋯ in  #› open below · 1 error: Expecting type Int but got inconsistent type Bool · 1 use #
g

#› open entries, in full #
#› open: g #
let g =
true + 1 in
#›^^^^ Expecting type Int but got inconsistent type Bool #|},
      render(~opened=["g"], "let g =\ntrue + 1 in\ng"),
    )
  ),
  test_case("the map keeps a `fun` definition's parameters", `Quick, () =>
    golden(
      "fun heads",
      "#› program map · no errors #\n"
      ++ CompositionView.legend
      ++ {|
let pad = fun (n: Int) -> ⋯ in  #› 1 use #
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
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
let a = ⋯;  #› 3 uses #
module N = { let b = ⋯; test b == 3 end; let c = ⋯ };  #› b: 2 uses · N/test#1 · c: 1 error: Expecting type Int but got inconsistent type Bool · N: open below #
test a == 1 end;  #› test#1 · open below #
print(a);  #› stmt#1 #
a + 1

#› open entries, in full #
#› open: N #
module N = { let b = 2; test b == 3 end; let c = b + true }  #› N/test#1 #
#›                                                   ^^^^ Expecting type Int but got inconsistent type Bool #

#› open: test#1 #
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
      "#› program map · no errors #\n"
      ++ CompositionView.legend
      ++ {|
let f(x) = ⋯ in  #› 1 use #
module Geo = { module Units = { let cm = ⋯; let m = ⋯ } } in  #› Units: open below #
f(1)

#› open entries, in full #
#› open: f/y #
let y = x + 1 in

#› open: Geo/Units #
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
      "#› program map · 1 error #\n"
      ++ CompositionView.legend
      ++ {|
let ^pct = {  #› 1 error: "^pct" isn't a valid token #
  type T = Int;
  let init = ⋯  #› open below #
} in
let m = { let a = ⋯; let b = ⋯ } in  #› a: 1 use · m: 1 use #
m.a

#› open entries, in full #
#› open: ^pct/init #
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

/* a request's inputs besides its messages */
let request: W.SentView.request = {
  model_id: "anthropic/claude-test",
  reasoning: None,
  mode: Edit,
  disabled_tools: [],
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
  let (agent, payload) = W.AgentSend.main_request(~request, chat_id, agent);
  (
    agent,
    payload.messages,
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
        Util.StringUtil.plain_search({js|#› open: b #|js}, r.view, 0) >= 0,
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
        request: None,
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
      | Some({program: Whole(_), sent, summary, _}) =>
        check(
          bool,
          "the view, as sent",
          true,
          String.starts_with(~prefix={js|#› program map|js}, sent)
          && Util.StringUtil.plain_search({js|let a = ⋯ in|js}, sent, 0)
          >= 0,
        );
        check(string, "the summary", "no errors", summary);
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
    {js|== 2 end;  #› test#1 · ✓ #|js},
  ),
  toggle_case(
    "the program's value",
    {
      ...d,
      value: false,
    },
    {js|#› ⇒ 2 #|js},
  ),
  toggle_case(
    "error marks",
    {
      ...d,
      carets: false,
    },
    {js|#›          ^^^^ Expecting type Int|js},
  ),
  toggle_case(
    "probe values",
    {
      ...d,
      probes: false,
    },
    {js|#› ≡ 2 #|js},
  ),
  toggle_case(
    "test marks",
    {
      ...d,
      test_marks: false,
    },
    {js|x + 1 end;  #› f/test#1 · ✓ #|js},
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
        let (agent, _) = W.AgentSend.main_request(~request, chat_id, agent);
        Option.get(
          Util.ListUtil.last_opt(
            W.ChatSystem.Utils.find_chat(chat_id, agent.chat_system).sent,
          ),
        ).
          view;
      };
      let has = (s, m) => Util.StringUtil.plain_search(m, s, 0) >= 0;
      check(bool, "uses on", true, has(sent(g), {js|#› 2 uses #|js}));
      check(
        bool,
        "uses off",
        false,
        has(
          sent(W.AgentGlobals.Update.update(ToggleView(Uses), g, _ => ())),
          {js|#› 2 uses #|js},
        ),
      );
    },
  ),
];

/* Trajectory: what the lens shows is what AgentSend sent. The agent's
   loop runs for real; only the HTTP request is replaced, by one that
   keeps each body as start_streaming_chat would send it */
module T = W.Trajectory;

let llm: Util.OpenRouter.AvailableLLMs.Model.llm_info = {
  id: "anthropic/claude-test",
  name: "Test",
  pricing: {
    prompt: "0",
    completion: "0",
  },
  context_length: None,
  supports_reasoning: false,
};

let api_settings = {
  ...W.Settings.Model.init,
  agent_globals: {
    ...W.Settings.Model.init.agent_globals,
    api_key: Some("test-key"),
    active_llm: Some(llm),
  },
};

/* a chat driven through [W.Agent.Update.update]: each [turns] entry a
   user message then the replies it gets, each reply a tool call list
   (text when empty); the bodies sent, in order, and the agent after */
let drive =
    (
      ~src=lens_src,
      turns: list((string, list(list((string, Yojson.Safe.t))))),
    )
    : (list(string), W.Agent.Model.t) => {
  let bodies = ref([]);
  let saved = (
    W.AgentSend.stream_chat^,
    W.AgentSend.defer_dispatch_send^,
    W.AgentSend.max_eval_wait_attempts^,
  );
  W.AgentSend.stream_chat :=
    (
      (~payload, ~key as _, ~on_chunk as _, ~on_done as _) => {
        bodies :=
          bodies^
          @ [
            Util.API.Json.to_string(
              Util.OpenRouter.Payload.Utils.json_of_payload(
                ~payload=Util.OpenRouter.Utils.streaming(payload),
              ),
            ),
          ];
        {abort: () => ()};
      }
    );
  W.AgentSend.defer_dispatch_send := (thunk => thunk());
  W.AgentSend.max_eval_wait_attempts := 0;
  let queue = Queue.create();
  let agent = W.Agent.Utils.init();
  let chat_id = agent.chat_system.current;
  /* named, so no naming request goes out */
  let agent = {
    ...agent,
    chat_system:
      W.ChatSystem.Update.update(
        ChatAction(SetTitle("Trajectory test"), chat_id),
        agent.chat_system,
      )
      |> W.ChatSystem.Update.get,
  };
  let ce = ref(W.CellEditor.Model.mk(cws_of(src).editor));
  let agent = ref(agent);
  let step = (a: W.Agent.Update.Action.t) => {
    let (agent', ce') =
      W.Agent.Update.update(a, agent^, ce^, api_settings, a =>
        Queue.add(a, queue)
      );
    agent := agent';
    ce := ce'.model;
  };
  let drain = () =>
    while (!Queue.is_empty(queue)) {
      switch (Queue.pop(queue)) {
      /* the replies below stand in for the stream */
      | StreamDelta(_) => ()
      | a => step(a)
      };
    };
  List.iteri(
    (t, (says, replies)) => {
      step(SendMessage(W.Message.Utils.mk_user_message(says), chat_id));
      drain();
      List.iteri(
        (i, calls) => {
          let reply: Util.OpenRouter.Reply.Model.t = {
            content: calls == [] ? "Done." : "",
            tool_calls:
              List.mapi(
                (j, (name, args)): Util.OpenRouter.Reply.Model.tool_call =>
                  {
                    id: Printf.sprintf("call-%d-%d-%d", t, i, j),
                    name,
                    args,
                  },
                calls,
              ),
            usage: None,
            reasoning: None,
          };
          step(HandleLLMResponse(reply, chat_id, agent^.main_llm_seq, 0));
          drain();
        },
        replies,
      );
    },
    turns,
  );
  let (sc, dd, mw) = saved;
  W.AgentSend.stream_chat := sc;
  W.AgentSend.defer_dispatch_send := dd;
  W.AgentSend.max_eval_wait_attempts := mw;
  (bodies^, agent^);
};

let targets = paths =>
  `Assoc([("targets", `List(List.map(p => `String(p), paths)))]);

let chat_of = (agent: W.Agent.Model.t) =>
  W.ChatSystem.Utils.find_chat(agent.chat_system.current, agent.chat_system);

let messages_of = (body: string): list(string) =>
  switch (Yojson.Safe.from_string(body)) {
  | `Assoc(fs) =>
    switch (List.assoc_opt("messages", fs)) {
    | Some(`List(ms)) => List.map(Yojson.Safe.to_string, ms)
    | _ => []
    }
  | _ => []
  };

let trajectory_tests = [
  test_case(
    "each request rebuilds to the body AgentSend sent",
    `Quick,
    () => {
      let (bodies, agent) =
        drive([
          ("open b", [[("open", targets(["b"]))], []]),
          (
            "now c and a",
            [[("open", targets(["c"])), ("close", targets(["b"]))], []],
          ),
        ]);
      let chat = chat_of(agent);
      check(int, "requests", 4, List.length(bodies));
      check(int, "records", 4, List.length(chat.sent));
      List.iteri(
        (i, body) =>
          check(
            option(string),
            Printf.sprintf("request %d", i + 1),
            Some(body),
            T.body(chat, i + 1),
          ),
        bodies,
      );
    },
  ),
  test_case(
    "the trajectory shows every message as sent, contexts where they went",
    `Quick,
    () => {
      let (bodies, agent) =
        drive([
          ("open b", [[("open", targets(["b"]))], []]),
          ("thanks", [[]]),
        ]);
      let chat = chat_of(agent);
      let t = Option.get(T.build(chat, 3));
      let last = Util.ListUtil.last(bodies);
      check(string, "the body", last, t.body);
      let raws = List.map((s: T.section) => s.raw, t.sections);
      /* the last body's messages, in order, among the sections */
      let rec subseq = (want, got) =>
        switch (want, got) {
        | ([], _) => true
        | ([w, ...ws], [g, ...gs]) =>
          w == g ? subseq(ws, gs) : subseq(want, gs)
        | (_, []) => false
        };
      check(
        bool,
        "messages in order",
        true,
        subseq(messages_of(last), raws),
      );
      /* the earlier requests' contexts too, in order */
      check(
        bool,
        "each context",
        true,
        subseq(
          List.map(b => Util.ListUtil.last(messages_of(b)), bodies),
          raws,
        ),
      );
      let kinds = List.map((s: T.section) => s.kind, t.sections);
      check(
        bool,
        "prompt, tools, request first",
        true,
        Util.ListUtil.take(3, kinds) == [System, Tools, Request],
      );
      let calls =
        List.concat_map(
          (s: T.section) =>
            List.filter((c: T.section) => c.kind == Call, s.children),
          t.sections,
        );
      check(
        list(string),
        "the call's arguments, exactly",
        [{|{"targets":["b"]}|}],
        List.map((c: T.section) => c.text, calls),
      );
      check(
        list(bool),
        "contexts folded but this request's",
        [true, true, false],
        List.filter_map(
          (s: T.section) => s.kind == Context ? Some(s.folded) : None,
          t.sections,
        ),
      );
      check(
        list(bool),
        "only this request's context is in it",
        [false, false, true],
        List.filter_map(
          (s: T.section) => s.kind == Context ? Some(s.in_request) : None,
          t.sections,
        ),
      );
    },
  ),
  test_case(
    "the mode is kept for the session",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b"]);
      let code = step(AgentAction(ViewAsAgent(chat_id, Some(1))), code);
      let code = step(LensMode(Trajectory), code);
      check(
        bool,
        "a trajectory",
        true,
        Option.is_some(
          Option.bind(code.lens, (l: W.AgentLens.t) => l.trajectory),
        ),
      );
      let code =
        code
        |> step(AgentAction(LeaveAgentView))
        |> step(AgentAction(ViewAsAgent(chat_id, Some(1))));
      check(
        option(bool),
        "still Trajectory",
        Some(true),
        Option.map((l: W.AgentLens.t) => l.mode == Trajectory, code.lens),
      );
      let code = step(LensMode(Stacked), code);
      check(
        option(bool),
        "Stacked drops it",
        Some(true),
        Option.map((l: W.AgentLens.t) => l.trajectory == None, code.lens),
      );
    },
  ),
  test_case(
    "Sent view: the agentEditorView, byte for byte",
    `Quick,
    () => {
      let (code, chat_id) = slide(~opened=["b"]);
      let code = step(AgentAction(ViewAsAgent(chat_id, Some(1))), code);
      let r = Option.get(Util.ListUtil.last_opt(chat_of(code.agent).sent));
      let l = Option.get(code.lens);
      check(
        bool,
        "inside the context as sent",
        true,
        Util.StringUtil.plain_search(
          "<agentEditorView>\n```\n" ++ l.sent ++ "\n```\n</agentEditorView>",
          r.view,
          0,
        )
        >= 0,
      );
    },
  ),
  /* the cost of storing bodies against rebuilding them;
     HAZEL_MEASURE=1 prints it */
  test_case(
    "a request's record grows little; its body would grow with the chat",
    `Quick,
    () => {
      let n = 35;
      let (bodies, agent) =
        drive(
          List.init(n / 2 + 1, i =>
            (
              "turn " ++ string_of_int(i),
              [[("open", targets([i mod 2 == 0 ? "b" : "c"]))], []],
            )
          ),
        );
      let chat = chat_of(agent);
      let sexp_len = sx => String.length(Sexplib.Sexp.to_string(sx));
      let records = sexp_len(W.Chat.Model.sexp_of_t(chat));
      let without =
        sexp_len(
          W.Chat.Model.sexp_of_t({
            ...chat,
            sent:
              List.map(
                (r: W.SentView.t) =>
                  {
                    ...r,
                    request: None,
                  },
                chat.sent,
              ),
          }),
        );
      let total = List.fold_left((n, b) => n + String.length(b), 0, bodies);
      if (Sys.getenv_opt("HAZEL_MEASURE") == Some("1")) {
        let t0 = Util.JsUtil.timestamp();
        ignore(T.build(chat, List.length(bodies)));
        Printf.printf(
          "trajectory of the last request built in %.0f ms\n",
          Util.JsUtil.timestamp() -. t0,
        );
        Printf.printf(
          "%d requests: bodies %d bytes (largest %d); chat sexp %d, of it the request fields %d\n",
          List.length(bodies),
          total,
          List.fold_left((m, b) => max(m, String.length(b)), 0, bodies),
          records,
          records - without,
        );
      };
      check(bool, "30+ requests", true, List.length(bodies) >= 30);
      check(
        bool,
        "the request fields: under 100 bytes each",
        true,
        records - without < 100 * List.length(bodies),
      );
    },
  ),
];

let tests = (
  "AgentView",
  format_tests
  @ open_set_tests
  @ record_tests
  @ lens_tests
  @ toggle_tests
  @ trajectory_tests,
);
