open Alcotest;
open Haz3lcore;
open Language;
open Web;
open Test_AgentTools;

let tests = (
  "AgentHardening",
  [
    test_case(
      "old saved tool result remains readable",
      `Quick,
      () => {
        let result =
          AgentToolResult.mk_skipped({
            id: "old",
            name: "edit",
            args: `Null,
          });
        let legacy =
          switch (AgentToolResult.sexp_of_tool_result(result)) {
          | Sexplib.Sexp.List(fields) =>
            Sexplib.Sexp.List(
              List.filter(
                fun
                | Sexplib.Sexp.List([
                    Sexplib.Sexp.Atom("content_is_payload"),
                    ..._,
                  ]) =>
                  false
                | _ => true,
                fields,
              ),
            )
          | s => s
          };
        ignore(AgentToolResult.tool_result_of_sexp(legacy));
      },
    ),
    test_case(
      "large valid boundary insert uses batch parser",
      `Quick,
      () => {
        let code =
          "let big = ["
          ++ String.concat(", ", List.init(850, string_of_int))
          ++ "] in";
        check(
          bool,
          "batch parser accepts this code",
          true,
          Option.is_some(FastParse.of_text(~root=Exp, code)),
        );
        switch (run_insert_at_program_boundary("?", Before, code)) {
        | Ok(_) => ()
        | Error(e) => fail(Action.Failure.show(e))
        };
      },
    ),
    test_case(
      "member comment semicolon is preserved",
      `Quick,
      () => {
        let code = "let a = 1 # first; second #; let b = 2";
        let actual =
          apply_and_render(
            "let m = { let x = 0 } in m",
            Insert(After, "m/x", code),
          );
        check_rendered(
          "comment preserved",
          "let m = { let x = 0; " ++ code ++ " } in m",
          actual,
        );
      },
    ),
    test_case(
      "offered statics respects disabling statics",
      `Quick,
      () => {
        let z = mk_zipper("let a = 1 in a");
        let st =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~root=Exp,
            ~is_dynamic_term=false,
            z,
          );
        CachedStatics.offer(~settings=CoreSettings.on, z, st);
        let m =
          CodeWithStatics.Model.mk(
            ~statics=st,
            Editor.Model.mk(~root=Exp, z),
          );
        let m =
          CodeWithStatics.Update.calculate(
            ~settings={
              ...CoreSettings.on,
              statics: false,
            },
            ~is_edited=true,
            ~statics_mode=Force,
            ~stitch=x => x,
            ~dynamics=Language.Dynamics.Map.empty,
            ~is_dynamic_term=false,
            m,
          );
        check(
          bool,
          "statics disabled",
          true,
          Id.Map.is_empty(m.statics.info_map),
        );
        CachedStatics.offered := [];
      },
    ),
    test_case(
      "unrelated module hole survives an edit",
      `Quick,
      () => {
        let code = "let m = { let x = 1; ?; let y = 2 } in let z = 0 in z";
        let actual = apply_and_render(code, Update(Definition, "z", "1"));
        check_rendered(
          "hole preserved",
          "let m = { let x = 1; ?; let y = 2 } in let z = 1 in z",
          actual,
        );
      },
    ),
    test_case(
      "edited program must not count old evaluation as settled",
      `Quick,
      () => {
        CachedStatics.offered := [];
        let settings = CoreSettings.on;
        let ce =
          CellEditor.Model.mk(
            Editor.Model.mk(~root=Exp, mk_zipper("let a = 1 in a")),
          );
        let ce =
          CellEditor.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~statics_mode=Force,
            ~queue_worker=None,
            ~stitch=x => x,
            ce,
          );
        check(
          bool,
          "initial evaluation settled",
          true,
          AgentSend.eval_settled(ce.result),
        );
        let agent = Agent.Utils.init();
        let editor =
          switch (
            AgentToolCallHandler.update(
              ~settings={
                ...Settings.Model.init,
                core: settings,
              },
              EditorAction(Update(Definition, "a", "2")),
              agent,
              ce.editor,
              agent.chat_system.current,
            )
          ) {
          | Ok((_, editor)) => editor
          | Error(_) => fail("edit failed")
          };
        let requests = ref([]);
        let statics_mode =
          CodeWithStatics.StaticsDebounce.consume(
            ~is_edited=true, ~schedule_refresh=() =>
            fail("agent edit was debounced")
          );
        check(
          bool,
          "agent edit forces statics",
          true,
          statics_mode == StaticsMode.Force,
        );
        let ce =
          CellEditor.Update.calculate(
            ~settings,
            ~is_edited=true,
            ~statics_mode,
            ~queue_worker=Some(r => requests := [r, ...requests^]),
            ~stitch=x => x,
            {
              ...ce,
              editor,
            },
          );
        check(int, "evaluation request queued", 1, List.length(requests^));
        check(
          bool,
          "edited evaluation is pending",
          false,
          AgentSend.eval_settled(ce.result),
        );
      },
    ),
    test_case(
      "unrelated module layout survives a definition edit",
      `Quick,
      () => {
        let code = "let m = {\n  let x = 1;\n  let y = 2\n} in let z = 0 in z";
        let actual = apply_and_render(code, Update(Definition, "z", "1"));
        check_rendered_exact(
          "unrelated module",
          "let m = {\n  let x = 1;\n  let y = 2\n} in let z = 1 in z",
          actual,
        );
      },
    ),
    test_case(
      "an unrelated module's tight spacing survives a definition edit",
      `Quick,
      () => {
        /* materialization's spacing repair skips unchanged syntax */
        let code = "let q = 1 in\nmodule M = {let x = 1;let y = 2} in M.x";
        check_rendered_exact(
          "unrelated tight module",
          "let q = 5 in\nmodule M = {let x = 1;let y = 2} in M.x",
          apply_and_render(code, Update(Definition, "q", "5")),
        );
      },
    ),
    test_case(
      "statics handoff rejects changed tokens with the same IDs",
      `Quick,
      () => {
        let z = mk_zipper("let a = 1 in a + 1");
        let st =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~root=Exp,
            ~is_dynamic_term=false,
            z,
          );
        CachedStatics.offer(~settings=CoreSettings.on, z, st);
        let z =
          ZipperBase.MapPiece.go(
            p =>
              switch (p) {
              | Tile(t) when t.form == Form.Tok("1") => [
                  Tile({
                    ...t,
                    form: Form.Tok("true"),
                  }),
                ]
              | p => [p]
              },
            z,
          );
        let m =
          CodeWithStatics.Model.mk(
            ~statics=st,
            Editor.Model.mk(~root=Exp, z),
          );
        let m =
          CodeWithStatics.Update.calculate(
            ~settings=CoreSettings.on,
            ~is_edited=true,
            ~statics_mode=Force,
            ~stitch=x => x,
            ~dynamics=Language.Dynamics.Map.empty,
            ~is_dynamic_term=false,
            m,
          );
        check(
          bool,
          "changed program has type errors",
          true,
          m.statics.error_ids != [],
        );
        CachedStatics.offered := [];
      },
    ),
    test_case(
      "agent context refreshes when statics arrive",
      `Quick,
      () => {
        let z = mk_zipper("let a = missing in a");
        let editor = CodeWithStatics.Model.mk(Editor.Model.mk(~root=Exp, z));
        let agent = Agent.Utils.init();
        let chat_id = agent.chat_system.current;
        let agent =
          AgentUtils.update_context(
            ~session_mode=Settings.Model.init.agent_globals.session_mode,
            agent,
            editor,
            chat_id,
          );
        let statics =
          CachedStatics.init(
            ~settings=CoreSettings.on,
            ~stitch=x => x,
            ~root=Exp,
            ~is_dynamic_term=false,
            z,
          );
        let agent =
          AgentUtils.update_context(
            ~session_mode=Settings.Model.init.agent_globals.session_mode,
            agent,
            {
              ...editor,
              statics,
            },
            chat_id,
          );
        let content =
          ChatSystem.Utils.find_chat(chat_id, agent.chat_system)
          |> Chat.Utils.get
          |> List.map((m: Message.Model.t) => m.content)
          |> String.concat("\n");
        check(
          bool,
          "new errors appear in context",
          true,
          Util.StringUtil.plain_search("not bound", content, 0) >= 0,
        );
      },
    ),
    test_case(
      "send waits for pending statics even with a settled old result",
      `Quick,
      () => {
        let agent = Agent.Utils.init();
        let chat_id = agent.chat_system.current;
        let agent = {
          ...agent,
          pending_dispatch_send: Some(chat_id),
        };
        let ce =
          CellEditor.Model.mk(
            Editor.Model.mk(~root=Exp, mk_zipper("let a = 1 in a")),
          );
        let ce =
          CellEditor.Update.calculate(
            ~settings=CoreSettings.on,
            ~is_edited=true,
            ~statics_mode=Force,
            ~queue_worker=None,
            ~stitch=x => x,
            ce,
          );
        let saved_budget = AgentSend.max_eval_wait_attempts^;
        let saved_attempts = AgentSend.eval_wait_attempts^;
        AgentSend.max_eval_wait_attempts := 3;
        List.iter(
          dynamics => {
            CodeWithStatics.StaticsDebounce.force_on_next := true;
            AgentSend.eval_wait_attempts := 0;
            let settings = {
              ...Settings.Model.init,
              core: {
                ...CoreSettings.on,
                dynamics,
              },
            };
            let scheduled = ref([]);
            let (agent, _) =
              AgentSend.handle_dispatch_send(chat_id, agent, ce, settings, a =>
                scheduled := [a, ...scheduled^]
              );
            check(
              bool,
              "send remains pending",
              true,
              agent.pending_dispatch_send == Some(chat_id),
            );
            check(int, "retry scheduled", 1, List.length(scheduled^));
          },
          [true, false],
        );
        AgentSend.max_eval_wait_attempts := saved_budget;
        AgentSend.eval_wait_attempts := saved_attempts;
        CodeWithStatics.StaticsDebounce.force_on_next := false;
      },
    ),
  ],
);
