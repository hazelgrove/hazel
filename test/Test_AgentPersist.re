open Web;
open Alcotest;

/* The persisted agent must not carry rebuildable bulk: the tool registry
   and the composed system prompt (~47KB, present in THREE places on a
   fresh agent) are swapped for a sentinel at persist and restamped from
   code at unpersist. */

let sexp_len = (p: Agent.Persistent.t): int =>
  Agent.Persistent.sexp_of_t(p) |> Sexplib.Sexp.to_string |> String.length;

/* A legacy conversation, including segment snapshots and a diff. */
let legacy_tool_result = () => {
  open Haz3lcore;
  let segment =
    Test_AgentTools.mk_zipper("let a = 1 in a") |> Zipper.unselect_and_zip;
  let tr = {
    ...
      AgentToolResult.mk_skipped({
        id: "legacy",
        name: "update_definition",
        args: `Null,
      }),
    success: true,
    skipped: false,
  };
  let msg = Message.Utils.mk_tool_result_message(tr);
  let agent = Agent.Utils.init();
  let agent =
    AgentUtils.append_message(~chat_id=agent.chat_system.current, msg, agent);
  let seg = Segment.sexp_of_t(segment);
  let rec old = sx => {
    Sexplib.Sexp.(
      switch (sx) {
      | List([Atom("before_text"), _]) =>
        List([Atom("before_segment"), List([seg])])
      | List([Atom("after_text"), _]) =>
        List([Atom("after_segment"), List([seg])])
      | List([Atom("diff"), _]) =>
        List([
          Atom("diff"),
          List([
            List([
              List([Atom("old_segment"), seg]),
              List([Atom("new_segment"), List([seg])]),
            ]),
          ]),
        ])
      | List(xs) =>
        List(
          List.filter_map(
            ~f=
              x =>
                switch (x) {
                | List([Atom("content_is_payload"), _]) => None
                | _ => Some(old(x))
                },
            xs,
          ),
        )
      | _ => sx
      }
    );
  };
  Agent.Persistent.persist(agent)
  |> Agent.Persistent.sexp_of_t
  |> old
  |> Agent.Persistent.t_of_sexp
  |> Agent.Persistent.unpersist;
};

let tests = (
  "AgentPersist",
  [
    test_case(
      "legacy conversation restores textual snapshots",
      `Quick,
      () => {
        open Haz3lcore;
        let agent = legacy_tool_result();
        let results =
          ChatSystem.Utils.find_chat(
            agent.chat_system.current,
            agent.chat_system,
          )
          |> Chat.Utils.linearize
          |> List.filter_map(~f=(m: Message.Model.t) =>
               switch (m.role) {
               | ToolResult(tr) => Some(tr)
               | _ => None
               }
             );
        check(int, "tool history preserved", 1, List.length(results));
        let tr = List.hd_exn(results);
        check(
          bool,
          "missing payload flag defaults false",
          false,
          tr.content_is_payload,
        );
        let render = t =>
          AgentToolResult.segment_of_text(t)
          |> Zipper.unzip
          |> Test_AgentTools.render_zipper;
        List.iter(
          ~f=
            t =>
              Test_AgentTools.check_rendered(
                "snapshot",
                "let a = 1 in a",
                render(Option.value_exn(t)),
              ),
          [tr.before_text, tr.after_text],
        );
        let diff = Option.value_exn(tr.diff);
        Test_AgentTools.check_rendered(
          "diff",
          "let a = 1 in a",
          render(diff.old_text),
        );
        check(
          bool,
          "new persistence uses text",
          true,
          Util.StringUtil.plain_search(
            "before_segment",
            Agent.Persistent.persist(agent)
            |> Agent.Persistent.sexp_of_t
            |> Sexplib.Sexp.to_string,
            0,
          )
          < 0,
        );
      },
    ),
    test_case(
      "tool snapshots round trip formatting and holes",
      `Quick,
      () => {
        open Haz3lcore;
        let code = "let a = 1 in\n\nlet b = ? in\n  b\n";
        let z = Test_AgentTools.mk_zipper(code);
        let text = PersistentZipper.to_string(z) ++ "\n";
        let restored = AgentToolResult.segment_of_text(text) |> Zipper.unzip;
        check(
          string,
          "lossless snapshot text",
          text,
          PersistentZipper.to_string(restored) ++ "\n",
        );
      },
    ),
    test_case(
      "text diffs preserve expression fragments and module members",
      `Quick,
      () => {
        open Haz3lcore;
        let render = s =>
          s
          |> Indentation.shallow_complete_segment
          |> CompositionView.Public.print_segment;
        List.iter(
          ~f=
            ((root, text)) => {
              let old = Option.value_exn(FastParse.of_text(~root, text));
              let restored = AgentToolResult.segment_of_diff_text(text);
              check(string, "diff preview", render(old), render(restored));
            },
          [
            (Sort.Exp, "let x = 1 in"),
            (Sort.Exp, "let x = 1 in\n"),
            (Sort.Mod, "let x = 1"),
            (Sort.Mod, "type t = Int"),
          ],
        );
      },
    ),
    test_case(
      "fresh agent persists small",
      `Quick,
      () => {
        let n = sexp_len(Agent.Persistent.persist(Agent.Utils.init()));
        check(
          bool,
          "under 10KB (was ~152KB with embedded prompt copies): "
          ++ string_of_int(n),
          true,
          n < 10_000,
        );
      },
    ),
    test_case(
      "unpersist restamps prompt and tools",
      `Quick,
      () => {
        let round =
          Agent.Persistent.unpersist(
            Agent.Persistent.persist(Agent.Utils.init()),
          );
        let cur =
          Haz3lcore.CompositionPrompt.self |> String.concat(~sep="\n");
        check(
          bool,
          "system_prompt restored",
          true,
          String.equal(round.prompting.system_prompt, cur),
        );
        check(
          bool,
          "tool registry restored",
          true,
          Poly.equal(
            round.prompting.tools,
            Haz3lcore.CompositionUtils.Public.tools,
          ),
        );
        /* every chat's root prompt message restored with its api copy */
        let prompts_ok =
          Haz3lcore.Id.Map.for_all(
            (_, chat: Chat.Model.t) =>
              Haz3lcore.Id.Map.for_all(
                (_, msg: Message.Model.t) =>
                  switch (msg.role) {
                  | System(Prompt) =>
                    String.equal(msg.content, String.strip(cur))
                    && Option.is_some(msg.api_message)
                  | _ => true
                  },
                chat.message_map,
              ),
            round.chat_system.chat_map,
          );
        check(bool, "chat prompt messages restored", true, prompts_ok);
      },
    ),
  ],
);
