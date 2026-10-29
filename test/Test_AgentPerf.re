open Haz3lcore;
open Util;
open Web;

/* wall time of a reply's structural tool on Mega 4k, as the assistant
   runs it (Agent.Update), the editor's statics in hand. Runs only with
   AGENT_PERF set: `AGENT_PERF=1 bash test/run_node.sh test AgentPerf`
   (AGENT_PERF=whole: the editor's statics whole-program), on a release
   build for the numbers that matter */

/* [warm]: the editor's statics as the editor makes them, per item, so
   their caches are warm; else whole-program statics */
/* AGENT_PERF_ONLY=a,b: just those tools */
let wanted = (name: string): bool =>
  switch (Sys.getenv_opt("AGENT_PERF_ONLY")) {
  | Some(only) when only != "" =>
    List.mem(name, String.split_on_char(',', only))
  | _ => true
  };

let reply_ms =
    (~root=Sort.Exp, ~warm, ~file, name: string, args: Yojson.Safe.t) =>
  if (wanted(name)) {
    let settings = Settings.Model.init;
    let z = Zipper.unzip(CorpusUtil.mega_seg(~root, file));
    let t_st = PerfTimer.now();
    let statics =
      warm
        ? CachedStatics.init_compositional(
            ~settings=settings.core,
            ~stitch=x => x,
            ~root,
            z,
          )
        : CachedStatics.init(
            ~settings=settings.core,
            ~is_dynamic_term=false,
            ~stitch=x => x,
            ~root,
            z,
          );
    let cell_editor: CellEditor.Model.t = {
      editor: CodeWithStatics.Model.mk(~statics, Editor.Model.mk(z, ~root)),
      result: EvalResult.Model.init,
    };
    Printf.printf(
      "AgentPerf %s (%s): editor statics %.0f ms\n%!",
      file,
      warm ? "per item" : "whole",
      PerfTimer.now() -. t_st,
    );
    let agent = Agent.Utils.init();
    let reply: OpenRouter.Reply.Model.t = {
      content: "",
      tool_calls: [
        {
          id: "perf",
          name,
          args,
        },
      ],
      usage: None,
      reasoning: None,
    };
    PerfTimer.reset();
    let t0 = PerfTimer.now();
    let (agent', _) =
      Agent.Update.update(
        Agent.Update.Action.HandleLLMResponse(
          reply,
          agent.chat_system.current,
          agent.main_llm_seq,
          None,
        ),
        agent,
        cell_editor,
        settings,
        _ =>
        ()
      );
    let ms = PerfTimer.now() -. t0;
    let phases = PerfTimer.take_summary();
    let chat =
      ChatSystem.Utils.find_chat(
        agent'.chat_system.current,
        agent'.chat_system,
      );
    let ok =
      List.exists(
        (m: Message.Model.t) =>
          switch (m.role) {
          | ToolResult(tr) => tr.success
          | _ => false
          },
        Chat.Utils.linearize(chat),
      );
    Printf.printf(
      "AgentPerf %s %s: %.0f ms%s\n  %s\n%!",
      file,
      name,
      ms,
      ok ? "" : " (FAILED)",
      phases,
    );
  };

let s = (k, v) => (k, `String(v));

let run = () =>
  switch (Sys.getenv_opt("AGENT_PERF")) {
  | None
  | Some("") => ()
  | Some(mode) =>
    let warm = mode != "whole";
    reply_ms(
      ~warm,
      ~file="mega-4k.hz",
      "replace",
      `Assoc([
        s("target", "Text/int_to_text:definition"),
        s(
          "code",
          "fun n -> if n < 0 then \"-\" ++ go_render(0 - n) else go_render(n + 0)",
        ),
      ]),
    );
    reply_ms(
      ~warm,
      ~file="mega-4k.hz",
      "insert",
      `Assoc([s("after", "Text"), s("code", "let extra = 1")]),
    );
    reply_ms(
      ~warm,
      ~file="mega-4k.hz",
      "rename",
      `Assoc([s("target", "Text/int_to_text"), s("to", "int_text")]),
    );
    let paths = `Assoc([("paths", `List([`String("Text/int_to_text")]))]);
    reply_ms(~warm, ~file="mega-4k.hz", "place_probe", paths);
    reply_ms(~warm, ~file="mega-4k.hz", "place_statics", paths);
  };

let tests = [
  ("AgentPerf", [Alcotest.test_case("Mega 4k tool calls", `Slow, run)]),
];
