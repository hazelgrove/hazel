open Web;
open Alcotest;

/* The persisted agent must not carry rebuildable bulk: the tool registry
   and the composed system prompt (~47KB, present in THREE places on a
   fresh agent) are swapped for a sentinel at persist and restamped from
   code at unpersist. */

let sexp_len = (p: Agent.Persistent.t): int =>
  Agent.Persistent.sexp_of_t(p) |> Sexplib.Sexp.to_string |> String.length;

let tests = (
  "AgentPersist",
  [
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
        let cur = Haz3lcore.CompositionPrompt.self |> String.concat("\n");
        check(
          bool,
          "system_prompt restored",
          true,
          round.prompting.system_prompt == cur,
        );
        check(
          bool,
          "tool registry restored",
          true,
          round.prompting.tools == Haz3lcore.CompositionUtils.Public.tools,
        );
        /* every chat's root prompt message restored with its api copy */
        let prompts_ok =
          Haz3lcore.Id.Map.for_all(
            (_, chat: Chat.Model.t) =>
              Haz3lcore.Id.Map.for_all(
                (_, msg: Message.Model.t) =>
                  switch (msg.role) {
                  | System(Prompt) =>
                    msg.content == String.trim(cur) && msg.api_message != None
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
