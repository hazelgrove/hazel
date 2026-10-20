open Alcotest;
open Language;

/* An instance that lives on the canister (FumolaRun.remote_reply). The
   canister itself is not here: a stand-in `hazelFumolaRemote` answers as
   ic-backend.js does, by calling back with the reply text, so what is
   tested is the page's half -- asking once, keeping the reply, and saying
   so while there is none. */

module Js = Js_of_ocaml.Js;

/* The stand-in: records each program asked for, and answers [reply] now,
   or never when [reply] is None (a canister that has not answered yet). */
let install = (reply: option(string)): ref(list(string)) => {
  let asked = ref([]);
  Js.Unsafe.set(
    Js.Unsafe.global,
    "hazelFumolaRemote",
    Js.wrap_callback((_instance, _mode, program, on_text) => {
      asked := [Js.to_string(program), ...asked^];
      switch (reply) {
      | Some(text) =>
        ignore(
          Js.Unsafe.fun_call(
            on_text,
            [|Js.Unsafe.inject(Js.string(text))|],
          ),
        )
      | None => ()
      };
    }),
  );
  asked;
};

let uninstall = () =>
  Js.Unsafe.set(Js.Unsafe.global, "hazelFumolaRemote", Js.undefined);

/* The other stand-in, for `hazelFumolaRemoteQuery` (ic-backend.js): the
   side calls a reset and a re-init make. Records each (instance, op, body)
   asked, oldest first, and answers at once with [answer(op)]. */
let install_query =
    (answer: string => string): ref(list((string, string, string))) => {
  let asked = ref([]);
  Js.Unsafe.set(
    Js.Unsafe.global,
    "hazelFumolaRemoteQuery",
    Js.wrap_callback((instance, op, body, on_text) => {
      let op = Js.to_string(op);
      asked := asked^ @ [(Js.to_string(instance), op, Js.to_string(body))];
      ignore(
        Js.Unsafe.fun_call(
          on_text,
          [|Js.Unsafe.inject(Js.string(answer(op)))|],
        ),
      );
    }),
  );
  asked;
};

/* The page announces with window.dispatchEvent, and node has no window:
   this one records the events' names. It stays installed for the run, as
   a reset's timers may fire after its test has finished. */
let dispatched: ref(list(string)) = ref([]);
let install_window = () => {
  dispatched := [];
  Js.Unsafe.set(
    Js.Unsafe.global,
    "window",
    Js.Unsafe.obj([|
      (
        "dispatchEvent",
        Js.Unsafe.inject(
          Js.wrap_callback(event =>
            dispatched :=
              dispatched^ @ [Js.to_string(Js.Unsafe.get(event, "type"))]
          ),
        ),
      ),
    |]),
  );
};

let status_words =
  fun
  | None => "none"
  | Some(FumolaRun.Resetting) => "resetting"
  | Some(Rerunning) => "rerunning"
  | Some(Asked) => "asked"
  | Some(Ran) => "ran"
  | Some(Not_run) => "not run"
  | Some(Failed(why)) => "failed: " ++ why
  | Some(Reinited(words)) => words;
let status = instance => status_words(FumolaRun.reset_status(instance));

let ok = (json: option(Yojson.Safe.t)) =>
  switch (json) {
  | Some(`Assoc(obj)) => List.assoc_opt("ok", obj) == Some(`Bool(true))
  | _ => false
  };

let tests = (
  "FumolaRemote",
  [
    test_case(
      "a reply is asked for once and then kept",
      `Quick,
      () => {
        let asked = install(Some({|{"ok":true,"tag":"Int","value":"40"}|}));
        let first =
          FumolaRun.remote_reply(~instance="r1", ~mode=None, ~at=1, "20 * 2");
        check(bool, "answered", true, ok(first));
        let again =
          FumolaRun.remote_reply(~instance="r1", ~mode=None, ~at=2, "20 * 2");
        check(bool, "kept", true, ok(again));
        check(
          int,
          "asked once, though the moment moved",
          1,
          List.length(asked^),
        );
        uninstall();
      },
    ),
    test_case(
      "no reply yet is None, and is not asked again",
      `Quick,
      () => {
        let asked = install(None);
        let first =
          FumolaRun.remote_reply(~instance="r2", ~mode=None, ~at=1, "1");
        let again =
          FumolaRun.remote_reply(~instance="r2", ~mode=None, ~at=2, "1");
        check(bool, "pending", true, first == None && again == None);
        check(int, "asked once", 1, List.length(asked^));
        uninstall();
      },
    ),
    test_case(
      "the program is sent at its moment",
      `Quick,
      () => {
        let asked = install(None);
        ignore(
          FumolaRun.remote_reply(~instance="r3", ~mode=None, ~at=7, "x"),
        );
        check(list(string), "sent", [FumolaRun.at_moment(7, "x")], asked^);
        uninstall();
      },
    ),
    test_case(
      "with no canister, the reply says so",
      `Quick,
      () => {
        uninstall();
        switch (
          FumolaRun.remote_reply(~instance="r4", ~mode=None, ~at=1, "1")
        ) {
        | Some(`Assoc(obj)) =>
          check(
            bool,
            "not ok",
            true,
            List.assoc_opt("ok", obj) == Some(`Bool(false)),
          )
        | _ => fail("expected an error reply")
        };
      },
    ),
    test_case(
      "where an instance lives is the last word on it",
      `Quick,
      () => {
        FumolaRun.set_remote("r5", true);
        check(bool, "remote", true, FumolaRun.is_remote("r5"));
        FumolaRun.set_remote("r5", false);
        check(bool, "back in the page", false, FumolaRun.is_remote("r5"));
      },
    ),
    test_case(
      "a reset empties, declares the mode, forgets, and the re-run asks again",
      `Quick,
      () => {
        install_window();
        let programs =
          install(Some({|{"ok":true,"tag":"Int","value":"3"}|}));
        ignore(
          FumolaRun.remote_reply(~instance="q1", ~mode=None, ~at=1, "1 + 2"),
        );
        let asked =
          install_query(
            fun
            | "reset" => {|{"reset":true}|}
            | _ => {|{"ok":true,"mode":"graphical"}|},
          );
        FumolaRun.reset_remote(~mode=Graphical, "q1");
        check(
          list(triple(string, string, string)),
          "reset, then the mode, in that order",
          [("q1", "reset", ""), ("q1", "ensure_mode", "graphical")],
          asked^,
        );
        check(string, "running again", "rerunning", status("q1"));
        check(
          bool,
          "the programs are told to run again",
          true,
          List.mem("fumola-remote-reply", dispatched^),
        );
        /* The re-run: the kept reply is gone, so the canister is asked. */
        ignore(
          FumolaRun.remote_reply(~instance="q1", ~mode=None, ~at=2, "1 + 2"),
        );
        check(int, "asked again", 2, List.length(programs^));
        check(string, "and it ran", "ran", status("q1"));
        uninstall();
      },
    ),
    test_case(
      "without a mode, a reset declares none",
      `Quick,
      () => {
        install_window();
        let asked = install_query(_ => {|{"reset":true}|});
        FumolaRun.reset_remote("q2");
        check(
          list(triple(string, string, string)),
          "reset only",
          [("q2", "reset", "")],
          asked^,
        );
      },
    ),
    test_case(
      "a refused reset stops there, says why, and keeps the reply",
      `Quick,
      () => {
        install_window();
        let programs =
          install(Some({|{"ok":true,"tag":"Int","value":"1"}|}));
        ignore(
          FumolaRun.remote_reply(~instance="q3", ~mode=None, ~at=1, "1"),
        );
        let asked =
          install_query(_ =>
            {|{"ok":false,"error":"the canister did not answer: 404 Not Found"}|}
          );
        FumolaRun.reset_remote(~mode=Simple, "q3");
        check(
          int,
          "no mode asked after the refusal",
          1,
          List.length(asked^),
        );
        check(
          string,
          "says why",
          "failed: the canister did not answer: 404 Not Found",
          status("q3"),
        );
        check(
          bool,
          "no re-run announced",
          false,
          List.mem("fumola-remote-reply", dispatched^),
        );
        ignore(
          FumolaRun.remote_reply(~instance="q3", ~mode=None, ~at=2, "1"),
        );
        check(
          int,
          "the kept reply still answers",
          1,
          List.length(programs^),
        );
        uninstall();
      },
    ),
    test_case(
      "a reply that is not JSON fails the reset",
      `Quick,
      () => {
        install_window();
        ignore(install_query(_ => "<html>503</html>"));
        FumolaRun.reset_remote("q4");
        check(
          string,
          "says so",
          "failed: the canister's reply was not JSON",
          status("q4"),
        );
      },
    ),
    test_case(
      "re-init asks the store, says what it dropped, and forgets its replies",
      `Quick,
      () => {
        install_window();
        let store = FumolaRun.store_instance;
        let programs =
          install(Some({|{"ok":true,"tag":"Int","value":"38"}|}));
        ignore(
          FumolaRun.remote_reply(~instance=store, ~mode=None, ~at=1, "s"),
        );
        let asked =
          install_query(_ =>
            {|{"ok":true,"reinit":true,"keys":38,
               "before":{"stats":{"pointers":38,"versions":120,"edges":389}},
               "after":{"stats":{"pointers":38,"versions":38,"edges":38}}}|}
          );
        FumolaRun.reinit_store();
        check(
          list(triple(string, string, string)),
          "one reinit, of the store",
          [(store, "reinit", "")],
          asked^,
        );
        check(
          string,
          "what the rebuild dropped",
          "re-inited: 120 versions to 38, 389 edges to 38",
          status(store),
        );
        check(
          bool,
          "the programs are told to run again",
          true,
          List.mem("fumola-remote-reply", dispatched^),
        );
        ignore(
          FumolaRun.remote_reply(~instance=store, ~mode=None, ~at=2, "s"),
        );
        check(int, "the store is asked afresh", 2, List.length(programs^));
        uninstall();
      },
    ),
    test_case(
      "a refused re-init says why and forgets nothing",
      `Quick,
      () => {
        install_window();
        let store = FumolaRun.store_instance;
        let programs =
          install(Some({|{"ok":true,"tag":"Int","value":"1"}|}));
        ignore(
          FumolaRun.remote_reply(~instance=store, ~mode=None, ~at=1, "t"),
        );
        ignore(
          install_query(_ =>
            {|{"ok":false,"error":"only 3 of 4 values read back; nothing was rebuilt"}|}
          ),
        );
        FumolaRun.reinit_store();
        check(
          string,
          "says why",
          "failed: only 3 of 4 values read back; nothing was rebuilt",
          status(store),
        );
        check(
          bool,
          "no re-run announced",
          false,
          List.mem("fumola-remote-reply", dispatched^),
        );
        ignore(
          FumolaRun.remote_reply(~instance=store, ~mode=None, ~at=2, "t"),
        );
        check(
          int,
          "the kept reply still answers",
          1,
          List.length(programs^),
        );
        uninstall();
      },
    ),
  ],
);
