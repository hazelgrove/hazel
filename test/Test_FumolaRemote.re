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
  ],
);
