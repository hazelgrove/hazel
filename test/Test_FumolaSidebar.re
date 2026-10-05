open Alcotest;
open Web;

/* The Fumola panel's reset strip and pin bar, rendered and read back as a
   tree (virtual_dom's test helpers), with clicks fired on it. The canister
   is a stand-in `hazelFumolaRemoteQuery` that answers at once: an empty
   history to every read and graphical to `mode`. What is checked is what
   the panel offers, what it says, and the action each control sends. */

module Js = Js_of_ocaml.Js;
module H = Virtual_dom_test_helpers.Node_helpers;
module FumolaRun = Language.FumolaRun;

let install_canister = () =>
  Js.Unsafe.set(
    Js.Unsafe.global,
    "hazelFumolaRemoteQuery",
    Js.wrap_callback((_instance, op, _body, on_text) => {
      let reply =
        switch (Js.to_string(op)) {
        | "mode" => {|{"ok":true,"mode":"graphical"}|}
        | _ => {|{"ok":true}|}
        };
      ignore(
        Js.Unsafe.fun_call(
          on_text,
          [|Js.Unsafe.inject(Js.string(reply))|],
        ),
      );
    }),
  );

/* Globals whose inject_global keeps what it is sent, oldest first. */
let globals = (~pinned=None, ()) => {
  let sent = ref([]);
  let init = Settings.Model.init;
  let settings = {
    ...init,
    sidebar: {
      ...init.sidebar,
      fumola_pinned: pinned,
    },
  };
  let g = {
    ...Globals.Model.init(~settings, ()),
    inject_global: action => {
      sent := sent^ @ [action];
      Virtual_dom.Vdom.Effect.Ignore;
    },
  };
  (g, sent);
};

/* Rendered twice: the first render is what asks the stand-in, which
   answers at once, so the second has the history and the mode. */
let render = (g, target) => {
  ignore(FumolaSidebar.render(~globals=g, target));
  H.unsafe_convert_exn(FumolaSidebar.render(~globals=g, target));
};

let texts = (tree, selector) =>
  List.map(H.inner_text, H.select(tree, ~selector));

let click = (tree, selector, label) =>
  switch (
    List.find_opt(n => H.inner_text(n) == label, H.select(tree, ~selector))
  ) {
  | Some(node) => H.User_actions.click_on(node)
  | None => fail("no " ++ selector ++ " reading " ++ label)
  };

let contains = (part: string, s: string): bool => {
  let n = String.length(part);
  let rec at = i =>
    i + n <= String.length(s) && (String.sub(s, i, n) == part || at(i + 1));
  at(0);
};

let sent_shows = (sent: ref(list(Globals.Action.t))) =>
  List.map(Globals.Action.show, sent^);

let with_status = (instance, status, f) => {
  Hashtbl.replace(FumolaRun.remote_resets, instance, status);
  Fun.protect(
    ~finally=() => Hashtbl.remove(FumolaRun.remote_resets, instance),
    f,
  );
};

let canister_instance = name => {
  install_canister();
  FumolaRun.set_remote(name, true);
  FumolaSidebar.Instance(name, None);
};

let tests = (
  "FumolaSidebar",
  [
    test_case(
      "a canister instance's strip offers G and S, and G sends its reset",
      `Quick,
      () => {
        let target = canister_instance("sb1");
        let (g, sent) = globals();
        let tree = render(g, target);
        check(
          list(string),
          "the buttons",
          ["G", "S"],
          texts(tree, ".fumola-reset"),
        );
        check(
          list(string),
          "the current mode is marked",
          ["G"],
          texts(tree, ".fumola-reset-current"),
        );
        check(
          list(string),
          "no status before a reset",
          [],
          texts(tree, ".fumola-reset-status"),
        );
        click(tree, ".fumola-reset", "G");
        check(
          list(string),
          "sent",
          [Globals.Action.show(FumolaReset("sb1", Canister, Graphical))],
          sent_shows(sent),
        );
      },
    ),
    test_case(
      "the strip says where a canister reset has got to",
      `Quick,
      () => {
        let target = canister_instance("sb2");
        let (g, _) = globals();
        let words = status =>
          with_status("sb2", status, () =>
            texts(render(g, target), ".fumola-reset-status")
          );
        check(
          list(string),
          "resetting",
          ["resetting..."],
          words(Resetting),
        );
        check(
          list(string),
          "running again",
          ["reset; running again...", "reset; running again..."],
          [words(Rerunning), words(Asked)] |> List.concat,
        );
        check(list(string), "ran", ["reset; ran again"], words(Ran));
        check(
          list(string),
          "nothing runs it",
          ["reset; nothing on this slide runs it"],
          words(Not_run),
        );
        with_status(
          "sb2",
          Failed("the canister did not answer: 404 Not Found"),
          () => {
            let tree = render(g, target);
            check(
              list(string),
              "failed, in words",
              ["reset failed: the canister did not answer: 404 Not Found"],
              texts(tree, ".fumola-reset-status"),
            );
            check(
              int,
              "and marked as a failure",
              1,
              List.length(H.select(tree, ~selector=".fumola-reset-failed")),
            );
          },
        );
      },
    ),
    test_case(
      "the store offers re-init, not G or S, and re-init sends its action",
      `Quick,
      () => {
        let target = canister_instance(FumolaRun.store_instance);
        let (g, sent) = globals();
        let tree = render(g, target);
        check(
          list(string),
          "one button",
          ["re-init"],
          texts(tree, ".fumola-reset"),
        );
        click(tree, ".fumola-reset", "re-init");
        check(
          list(string),
          "sent",
          [Globals.Action.show(FumolaReinitStore)],
          sent_shows(sent),
        );
      },
    ),
    test_case(
      "the store's strip says re-init where a reset would say reset",
      `Quick,
      () => {
        let store = FumolaRun.store_instance;
        let target = canister_instance(store);
        let (g, _) = globals();
        let words = status =>
          with_status(store, status, () =>
            texts(render(g, target), ".fumola-reset-status")
          );
        check(
          list(string),
          "re-initing",
          ["re-initing..."],
          words(Resetting),
        );
        check(
          list(string),
          "what it dropped",
          ["re-inited: 120 versions to 38"],
          words(Reinited("re-inited: 120 versions to 38")),
        );
        check(
          list(string),
          "failed",
          ["re-init failed: nothing was rebuilt"],
          words(Failed("nothing was rebuilt")),
        );
      },
    ),
    test_case(
      "a pinned canister instance has a bar: follow the cursor, or ask again",
      `Quick,
      () => {
        ignore(canister_instance("sb3"));
        let (g, sent) =
          globals(~pinned=Some(("sb3", FumolaRun.Canister)), ());
        let tree = render(g, AtCursor(Cursor.empty));
        check(
          list(string),
          "the bar's links",
          ["\xe2\x86\x90 follow the cursor", "ask the canister again"],
          texts(tree, ".fumola-pinned-bar .fumola-list-link"),
        );
        check(
          bool,
          "it shows the pinned instance",
          true,
          List.exists(
            contains("sb3"),
            texts(tree, ".fumola-section-header"),
          ),
        );
        click(
          tree,
          ".fumola-pinned-bar .fumola-list-link",
          "ask the canister again",
        );
        click(
          tree,
          ".fumola-pinned-bar .fumola-list-link",
          "\xe2\x86\x90 follow the cursor",
        );
        check(
          list(string),
          "sent, in order",
          [
            Globals.Action.show(FumolaRefresh("sb3")),
            Globals.Action.show(FumolaPin(None)),
          ],
          sent_shows(sent),
        );
      },
    ),
    test_case(
      "a pinned page instance has no canister to ask",
      `Quick,
      () => {
        let (g, _) = globals(~pinned=Some(("sb4", FumolaRun.Page)), ());
        let tree = render(g, AtCursor(Cursor.empty));
        check(
          list(string),
          "only the way back",
          ["\xe2\x86\x90 follow the cursor"],
          texts(tree, ".fumola-pinned-bar .fumola-list-link"),
        );
      },
    ),
    test_case(
      "with nothing pinned there is no bar",
      `Quick,
      () => {
        let (g, _) = globals();
        let tree = render(g, AtCursor(Cursor.empty));
        check(
          int,
          "no bar",
          0,
          List.length(H.select(tree, ~selector=".fumola-pinned-bar")),
        );
      },
    ),
  ],
);
