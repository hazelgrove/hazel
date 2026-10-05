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
let render = (~canister=false, g, target) => {
  ignore(FumolaSidebar.render(~canister, ~globals=g, target));
  H.unsafe_convert_exn(FumolaSidebar.render(~canister, ~globals=g, target));
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

/* The instance list's two sources. The canister's: a stand-in
   `hazelBackendStats` (ic-backend.js, GET /stats) answering at once with
   [reply], or never when None. The page's: a stand-in runtime `fumola`
   with only the `instances` method, removed again by [without_page]. */
let install_stats = (reply: option(string)) => {
  FumolaRun.canister_stats_cache := None;
  FumolaRun.canister_stats_asking := false;
  Js.Unsafe.set(
    Js.Unsafe.global,
    "hazelBackendStats",
    Js.wrap_callback(on_text =>
      switch (reply) {
      | Some(text) =>
        ignore(
          Js.Unsafe.fun_call(
            on_text,
            [|Js.Unsafe.inject(Js.string(text))|],
          ),
        )
      | None => ()
      }
    ),
  );
};

let stats = (names: list(string)) =>
  Printf.sprintf(
    {|{"ok":true,"heap_bytes":3460000,
       "store":{"keys":38,"mode":"graphical",
                "stats":{"pointers":38,"versions":38,"edges":38,"history_events":76}},
       "instances":{%s}}|},
    String.concat(
      ",",
      List.map(
        n =>
          Printf.sprintf(
            {|"%s":{"mode":"graphical","stats":{"pointers":1,"versions":1,"edges":2,"history_events":3}}|},
            n,
          ),
        names,
      ),
    ),
  );

let with_page = (names: list(string), f) => {
  let reply =
    Printf.sprintf(
      {|{"ok":true,"heap_bytes":9400000,"instances":[%s]}|},
      String.concat(
        ",",
        List.map(
          n => Printf.sprintf({|{"name":"%s","stats":{"pointers":1}}|}, n),
          names,
        ),
      ),
    );
  Js.Unsafe.set(
    Js.Unsafe.global,
    "fumola",
    Js.Unsafe.obj([|
      (
        "instances",
        Js.Unsafe.inject(Js.wrap_callback(() => Js.string(reply))),
      ),
    |]),
  );
  Fun.protect(
    ~finally=() => Js.Unsafe.set(Js.Unsafe.global, "fumola", Js.undefined),
    f,
  );
};

let list_links = tree => texts(tree, ".fumola-list .fumola-list-link");
let pin_of = (name, place) =>
  Globals.Action.show(FumolaPin(Some((name, place))));

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
    test_case(
      "each name in the list pins its instance, and its place with it",
      `Quick,
      () => {
        install_stats(Some(stats(["listRemote", "listOther"])));
        let (g, sent) = globals();
        with_page(
          ["listPage"],
          () => {
            let tree = render(~canister=true, g, AtCursor(Cursor.empty));
            check(
              list(string),
              "the page's, then the store, then the canister's",
              ["listPage", "hazelStore", "listRemote", "listOther"],
              list_links(tree),
            );
            List.iter(
              name => click(tree, ".fumola-list .fumola-list-link", name),
              ["listPage", "hazelStore", "listRemote"],
            );
          },
        );
        check(
          list(string),
          "each click pins one, the place with it",
          [
            pin_of("listPage", Page),
            pin_of("hazelStore", Canister),
            pin_of("listRemote", Canister),
          ],
          sent_shows(sent),
        );
      },
    ),
    test_case(
      "one name in the page and on the canister pins two instances",
      `Quick,
      () => {
        install_stats(Some(stats(["twin"])));
        let (g, sent) = globals();
        with_page(
          ["twin"],
          () => {
            let tree = render(~canister=true, g, AtCursor(Cursor.empty));
            let twins =
              List.filter(
                n => H.inner_text(n) == "twin",
                H.select(tree, ~selector=".fumola-list .fumola-list-link"),
              );
            check(int, "listed twice", 2, List.length(twins));
            List.iter(H.User_actions.click_on, twins);
          },
        );
        check(
          list(string),
          "the page's, then the canister's",
          [pin_of("twin", Page), pin_of("twin", Canister)],
          sent_shows(sent),
        );
      },
    ),
    test_case(
      "the pinned instance is marked in the list, and only on its side",
      `Quick,
      () => {
        install_canister();
        install_stats(Some(stats(["shownTwin"])));
        let (g, _) =
          globals(~pinned=Some(("shownTwin", FumolaRun.Canister)), ());
        with_page(
          ["shownTwin"],
          () => {
            let tree = render(~canister=true, g, AtCursor(Cursor.empty));
            check(
              int,
              "one row marked",
              1,
              List.length(H.select(tree, ~selector=".fumola-list-shown")),
            );
            check(
              list(string),
              "and it is the canister's",
              ["shownTwin"],
              texts(tree, ".fumola-list-shown .fumola-list-link"),
            );
            let twin_rows =
              H.select(tree, ~selector=".fumola-list tr")
              |> List.filter(r => H.inner_text(r) |> contains("shownTwin"));
            check(int, "both twins are listed", 2, List.length(twin_rows));
            check(
              list(bool),
              "the second, the canister's, is the marked one",
              [false, true],
              List.map(H.has_class(~cls="fumola-list-shown"), twin_rows),
            );
          },
        );
      },
    ),
    test_case(
      "the canister's table says it is asking until the canister answers",
      `Quick,
      () => {
        install_stats(None);
        let (g, _) = globals();
        let tree = render(~canister=true, g, AtCursor(Cursor.empty));
        check(
          bool,
          "asking",
          true,
          List.mem("asking the canister...", texts(tree, ".fumola-blurb")),
        );
        check(list(string), "and no links", [], list_links(tree));
      },
    ),
    test_case(
      "with no canister, only the page's instances are listed",
      `Quick,
      () => {
        install_stats(Some(stats(["unseen"])));
        let (g, _) = globals();
        with_page(
          ["alone"],
          () => {
            let tree = render(~canister=false, g, AtCursor(Cursor.empty));
            check(
              list(string),
              "the page's only",
              ["alone"],
              list_links(tree),
            );
            check(
              bool,
              "no canister heading",
              false,
              List.exists(
                contains("canister"),
                texts(tree, ".fumola-list-heading"),
              ),
            );
          },
        );
      },
    ),
  ],
);
