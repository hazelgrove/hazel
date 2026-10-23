open Alcotest;

/* the Cmd-hover underline follows Cmd, though the page's key listener
   reads the model of the last render */

let key = (k: Util.Key.key, ~meta: Util.Key.held): Util.Key.t => {
  key: k,
  code: "",
  sys: Mac,
  shift: Up,
  meta,
  ctrl: Up,
  alt: Up,
  target_tag: None,
};

let press = key(D("Meta"), ~meta=Down);
let release = key(U("Meta"), ~meta=Up);

/* [keys] arriving before the next render: the model's Cmd state after
   them, from [model], the listener reading [rendered] */
let within_frame = (~rendered: bool, model: bool, keys: list(Util.Key.t)) =>
  List.fold_left(
    (model, k) =>
      switch (Web.Page.View.meta_change(~rendered, k)) {
      | Some(down) => down
      | None => model
      },
    model,
    keys,
  );

let tests = (
  "PageKeys",
  [
    test_case("a Cmd tap within one frame leaves Cmd up", `Quick, () =>
      check(
        bool,
        "Cmd down",
        false,
        within_frame(~rendered=false, false, [press, release]),
      )
    ),
    test_case("a Cmd press shows the underline", `Quick, () =>
      check(
        bool,
        "Cmd down",
        true,
        within_frame(~rendered=false, false, [press]),
      )
    ),
    test_case("a key with Cmd held, unseen, sets it", `Quick, () =>
      check(
        bool,
        "Cmd down",
        true,
        within_frame(~rendered=false, false, [key(D("k"), ~meta=Down)]),
      )
    ),
    test_case("a key changing nothing gives nothing", `Quick, () =>
      check(
        bool,
        "no change",
        true,
        Web.Page.View.meta_change(~rendered=true, key(D("k"), ~meta=Down))
        == None,
      )
    ),
  ],
);
