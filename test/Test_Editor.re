open Alcotest;
open Haz3lcore;
let typ = testable(Fmt.using(Editor.show, Fmt.string), Editor.equal);

let test_initial_editor = () => {
  let zipper = Zipper.init();
  let ed = Editor.init(~settings=CoreSettings.on, zipper);

  let expected: Editor.t = {
    state: {
      zipper,
      meta: Editor.Meta.init(zipper, ~settings=CoreSettings.on),
    },
    history: ([], []),
    read_only: false,
  };
  check(typ, "Initial editor", expected, ed);
};

// test Perform.go with Insert("4+5")
let test_insert = () => {
  let zipper = Zipper.init();
  let ed = Editor.init(~settings=CoreSettings.on, zipper);
  let a = Action.Insert("4+5");
  let ed = Perform.go(~settings=CoreSettings.on, a, ed);
  let m: Mold.t = {
    in_: Any,
    out: Any,
    nibs: (Nib.{shape: Convex, sort: 0}, Nib.{shape: Convex, sort: 0}),
  };
  let expected: Editor.t = {
    state: {
      zipper:
        Zipper.unzip([
          Tile({
            label: ["4+5"],
            mold: {
              in_: Any,
              out: Any,
              nibs: (
                Nib.{shape: Convex, sort: 0},
                Nib.{shape: Convex, sort: 0},
              ),
            },
          }),
        ]),
      meta: Editor.Meta.init(zipper, ~settings=CoreSettings.on),
    },
    history: ([(a, ed.state)], []),
    read_only: false,
  };
  check(typ, "Insert 4+5", expected, ed);
};

let tests = [test_case("Initial editor", `Quick, test_initial_editor)];
