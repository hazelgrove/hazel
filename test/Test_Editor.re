open Alcotest;
open Haz3lcore;
let typ = testable(Fmt.using(Editor.show, Fmt.string), Editor.equal);
let zipper_typ = testable(Fmt.using(Zipper.show, Fmt.string), Zipper.equal);

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

// TODO Test inserting in the editor
let test_insert = () => {
  let zipper = Zipper.init();
  let ed = Editor.init(~settings=CoreSettings.on, zipper);
  let a = Action.Paste("4+5");
  let ed: Action.Result.t(Editor.t) =
    Perform.go(~settings=CoreSettings.on, a, ed);
  let m: Mold.t = {
    in_: [],
    out: Any,
    nibs: (Nib.{shape: Convex, sort: Any}, Nib.{shape: Convex, sort: Any}),
  };
  let t: Tile.t = {
    label: ["4+5"],
    mold: m,
    shards: [0],
    children: [],
    id: Id.mk(),
  };
  let til: Segment.t = [Tile(t)];
  let history: Editor.History.t = ([], []);
  let expected: Editor.t = {
    state: {
      zipper: Zipper.unzip(til),
      meta: Editor.Meta.init(zipper, ~settings=CoreSettings.on),
    },
    history,
    read_only: false,
  };
  let actual = Result.get_ok(ed);

  check(zipper_typ, "Insert 4+5", expected.state.zipper, actual.state.zipper);
};

let tests = [
  test_case("Initial editor", `Quick, test_initial_editor),
  // test_case("Insert 4", `Quick, test_insert),
];
