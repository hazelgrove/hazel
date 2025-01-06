open Alcotest;
open Haz3lcore;
let settings = ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on);
let segment =
  testable(Fmt.using(Segment.show, Fmt.string), (x, y) => x == y);
let exp_to_segment = ExpToSegment.exp_to_segment(~settings);

let tests = (
  "ExpToSegment",
  [
    test_case("Integer", `Quick, () => {
      check(
        segment,
        "split",
        [
          Tile({
            id: Id.invalid,
            label: ["1"],
            mold: Mold.mk_op(Exp, []),
            shards: [0],
            children: [],
          }),
        ],
        exp_to_segment({term: Int(1), ids: [Id.invalid], copied: false}),
      )
    }),
  ],
);
