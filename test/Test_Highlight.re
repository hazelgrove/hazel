open Alcotest;
open Haz3lcore;

/* One string per drawn row: "row origin_col-last_col left/right" */
let rows = (seg: Segment.t): list(string) => {
  let measured =
    Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty);
  let tip: option(Nib.Shape.t) => string =
    fun
    | None => "none"
    | Some(Convex) => "convex"
    | Some(Concave(_)) => "concave";
  Web.Highlight.rows_of_segment(
    ~measured,
    ~shape_map=ProjectorCore.Shape.Map.empty,
    ~shape_init=Some(Convex),
    seg,
  )
  |> List.map(((m: Measured.measurement, (l, r))) =>
       Printf.sprintf(
         "%d %d-%d %s/%s",
         m.origin.row,
         m.origin.col,
         m.last.col,
         tip(l),
         tip(r),
       )
     );
};

let parse = (s: string): Segment.t =>
  switch (Parser.to_segment(s, ~root=Exp)) {
  | Some(seg) => seg
  | None => fail("Failed to parse: " ++ s)
  };

let tests = (
  "Highlight",
  [
    test_case("Indented rows start at their first token", `Quick, () =>
      check(
        list(string),
        "rows",
        [
          "0 0-32 convex/concave",
          "1 2-19 convex/concave",
          "2 4-10 convex/convex",
          "3 2-3 concave/convex",
          "4 0-1 concave/convex",
        ],
        rows(
          parse(
            "map([[1, 2], [3, 4]], fun row ->
  map(row, fun n ->
    n * 10
  )
)",
          ),
        ),
      )
    ),
    test_case("Blank rows span the rows around them", `Quick, () =>
      check(
        list(string),
        "rows",
        [
          "0 0-17 convex/concave",
          "1 2-14 convex/concave",
          "2 2-14 none/none",
          "3 2-14 convex/concave",
          "4 2-3 none/none",
          "5 2-3 convex/convex",
          "6 0-1 concave/convex",
        ],
        rows(
          parse(
            "map([1], fun n ->\n  let m = n in\n  \n  let k = m in\n\n  k\n)",
          ),
        ),
      )
    ),
    test_case(
      "Whitespace-only first and last rows keep their extent", `Quick, () =>
      check(
        list(string),
        "rows",
        ["0 0-2 concave/none", "1 2-3 convex/convex", "2 0-2 none/convex"],
        rows(parse("  \n  1\n  ")),
      )
    ),
    test_case("Trailing whitespace gives a straight right edge", `Quick, () =>
      check(
        list(string),
        "rows",
        ["0 0-6 convex/none", "1 2-3 convex/convex"],
        rows(parse("1 +   \n  2")),
      )
    ),
    test_case("Row after a leading linebreak is trimmed", `Quick, () =>
      check(
        list(string),
        "rows",
        ["1 2-7 convex/convex"],
        rows(parse("\n  1 + 2")),
      )
    ),
  ],
);
