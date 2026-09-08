open Alcotest;
open Web;

/* Tests for SlidePath: the "/"-separated hierarchical name behind
   Documentation-mode slide names and Tutorial-mode lesson titles. */

let path = testable(Fmt.of_to_string(SlidePath.show), (==));
let ints = list(int);
let strings = list(string);

let mk = (folders, leaf): SlidePath.t => {
  folders,
  leaf,
};

let of_string_tests =
  [
    ("empty", mk([], ""), ""),
    ("top level", mk([], "Holes"), "Holes"),
    ("one folder", mk(["Basics"], "Holes"), "Basics / Holes"),
    ("nested", mk(["a", "b"], "c"), "a / b / c"),
    /* Segments are trimmed, so spacing around the separator is irrelevant. */
    ("no spaces", mk(["Basics"], "Holes"), "Basics/Holes"),
    ("extra spaces", mk(["Basics"], "Holes"), "Basics  /  Holes"),
    ("trailing separator", mk(["Basics"], ""), "Basics /"),
  ]
  |> List.map(((name, expected, input)) =>
       test_case("of_string: " ++ name, `Quick, () =>
         check(path, input, expected, SlidePath.of_string(input))
       )
     );

let accessor_tests = [
  test_case(
    "segments and depth",
    `Quick,
    () => {
      let p = SlidePath.of_string("a / b / c");
      check(strings, "segments", ["a", "b", "c"], SlidePath.segments(p));
      check(int, "depth", 3, SlidePath.depth(p));
      check(string, "folder", "a / b", SlidePath.folder(p));
    },
  ),
  test_case("top-level path has no folder", `Quick, () =>
    check(
      string,
      "folder",
      "",
      SlidePath.folder(SlidePath.of_string("Holes")),
    )
  ),
  test_case(
    "to_string round-trips a canonical name",
    `Quick,
    () => {
      let name = "Tuple Structural Operations / Labeled Tuple Omission";
      check(
        string,
        "round trip",
        name,
        SlidePath.to_string(SlidePath.of_string(name)),
      );
    },
  ),
  test_case(
    "same_folder compares folders only",
    `Quick,
    () => {
      let a = SlidePath.of_string("Basics / Holes");
      let b = SlidePath.of_string("Basics / Functions");
      let c = SlidePath.of_string("Tables / Tables");
      check(bool, "siblings", true, SlidePath.same_folder(a, b));
      check(bool, "different folders", false, SlidePath.same_folder(a, c));
    },
  ),
];

/* Two folders of two, then a folder of one. */
let nav_paths =
  List.map(
    SlidePath.of_string,
    [
      "Basics / Holes",
      "Basics / Functions",
      "Tables / Tables",
      "Tables / Column Projection",
      "Rich Probes / Rich Probes",
    ],
  );

let nav_tests = [
  test_case(
    "folder_indices groups by folder",
    `Quick,
    () => {
      check(
        ints,
        "Basics",
        [0, 1],
        SlidePath.folder_indices(~current=0, nav_paths),
      );
      check(
        ints,
        "Tables",
        [2, 3],
        SlidePath.folder_indices(~current=3, nav_paths),
      );
      check(
        ints,
        "singleton",
        [4],
        SlidePath.folder_indices(~current=4, nav_paths),
      );
    },
  ),
  test_case(
    "folder_indices groups a non-contiguous folder",
    `Quick,
    () => {
      let paths = List.map(SlidePath.of_string, ["A / x", "B / y", "A / z"]);
      check(ints, "A", [0, 2], SlidePath.folder_indices(~current=0, paths));
    },
  ),
  test_case("folder_indices of an out-of-range index is empty", `Quick, () =>
    check(
      ints,
      "empty",
      [],
      SlidePath.folder_indices(~current=99, nav_paths),
    )
  ),
  test_case(
    "folder_position reports position and size",
    `Quick,
    () => {
      let pos_pair =
        testable(
          Fmt.of_to_string(((a, b)) => Printf.sprintf("(%d, %d)", a, b)),
          (==),
        );
      check(
        pos_pair,
        "first of two",
        (0, 2),
        SlidePath.folder_position(~current=0, nav_paths),
      );
      check(
        pos_pair,
        "last of two",
        (1, 2),
        SlidePath.folder_position(~current=3, nav_paths),
      );
      check(
        pos_pair,
        "only one",
        (0, 1),
        SlidePath.folder_position(~current=4, nav_paths),
      );
    },
  ),
  test_case(
    "step_in_folder walks within a folder",
    `Quick,
    () => {
      check(
        int,
        "forward",
        1,
        SlidePath.step_in_folder(~current=0, ~by=1, nav_paths),
      );
      check(
        int,
        "backward",
        0,
        SlidePath.step_in_folder(~current=1, ~by=-1, nav_paths),
      );
    },
  ),
  test_case(
    "step_in_folder clamps at both folder edges",
    `Quick,
    () => {
      /* Index 1 is the last of Basics and index 2 the first of Tables, so an
         unclamped step would cross the boundary. */
      check(
        int,
        "past the end",
        1,
        SlidePath.step_in_folder(~current=1, ~by=1, nav_paths),
      );
      check(
        int,
        "before the start",
        2,
        SlidePath.step_in_folder(~current=2, ~by=-1, nav_paths),
      );
      check(
        int,
        "singleton forward",
        4,
        SlidePath.step_in_folder(~current=4, ~by=1, nav_paths),
      );
      check(
        int,
        "singleton backward",
        4,
        SlidePath.step_in_folder(~current=4, ~by=-1, nav_paths),
      );
    },
  ),
  test_case(
    "step_in_folder skips a non-contiguous gap",
    `Quick,
    () => {
      let paths = List.map(SlidePath.of_string, ["A / x", "B / y", "A / z"]);
      check(
        int,
        "0 -> 2",
        2,
        SlidePath.step_in_folder(~current=0, ~by=1, paths),
      );
    },
  ),
];

let crumb_options = (paths, current) =>
  SlidePath.breadcrumb(~current, paths)
  |> List.map((c: SlidePath.crumb) =>
       (c.selected, List.map(snd, c.options))
     );

let crumbs =
  testable(
    Fmt.of_to_string(cs =>
      cs
      |> List.map(((sel, opts)) =>
           sel ++ ":[" ++ String.concat(",", opts) ++ "]"
         )
      |> String.concat(" / ")
    ),
    (==),
  );

let breadcrumb_tests = [
  test_case("one crumb per segment, siblings as options", `Quick, () =>
    check(
      crumbs,
      "Tables / Tables",
      [
        ("Tables", ["Basics", "Tables", "Rich Probes"]),
        ("Tables", ["Tables", "Column Projection"]),
      ],
      crumb_options(nav_paths, 2),
    )
  ),
  test_case(
    "deeper paths yield a deeper breadcrumb",
    `Quick,
    () => {
      let paths =
        List.map(
          SlidePath.of_string,
          [
            "B2T2 / API / Constructors / vcat",
            "B2T2 / API / Constructors / hcat",
            "B2T2 / Datasheet",
          ],
        );
      check(
        crumbs,
        "depth 4",
        [
          ("B2T2", ["B2T2"]),
          ("API", ["API", "Datasheet"]),
          ("Constructors", ["Constructors"]),
          ("vcat", ["vcat", "hcat"]),
        ],
        crumb_options(paths, 0),
      );
    },
  ),
  test_case(
    "folder option jumps to the first path in that folder",
    `Quick,
    () => {
      let by_name = SlidePath.breadcrumb(~current=0, nav_paths) |> List.hd;
      check(
        ints,
        "folder indices",
        [0, 2, 4],
        List.map(fst, by_name.options),
      );
    },
  ),
  test_case(
    "a proper-prefix name is skipped, not fatal",
    `Quick,
    () => {
      /* "Tables" alone has no segment at depth 1; before SlidePath this raised
         Failure("nth") and took down the whole top bar. */
      let paths =
        List.map(SlidePath.of_string, ["Tables", "Tables / Tables"]);
      check(
        crumbs,
        "deep crumb omits the short path",
        [("Tables", ["Tables"]), ("Tables", ["Tables"])],
        crumb_options(paths, 1),
      );
    },
  ),
  test_case("out-of-range current yields no crumbs", `Quick, () =>
    check(crumbs, "empty", [], crumb_options(nav_paths, 99))
  ),
];

let tests = [
  ("SlidePath.of_string", of_string_tests),
  ("SlidePath accessors", accessor_tests),
  ("SlidePath navigation", nav_tests),
  ("SlidePath.breadcrumb", breadcrumb_tests),
];
