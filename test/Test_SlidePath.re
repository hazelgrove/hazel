open Alcotest;
open Web;

/* Tests for SlidePath: the "/"-separated hierarchical name behind
   Documentation-mode slide names and Tutorial-mode lesson titles. */

let path = testable(Fmt.of_to_string(SlidePath.show), SlidePath.equal);
let ints = list(int);
let strings = list(string);
let pos_pair =
  testable(
    Fmt.of_to_string(((a, b)) => Printf.sprintf("(%d, %d)", a, b)),
    (==),
  );

let of_string_tests =
  [
    ("an empty name", "", [], ""),
    ("a name with no folder", "Holes", [], "Holes"),
    ("one folder", "Basics / Holes", ["Basics"], "Holes"),
    ("nested folders", "a / b / c", ["a", "b"], "c"),
    /* Segments are trimmed, so spacing around a separator is irrelevant. */
    ("no spaces around the separator", "Basics/Holes", ["Basics"], "Holes"),
    ("extra spaces", "Basics  /  Holes", ["Basics"], "Holes"),
    ("a trailing separator", "Basics /", ["Basics"], ""),
  ]
  |> List.map(((name, input, folders, leaf)) =>
       test_case(name, `Quick, () =>
         check(
           path,
           input,
           SlidePath.mk(~folders, leaf),
           SlidePath.of_string(input),
         )
       )
     );

let mk_tests = [
  test_case("normalizes a part holding a separator", `Quick, ()
    /* Otherwise the path would not render back to its own name. */
    =>
      check(
        path,
        "folder split into two",
        SlidePath.of_string("a / b / c"),
        SlidePath.mk(~folders=["a / b"], "c"),
      )
    ),
  test_case("a leaf alone has no folders", `Quick, () =>
    check(strings, "folders", [], SlidePath.folders(SlidePath.mk("Holes")))
  ),
];

let to_string_tests = [
  test_case(
    "round-trips a canonical name",
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
  test_case("renders a normalized name from a spaceless one", `Quick, () =>
    check(
      string,
      "canonical spacing",
      "Basics / Holes",
      SlidePath.to_string(SlidePath.of_string("Basics/Holes")),
    )
  ),
];

let leaf_tests = [
  test_case("the last segment", `Quick, () =>
    check(
      string,
      "c",
      "c",
      SlidePath.leaf(SlidePath.of_string("a / b / c")),
    )
  ),
];

let folders_tests = [
  test_case("every segment but the last", `Quick, () =>
    check(
      strings,
      "a, b",
      ["a", "b"],
      SlidePath.folders(SlidePath.of_string("a / b / c")),
    )
  ),
];

let segments_tests = [
  test_case("the folders and the leaf", `Quick, () =>
    check(
      strings,
      "a, b, c",
      ["a", "b", "c"],
      SlidePath.segments(SlidePath.of_string("a / b / c")),
    )
  ),
];

let folder_tests = [
  test_case("joins the folder segments", `Quick, () =>
    check(
      option(string),
      "a / b",
      Some("a / b"),
      SlidePath.folder(SlidePath.of_string("a / b / c")),
    )
  ),
  test_case("none when the path has no folder segment", `Quick, () =>
    check(
      option(string),
      "Holes",
      None,
      SlidePath.folder(SlidePath.of_string("Holes")),
    )
  ),
];

/* A navigation space: two folders of two paths, then a folder of one. Named
   so a failure message says which folder a position belongs to. */
let basics_holes = 0
and basics_functions = 1
and tables_tables = 2
and tables_projection = 3
and probes_only = 4;

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

let folder_position_tests = [
  test_case("first path of a folder of two", `Quick, () =>
    check(
      pos_pair,
      "Basics / Holes",
      (0, 2),
      SlidePath.folder_position(~current=basics_holes, nav_paths),
    )
  ),
  test_case("last path of a folder of two", `Quick, () =>
    check(
      pos_pair,
      "Tables / Column Projection",
      (1, 2),
      SlidePath.folder_position(~current=tables_projection, nav_paths),
    )
  ),
  test_case("the only path in its folder", `Quick, () =>
    check(
      pos_pair,
      "Rich Probes / Rich Probes",
      (0, 1),
      SlidePath.folder_position(~current=probes_only, nav_paths),
    )
  ),
  test_case(
    "a folder whose paths are not adjacent",
    `Quick,
    () => {
      /* Grouping is by folder, not by adjacency: "A / z" belongs with "A / x"
         even with "B / y" between them. */
      let paths = List.map(SlidePath.of_string, ["A / x", "B / y", "A / z"]);
      check(
        pos_pair,
        "A / x is first of two",
        (0, 2),
        SlidePath.folder_position(~current=0, paths),
      );
      check(
        pos_pair,
        "A / z is second of two",
        (1, 2),
        SlidePath.folder_position(~current=2, paths),
      );
    },
  ),
];

let step_in_folder_tests = [
  test_case("forward within a folder", `Quick, () =>
    check(
      int,
      "Basics / Holes -> Basics / Functions",
      basics_functions,
      SlidePath.step_in_folder(~current=basics_holes, ~by=1, nav_paths),
    )
  ),
  test_case("backward within a folder", `Quick, () =>
    check(
      int,
      "Basics / Functions -> Basics / Holes",
      basics_holes,
      SlidePath.step_in_folder(~current=basics_functions, ~by=-1, nav_paths),
    )
  ),
  test_case("stops at the end of a folder", `Quick, ()
    /* Basics / Functions is last in Basics, and Tables / Tables is next in
       the list, so an unclamped step would cross into another folder. */
    =>
      check(
        int,
        "Basics / Functions stays put",
        basics_functions,
        SlidePath.step_in_folder(~current=basics_functions, ~by=1, nav_paths),
      )
    ),
  test_case("stops at the start of a folder", `Quick, () =>
    check(
      int,
      "Tables / Tables stays put",
      tables_tables,
      SlidePath.step_in_folder(~current=tables_tables, ~by=-1, nav_paths),
    )
  ),
  test_case(
    "cannot move within a folder of one",
    `Quick,
    () => {
      check(
        int,
        "Rich Probes / Rich Probes forward",
        probes_only,
        SlidePath.step_in_folder(~current=probes_only, ~by=1, nav_paths),
      );
      check(
        int,
        "Rich Probes / Rich Probes backward",
        probes_only,
        SlidePath.step_in_folder(~current=probes_only, ~by=-1, nav_paths),
      );
    },
  ),
  test_case(
    "steps over a path from another folder",
    `Quick,
    () => {
      let paths = List.map(SlidePath.of_string, ["A / x", "B / y", "A / z"]);
      check(
        int,
        "A / x -> A / z, skipping B / y",
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
      crumb_options(nav_paths, tables_tables),
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
        "B2T2 / API / Constructors / vcat",
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
    "a folder option moves to the first path in it",
    `Quick,
    () => {
      let outermost =
        SlidePath.breadcrumb(~current=basics_holes, nav_paths) |> List.hd;
      check(
        ints,
        "Basics, Tables, Rich Probes",
        [basics_holes, tables_tables, probes_only],
        List.map(fst, outermost.options),
      );
    },
  ),
  test_case(
    "a name that is a prefix of another is skipped",
    `Quick,
    () => {
      /* "Tables" has no segment at depth 1; before this module that raised
         Failure("nth") and took down the whole top bar. */
      let paths =
        List.map(SlidePath.of_string, ["Tables", "Tables / Tables"]);
      check(
        crumbs,
        "the deeper crumb omits the shorter path",
        [("Tables", ["Tables"]), ("Tables", ["Tables"])],
        crumb_options(paths, 1),
      );
    },
  ),
  test_case("an out-of-range position has no breadcrumb", `Quick, () =>
    check(crumbs, "empty", [], crumb_options(nav_paths, 99))
  ),
];

let tests = [
  ("SlidePath.of_string", of_string_tests),
  ("SlidePath.mk", mk_tests),
  ("SlidePath.to_string", to_string_tests),
  ("SlidePath.leaf", leaf_tests),
  ("SlidePath.folders", folders_tests),
  ("SlidePath.segments", segments_tests),
  ("SlidePath.folder", folder_tests),
  ("SlidePath.folder_position", folder_position_tests),
  ("SlidePath.step_in_folder", step_in_folder_tests),
  ("SlidePath.breadcrumb", breadcrumb_tests),
];
