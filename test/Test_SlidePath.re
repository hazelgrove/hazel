open Alcotest;
open Web;

/* Tests for SlidePath: the "/"-separated hierarchical name behind
   Documentation-mode slide names and Tutorial-mode lesson titles. */

let path = testable(Fmt.of_to_string(SlidePath.show), SlidePath.equal);
let ints = list(int);
let strings = list(string);
let folder_position =
  testable(
    Fmt.of_to_string(
      ({index_in_folder, folder_size}: SlidePath.folder_position) =>
      Printf.sprintf("%d of %d", index_in_folder, folder_size)
    ),
    (==),
  );
let at = (index_in_folder, folder_size): SlidePath.folder_position => {
  index_in_folder,
  folder_size,
};

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
    check(
      strings,
      "folders of a bare leaf",
      [],
      SlidePath.folders(SlidePath.mk("Holes")),
    )
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
      "leaf of a / b / c",
      "c",
      SlidePath.leaf(SlidePath.of_string("a / b / c")),
    )
  ),
];

let folders_tests = [
  test_case("every segment but the last", `Quick, () =>
    check(
      strings,
      "folders of a / b / c",
      ["a", "b"],
      SlidePath.folders(SlidePath.of_string("a / b / c")),
    )
  ),
];

let segments_tests = [
  test_case("the folders and the leaf", `Quick, () =>
    check(
      strings,
      "segments of a / b / c",
      ["a", "b", "c"],
      SlidePath.segments(SlidePath.of_string("a / b / c")),
    )
  ),
];

let folder_tests = [
  test_case("joins the folder segments", `Quick, () =>
    check(
      option(string),
      "folder of a / b / c",
      Some("a / b"),
      SlidePath.folder(SlidePath.of_string("a / b / c")),
    )
  ),
  test_case("none when the path has no folder segment", `Quick, () =>
    check(
      option(string),
      "folder of Holes",
      None,
      SlidePath.folder(SlidePath.of_string("Holes")),
    )
  ),
];

/* A navigation space: two folders of two slides, then a folder of one,
   written as the names a slide picker would show. The helpers below let each
   test name the slide it acts on, and report where it lands by name too. */
let nav_space = [
  "Basics / Holes",
  "Basics / Functions",
  "Tables / Filtering",
  "Tables / Column Projection",
  "Rich Probes / Sampling",
];

let paths_of = (space: list(string)) =>
  List.map(SlidePath.of_string, space);

let index_of = (space: list(string), name: string): int =>
  switch (Util.ListUtil.findi_opt(String.equal(name), space)) {
  | Some((i, _)) => i
  | None => failwith("not in this space: " ++ name)
  };

/* Where `name` sits among the slides sharing its folder. */
let position_of = (~space=nav_space, name: string) =>
  SlidePath.folder_position(
    ~current=index_of(space, name),
    paths_of(space),
  );

/* The slide `by` steps away within `name`'s folder. */
let step_from = (~space=nav_space, ~by: int, name: string): string =>
  SlidePath.step_in_folder(
    ~current=index_of(space, name),
    ~by,
    paths_of(space),
  )
  |> List.nth(space);

/* Each crumb as (selected segment, the segments it offers). */
let crumbs_of = (~space=nav_space, name: string) =>
  SlidePath.breadcrumb(~current=index_of(space, name), paths_of(space))
  |> List.map((c: SlidePath.crumb) =>
       (c.selected, List.map(snd, c.options))
     );

/* Each crumb as (selected segment, the slides its options jump to). */
let jumps_of = (~space=nav_space, name: string) =>
  SlidePath.breadcrumb(~current=index_of(space, name), paths_of(space))
  |> List.map((c: SlidePath.crumb) =>
       (c.selected, List.map(((i, _)) => List.nth(space, i), c.options))
     );

let crumbs =
  testable(
    Fmt.of_to_string(cs =>
      cs
      |> List.map(((sel, opts)) =>
           sel ++ ":[" ++ String.concat(", ", opts) ++ "]"
         )
      |> String.concat("  /  ")
    ),
    (==),
  );

let scattered = ["A / x", "B / y", "A / z"];

let folder_position_tests = [
  test_case("first slide of a folder of two", `Quick, () =>
    check(
      folder_position,
      "position of Basics / Holes",
      at(0, 2),
      position_of("Basics / Holes"),
    )
  ),
  test_case("last slide of a folder of two", `Quick, () =>
    check(
      folder_position,
      "position of Tables / Column Projection",
      at(1, 2),
      position_of("Tables / Column Projection"),
    )
  ),
  test_case("the only slide in its folder", `Quick, () =>
    check(
      folder_position,
      "position of Rich Probes / Sampling",
      at(0, 1),
      position_of("Rich Probes / Sampling"),
    )
  ),
  test_case(
    "a folder whose slides are not adjacent",
    `Quick,
    () => {
      /* Grouping is by folder, not by adjacency: "A / z" belongs with "A / x"
         even with "B / y" between them. */
      check(
        folder_position,
        "position of A / x",
        at(0, 2),
        position_of(~space=scattered, "A / x"),
      );
      check(
        folder_position,
        "position of A / z",
        at(1, 2),
        position_of(~space=scattered, "A / z"),
      );
    },
  ),
];

let step_in_folder_tests = [
  test_case("forward within a folder", `Quick, () =>
    check(
      string,
      "Basics / Holes forward",
      "Basics / Functions",
      step_from(~by=1, "Basics / Holes"),
    )
  ),
  test_case("backward within a folder", `Quick, () =>
    check(
      string,
      "Basics / Functions backward",
      "Basics / Holes",
      step_from(~by=-1, "Basics / Functions"),
    )
  ),
  test_case("stops at the end of a folder", `Quick, ()
    /* "Basics / Functions" is last in Basics and "Tables / Filtering" is next
       in the space, so an unclamped step would cross into another folder. */
    =>
      check(
        string,
        "Basics / Functions forward stays put",
        "Basics / Functions",
        step_from(~by=1, "Basics / Functions"),
      )
    ),
  test_case("stops at the start of a folder", `Quick, () =>
    check(
      string,
      "Tables / Filtering backward stays put",
      "Tables / Filtering",
      step_from(~by=-1, "Tables / Filtering"),
    )
  ),
  test_case(
    "cannot move within a folder of one",
    `Quick,
    () => {
      check(
        string,
        "Rich Probes / Sampling forward",
        "Rich Probes / Sampling",
        step_from(~by=1, "Rich Probes / Sampling"),
      );
      check(
        string,
        "Rich Probes / Sampling backward",
        "Rich Probes / Sampling",
        step_from(~by=-1, "Rich Probes / Sampling"),
      );
    },
  ),
  test_case("steps over a slide from another folder", `Quick, () =>
    check(
      string,
      "A / x forward, skipping B / y",
      "A / z",
      step_from(~space=scattered, ~by=1, "A / x"),
    )
  ),
];

let breadcrumb_tests = [
  test_case("one crumb per segment, siblings as options", `Quick, () =>
    check(
      crumbs,
      "breadcrumb for Tables / Filtering",
      [
        ("Tables", ["Basics", "Tables", "Rich Probes"]),
        ("Filtering", ["Filtering", "Column Projection"]),
      ],
      crumbs_of("Tables / Filtering"),
    )
  ),
  test_case(
    "deeper slides yield a deeper breadcrumb",
    `Quick,
    () => {
      let space = [
        "B2T2 / API / Constructors / vcat",
        "B2T2 / API / Constructors / hcat",
        "B2T2 / Datasheet",
      ];
      check(
        crumbs,
        "breadcrumb for B2T2 / API / Constructors / vcat",
        [
          ("B2T2", ["B2T2"]),
          ("API", ["API", "Datasheet"]),
          ("Constructors", ["Constructors"]),
          ("vcat", ["vcat", "hcat"]),
        ],
        crumbs_of(~space, "B2T2 / API / Constructors / vcat"),
      );
    },
  ),
  test_case("a folder option jumps to the first slide in it", `Quick, () =>
    check(
      crumbs,
      "the outermost crumb of Basics / Holes",
      [
        (
          "Basics",
          ["Basics / Holes", "Tables / Filtering", "Rich Probes / Sampling"],
        ),
      ],
      jumps_of("Basics / Holes") |> List.filteri((i, _) => i == 0),
    )
  ),
  test_case("a name that is a prefix of another is skipped", `Quick, ()
    /* "Tables" has no segment at depth 1; before this module that raised
       Failure("nth") and took down the whole top bar. */
    =>
      check(
        crumbs,
        "the deeper crumb omits the shorter name",
        [("Tables", ["Tables"]), ("Tables", ["Tables"])],
        crumbs_of(~space=["Tables", "Tables / Tables"], "Tables / Tables"),
      )
    ),
  test_case("a position outside the space has no breadcrumb", `Quick, () =>
    check(
      crumbs,
      "no crumbs",
      [],
      SlidePath.breadcrumb(~current=99, paths_of(nav_space))
      |> List.map((c: SlidePath.crumb) =>
           (c.selected, List.map(snd, c.options))
         ),
    )
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
