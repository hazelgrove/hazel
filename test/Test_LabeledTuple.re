open Alcotest;
open Haz3lcore;

let test_rearrange = (name, analyzed_types, actual_values, expected_values) =>
  test_case(
    name,
    `Quick,
    () => {
      let actual =
        LabeledTuple.rearrange2(
          ~show_b=[%derive.show: int],
          analyzed_types,
          actual_values,
        );
      check(
        list(pair(option(string), int)),
        name,
        expected_values,
        actual,
      );
      ();
    },
  );
// Create a property test
let tests: list(test_case(return)) = [
  test_rearrange(
    "Singleton unlabeled",
    [None],
    [(None, 1)],
    [(None, 1)],
  ),
  test_rearrange(
    "Singleton labeled",
    [Some("a")],
    [(Some("a"), 1)],
    [(Some("a"), 1)],
  ),
  test_rearrange(
    "unlabeled remains same order",
    [None, None, None],
    [(None, 1), (None, 2), (None, 3)],
    [(None, 1), (None, 2), (None, 3)],
  ),
  test_rearrange(
    "fully labeled retains ordering",
    [Some("a"), Some("b"), Some("c")],
    [(Some("a"), 1), (Some("b"), 2), (Some("c"), 3)],
    [(Some("a"), 1), (Some("b"), 2), (Some("c"), 3)],
  ),
  test_rearrange(
    "Missing labels get added",
    [Some("a"), Some("b"), Some("c")],
    [(None, 1), (None, 2), (None, 3)],
    [(Some("a"), 1), (Some("b"), 2), (Some("c"), 3)],
  ),
  test_rearrange(
    "Present labels get reordered",
    [Some("a"), Some("b"), Some("c")],
    [(Some("b"), 1), (Some("a"), 2), (Some("c"), 3)],
    [(Some("a"), 2), (Some("b"), 1), (Some("c"), 3)],
  ),
  test_rearrange(
    "Partial labels get reordered",
    [Some("a"), Some("b"), Some("c")],
    [(Some("b"), 1), (None, 2), (None, 3)],
    [(Some("a"), 2), (Some("b"), 1), (Some("c"), 3)],
  ),
  test_rearrange(
    "Extra labels get reordered",
    [Some("a"), Some("b"), Some("c")],
    [(Some("d"), 4), (Some("b"), 1), (Some("a"), 2), (Some("c"), 3)],
    [(Some("a"), 2), (Some("b"), 1), (Some("c"), 3), (Some("d"), 4)],
  ),
  test_rearrange(
    "pair labeled, unlabled",
    [Some("a"), None],
    [(Some("a"), 1), (None, 2)],
    [(Some("a"), 1), (None, 2)],
  ),
  test_rearrange(
    "Independent label sets with some overlap",
    [Some("a"), Some("b"), None, Some("c"), None],
    [
      (Some("d"), 4),
      (Some("c"), 1),
      (Some("e"), 5),
      (Some("b"), 3),
      (None, 2),
    ],
    [
      (Some("a"), 4),
      (Some("b"), 3),
      (None, 5),
      (Some("c"), 1),
      (None, 2),
    ],
  ),
];
