open Alcotest;

let split = (body: string): list((option(string), int)) =>
  Omd.of_string(body)
  |> Web.TaskReferenceSplit.split
  |> List.map(((h, blocks)) => (h, List.length(blocks)));

let section = pair(option(string), int);

let tests = (
  "TaskReferenceSplit",
  [
    test_case("empty body produces no sections", `Quick, () => {
      check(list(section), "empty", [], split(""))
    }),
    test_case("preamble only (no H3) is one None section", `Quick, () => {
      check(
        list(section),
        "preamble",
        [(None, 1)],
        split("Just a paragraph."),
      )
    }),
    test_case("single H3 produces one Some section", `Quick, () => {
      check(
        list(section),
        "single",
        [(Some("Title"), 1)],
        split("### Title\n\nbody"),
      )
    }),
    test_case("preamble plus H3 sections", `Quick, () => {
      check(
        list(section),
        "preamble + sections",
        [(None, 1), (Some("A"), 1), (Some("B"), 2)],
        split("intro\n\n### A\n\nbody-a\n\n### B\n\nbody-b1\n\nbody-b2"),
      )
    }),
    test_case("H2 does not split (only H3 boundaries)", `Quick, () => {
      check(
        list(section),
        "h2 stays in preamble",
        [(None, 2)],
        split("## Not a section\n\nstill preamble"),
      )
    }),
  ],
);
