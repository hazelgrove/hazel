open Alcotest;
open Haz3lcore;

let format_test =
    (~max_width=40, name: string, input: string, expected: string) => {
  test_case(
    name,
    `Quick,
    () => {
      let input_segment = Parser.to_segment(input) |> Option.get;
      let formatted_segment =
        PrettySegment.format_segment(~max_width, input_segment);
      let actual = Printer.of_segment(formatted_segment);
      print_endline("Actual");
      print_endline(actual);
      print_endline("Expected");
      print_endline(expected);
      check(string, name, expected, actual);
    },
  );
};

let tests = [
  (
    "PrettySegment",
    [
      format_test(
        ~max_width=10,
        "Long list",
        {hazel|[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]|hazel},
        {hazel|[1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16,
    17,
    18,
    19,
    20]|hazel},
      ),
    ],
  ),
];
