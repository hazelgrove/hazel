open Alcotest;
open Util;

let tests = (
  "CsvUtil",
  [
    test_case(
      "Basic csv with header",
      `Quick,
      () => {
        let csv_string = {csv|a,b,c
        1,2,3|csv};
        check(
          list(list(pair(string, string))),
          "Returns value",
          [[("a", "1"), ("b", "2"), ("c", "3")]],
          CsvUtil.parse_csv_with_headers(csv_string),
        );
      },
    ),
    test_case(
      "Basic csv without header",
      `Quick,
      () => {
        let csv_string = {csv|1,2,3
4,5,6|csv};
        check(
          list(list(string)),
          "Returns value",
          [["1", "2", "3"], ["4", "5", "6"]],
          CsvUtil.parse_csv_without_headers(csv_string),
        );
      },
    ),
  ],
);
