open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  test_map: TestMap.t,
  statuses: list(TestStatus.t),
  descriptions: list(string),
  total: int,
  passing: int,
  failing: int,
  unfinished: int,
};

let mk_results = (~descriptions=[], test_map: TestMap.t): t => {
  test_map,
  statuses: test_map |> List.map(r => r |> snd |> TestMap.joint_status),
  descriptions,
  total: TestMap.count(test_map),
  passing: TestMap.count_status(Pass, test_map),
  failing: TestMap.count_status(Fail, test_map),
  unfinished: TestMap.count_status(Indet, test_map),
};

let result_summary_str =
    (~n, ~p, ~q, ~n_str, ~ns_str, ~p_str, ~q_str, ~r_str): string => {
  let one_p = "one is " ++ p_str ++ " ";
  let one_q = "one is " ++ q_str ++ " ";
  let mny_p = Printf.sprintf("%d are %s ", p, p_str);
  let mny_q = Printf.sprintf("%d are %s ", q, q_str);
  let of_n = Printf.sprintf("Out of %d %s, ", n, ns_str);
  switch (n, p, q) {
  | (0, _, _) => "No " ++ ns_str ++ " available."
  | (_, 0, 0) => "All " ++ ns_str ++ " " ++ r_str ++ "! "
  | (n, _, c) when n == c => "All " ++ ns_str ++ " " ++ q_str ++ " "
  | (n, f, _) when n == f => "All " ++ ns_str ++ " " ++ p_str ++ " "
  | (1, 0, 1) => "One " ++ n_str ++ " " ++ q_str ++ " "
  | (1, 1, 0) => "One " ++ n_str ++ " " ++ p_str ++ " "
  | (2, 1, 1) =>
    "One " ++ n_str ++ " " ++ p_str ++ " and one " ++ q_str ++ " "
  | (_, 0, 1) => of_n ++ one_q
  | (_, 1, 0) => of_n ++ one_p
  | (_, 1, 1) => of_n ++ one_p ++ "and " ++ one_q
  | (_, 1, _) => of_n ++ one_p ++ "and " ++ mny_q
  | (_, _, 1) => of_n ++ mny_p ++ "and " ++ one_q
  | (_, 0, _) => of_n ++ mny_q
  | (_, _, 0) => of_n ++ mny_p
  | (_, _, _) => of_n ++ mny_p ++ "and " ++ mny_q
  };
};

let test_summary_str = (test_results: t): string =>
  result_summary_str(
    ~n=test_results.total,
    ~p=test_results.failing,
    ~q=test_results.unfinished,
    ~n_str="test",
    ~ns_str="tests",
    ~p_str="failing",
    ~q_str="indeterminate",
    ~r_str="passing",
  );
