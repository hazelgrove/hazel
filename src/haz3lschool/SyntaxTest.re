open Haz3lcore;
open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type syntax_result = {
  results: list(bool),
  percentage: float,
};

let check =
    (uexp: Term.UExp.t, tests: list(Term.UExp.t => bool)): syntax_result => {
  let results = List.map(fn => {uexp |> fn}, tests);
  let length = List.length(tests);
  let passing = Util.ListUtil.count_pred(res => res, results);

  {
    results,
    percentage:
      //vacuously passes if there are no tests
      length == 0 ? 1. : float_of_int(passing) /. float_of_int(length),
  };
};
