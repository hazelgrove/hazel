[@deriving sexp]
type t = {
  boxed_result: Evaluator.result,
  result: DHExp.t,
  result_ty: HTyp.t,
  hii: HoleInstanceInfo.t,
  assert_map: AssertMap.t,
};
