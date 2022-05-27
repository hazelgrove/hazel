let test_with_eval = source => {
  // Compile and execute expression.
  let compile_out = Common.Compile.(source |> compile |> run);
  // Evaluate expression.
  let eval_out = Common.Eval.(source |> parse |> elab |> eval |> stringify);

  Base.([%test_eq: string](compile_out, eval_out));
};

let test = (source, expect) => {
  // Compile and execute expression.
  let compile_out = Common.Compile.(source |> compile |> run);

  Base.([%test_eq: string](compile_out, expect));
};
