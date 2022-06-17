let profile = Common.Test;

let test_with_eval = source => {
  // Compile and execute expression.
  let compile_out =
    Common.Compile.(source |> compile(~profile) |> run(~profile));
  // Evaluate expression.
  let eval_out =
    Common.Eval.(
      source
      |> parse(~profile)
      |> elab(~profile)
      |> eval(~profile)
      |> stringify(~profile)
    );

  Base.([%test_eq: string](compile_out, eval_out));
};

let test = (source, expect) => {
  // Compile and execute expression.
  let compile_out =
    Common.Compile.(source |> compile(~profile) |> run(~profile));

  Base.([%test_eq: string](compile_out, expect));
};
