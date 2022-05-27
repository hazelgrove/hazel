let bench_eval = source => {
  open Common.Eval;

  let d = source |> parse |> elab;
  () => ignore(d |> eval);
};

let bench = source => {
  open Common.Compile;

  let wasm_path = source |> compile;
  () => ignore(wasm_path |> run);
};
