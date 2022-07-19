open Hazelctest.Bench;

let bench = (name, times, ~grain_source=None, ~wasm_source=None, hazel_source) => {
  let name' = prefix => prefix ++ " " ++ name;
  let bench' = fs => {
    let fs = fs |> List.map(((name, f)) => (name, f, ()));
    Benchmark.latencyN(~style=Auto, times, fs);
  };

  let bench_source = (name, f, source) => [(name'(name), f(source))];
  let bench_source' = (name, f, source) =>
    source |> Option.map(bench_source(name, f)) |> Option.value(~default=[]);

  let tests =
    [
      hazel_source |> bench_source("compiled", bench_comp),
      hazel_source |> bench_source("eval", bench_eval),
      grain_source |> bench_source'("grain", bench_grain),
      wasm_source |> bench_source'("wasm", bench_wasm),
    ]
    |> List.flatten;

  bench'(tests) |> ignore;
};

let bench_file =
    (name, times, ~grain_filename=None, ~wasm_filename=None, hazel_filename) => {
  let hazel_source = hazel_filename |> read_file_to_string;
  let grain_source = grain_filename |> Option.map(read_file_to_string);
  let wasm_source = wasm_filename |> Option.map(read_file_to_string);
  bench(name, times, hazel_source, ~grain_source, ~wasm_source);
};
