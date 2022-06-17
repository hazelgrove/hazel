open ChannelUtil.Syntax;

module W = Wasmtime.Wrappers;

let profile = Common.Bench;

let read_file_to_string = filename => {
  let&i ch = open_in(filename);
  really_input_string(ch, in_channel_length(ch));
};

let bench_wasm = wasm => {
  // Create wasmtime engine and load module.
  let engine = W.Engine.create();
  let store = W.Store.create(engine);

  let wasm = W.Byte_vec.of_string(wasm);
  let modl = W.Wasmtime.new_module(engine, ~wasm);

  // Link wasi for `fd_write`.
  let wasi_instance = W.Wasi_instance.create(store, `wasi_snapshot_preview);
  let linker = W.Wasmtime.Linker.create(store);
  W.Wasmtime.Linker.define_wasi(linker, wasi_instance);

  // Link module.
  let modl_name = W.Byte_vec.of_string("bench");
  W.Wasmtime.Linker.module_(linker, ~name=modl_name, modl);

  // Get `_start` function.
  let start_func = W.Wasmtime.Linker.get_default(linker, ~name=modl_name);

  () => W.Wasmtime.func_call0(start_func, []);
};

let bench_grain = grain_source => {
  open Common.Compile;

  let wasm_path = grain_source |> compile_grain(~profile);

  // Read wasm module.
  let wasm = read_file_to_string(wasm_path);
  bench_wasm(wasm);
};

let bench_comp = hazel_source => {
  open Common.Compile;

  // Compile.
  let wasm_path = hazel_source |> compile(~profile);

  // Read wasm module.
  let wasm = {
    let&i wasm_ch = open_in(wasm_path);
    really_input_string(wasm_ch, in_channel_length(wasm_ch));
  };

  bench_wasm(wasm);
};

let bench_eval = hazel_source => {
  open Common.Eval;

  let d = hazel_source |> parse(~profile) |> elab(~profile);
  () => ignore(d |> eval(~profile));
};

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
