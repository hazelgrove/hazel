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
