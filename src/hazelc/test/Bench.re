open LetOpen.Syntax;

module W = Wasmtime.Wrappers;

let bench = source => {
  open Common.Compile;

  // Compile.
  let wasm_path = source |> compile;

  // Read wasm module.
  let wasm = {
    let&i wasm_ch = open_in(wasm_path);
    really_input_string(wasm_ch, in_channel_length(wasm_ch));
  };

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
  let name = W.Byte_vec.of_string("bench");
  W.Wasmtime.Linker.module_(linker, ~name, modl);

  // Get `_start` function.
  let start_func = W.Wasmtime.Linker.get_default(linker, ~name);

  () => W.Wasmtime.func_call0(start_func, []);
};

let bench_eval = source => {
  open Common.Eval;

  let d = source |> parse |> elab;
  () => ignore(d |> eval);
};
