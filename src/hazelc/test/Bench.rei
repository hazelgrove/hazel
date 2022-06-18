let bench:
  (
    string,
    int64,
    ~grain_source: option(string)=?,
    ~wasm_source: option(string)=?,
    string
  ) =>
  unit;

let bench_file:
  (
    string,
    int64,
    ~grain_filename: option(string)=?,
    ~wasm_filename: option(string)=?,
    string
  ) =>
  unit;
