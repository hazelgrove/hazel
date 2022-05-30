let src = Logs.Src.create("hazel.core");

include (val Logs.src_log(src): Logs.LOG);

let timestamp_tag: Logs.Tag.def(float) =
  Logs.Tag.def("timestamp", (m, ts) => Format.fprintf(m, "%f", ts));

let timestamp = () =>
  Logs.Tag.empty |> Logs.Tag.add(timestamp_tag, Unix.gettimeofday());

let debug_msg = (msg: string) => debug(m => m("%s", msg, ~tags=timestamp()));

let debug_call = (fn: string) =>
  debug(m => m("CALL %s", fn, ~tags=timestamp()));

let debug_arg = (arg: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    m("ARG %s@.%s", arg, Sexplib.Sexp.to_string_hum(sexp), ~tags=timestamp())
  );

let debug_args = (args: list((string, Sexplib.Sexp.t))) =>
  List.iter(((arg, sexp)) => debug_arg(arg, sexp), args);

let debug_result = (fn: string, result: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    m(
      "RESULT %s.%s@.%s",
      fn,
      result,
      Sexplib.Sexp.to_string_hum(sexp),
      ~tags=timestamp(),
    )
  );

let debug_results = (fn: string, results: list((string, Sexplib.Sexp.t))) =>
  List.iter(((result, sexp)) => debug_result(fn, result, sexp), results);
