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

let debug_result = (fn: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    m(
      "RESULT %s@.%s",
      fn,
      Sexplib.Sexp.to_string_hum(sexp),
      ~tags=timestamp(),
    )
  );

let debug_state = (fn: string, name: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    m(
      "STATE %s.%s@.%s",
      fn,
      name,
      Sexplib.Sexp.to_string_hum(sexp),
      ~tags=timestamp(),
    )
  );

let debug_states = (fn: string, states: list((string, Sexplib.Sexp.t))) =>
  List.iter(((name, sexp)) => debug_state(fn, name, sexp), states);

let debug_function =
    (
      fn: string,
      _args: list((string, Sexplib.Sexp.t)),
      ~id: option(int)=?,
      ~result_sexp: 'a => Sexplib.Sexp.t,
      thunk: unit => 'a,
    )
    : 'a => {
  let fn =
    fn ++ Option.fold(~none="", ~some=id => "/" ++ Int.to_string(id), id);
  debug_call(fn);
  /* debug_args(args); */
  let result = thunk();
  let sexp = result_sexp(result);
  /* debug_msg("RETURN " ++ fn); */
  debug_result(fn, sexp);
  result;
};
