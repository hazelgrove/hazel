module type T = {
  let subsystem: option(string);
  let sort: option(string);
};

let log_name = (subsystem_opt, sort_opt) =>
  String.concat(
    ".",
    [
      "hazel",
      ...switch (subsystem_opt, sort_opt) {
         | (Some(subsystem), Some(sort)) => [subsystem, sort]
         | (Some(name), None)
         | (None, Some(name)) => [name]
         | (None, None) => []
         },
    ],
  );

module Make = (T: T) => {
  include (
            val Logs.src_log(Logs.Src.create(log_name(T.subsystem, T.sort))): Logs.LOG
          );

  /* Timestamps */

  let timestamp_tag: Logs.Tag.def(float) =
    Logs.Tag.def("timestamp", (m, ts) => Format.fprintf(m, "%f", ts));

  let timestamp = () =>
    Logs.Tag.empty |> Logs.Tag.add(timestamp_tag, Unix.gettimeofday());

  /* Level-specific Helpers */

  let debug_msg = (msg: string) =>
    debug(m => m("%s", msg, ~tags=timestamp()));

  let info_call = (fn: string) =>
    info(m => m("CALL %s", fn, ~tags=timestamp()));

  /* Function Arguments */

  let debug_arg = (name: string, sexp: unit => Sexplib.Sexp.t) =>
    debug(m =>
      m(
        "ARG %s@.%s",
        name,
        Sexplib.Sexp.to_string_hum(sexp()),
        ~tags=timestamp(),
      )
    );

  let debug_args = (args: list((string, unit => Sexplib.Sexp.t))) =>
    List.iter(((name, sexp)) => debug_arg(name, sexp), args);

  let debug_result = (fn: string, sexp: Sexplib.Sexp.t) =>
    debug(m =>
      m(
        "RESULT %s@.%s",
        fn,
        Sexplib.Sexp.to_string_hum(sexp),
        ~tags=timestamp(),
      )
    );

  /* Intermediate States */

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

  /* Function Calls */

  let fun_call =
      (
        fn: string,
        ~args: option(list((string, unit => Sexplib.Sexp.t)))=?,
        ~id: option(int)=?,
        ~result_sexp: 'a => Sexplib.Sexp.t,
        thunk: unit => 'a,
      )
      : 'a => {
    let fn =
      fn ++ Option.fold(~none="", ~some=id => "/" ++ Int.to_string(id), id);
    info_call(fn);
    if (Option.is_some(args)) {
      debug_args(Option.value(~default=[], args));
    };
    let result = thunk();
    let sexp = result_sexp(result);
    debug_result(fn, sexp);
    result;
  };
};
