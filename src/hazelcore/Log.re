include (val Logs.src_log(Logs.Src.create("hazel")): Logs.LOG);

/* Timestamps */

let timestamp_tag: Logs.Tag.def(float) =
  Logs.Tag.def("timestamp", (m, ts) => Format.fprintf(m, "%f", ts));

let timestamp = () =>
  Logs.Tag.empty |> Logs.Tag.add(timestamp_tag, Unix.gettimeofday());

/* Message Filtering */

module Filter = {
  type loc =
    | Eq(string)
    | Pre(string);

  type t =
    | Fun(loc)
    | Mod(loc)
    | And(t, t)
    | Or(t, t);

  let eq = str => Eq(str);
  let pre = str => Pre(str);

  let md = loc => Mod(loc);
  let fn = loc => Fun(loc);

  let ( * ) = (a, b) => And(a, b);
  let (+) = (a, b) => Or(a, b);

  let matches_loc = (loc, str) =>
    switch (loc) {
    | Eq(str') => String.equal(str', str)
    | Pre(prefix) =>
      String.equal(prefix, String.sub(str, 0, String.length(prefix)))
    };

  let rec matches = (flt, (mod_name, fun_name)) =>
    switch (flt) {
    | Fun(loc) => matches_loc(loc, fun_name)
    | Mod(loc) => matches_loc(loc, mod_name)
    | And(flt1, flt2) =>
      matches(flt1, (mod_name, fun_name))
      && matches(flt2, (mod_name, fun_name))
    | Or(flt1, flt2) =>
      matches(flt1, (mod_name, fun_name))
      || matches(flt2, (mod_name, fun_name))
    };
};

let watch_list =
  Some(
    Filter.(
      md(pre("Action_TPat"))
      + md(pre("Action_Exp"))
      * fn(pre("syn_perform_line"))
    ),
  );

let watching = fn =>
  {
    open OptUtil.Syntax;
    let+ watch_list = watch_list;
    let (m, f) =
      switch (List.rev(String.split_on_char('.', fn))) {
      | [] => ("--", "--")
      | [fun_name] => ("--", fun_name)
      | [fun_name, ...mod_names] => (
          String.concat(".", List.rev(mod_names)),
          fun_name,
        )
      };
    Filter.matches(watch_list, (m, f));
  }
  |> Option.value(~default=true);

/* Level-specific Helpers */

let debug_msg = (msg: string) => debug(m => m("%s", msg, ~tags=timestamp()));

let info_call = (fn: string) =>
  info(m => m("CALL %s", fn, ~tags=timestamp()));

/* Function Arguments */

let debug_arg = (fn: string, name: string, sexp: unit => Sexplib.Sexp.t) =>
  debug(m =>
    if (watching(fn)) {
      m(
        "ARG %s@.%s",
        name,
        Sexplib.Sexp.to_string_hum(sexp()),
        ~tags=timestamp(),
      );
    }
  );

let debug_args = (fn: string, args: list((string, unit => Sexplib.Sexp.t))) =>
  List.iter(((name, sexp)) => debug_arg(fn, name, sexp), args);

let debug_result = (fn: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    if (watching(fn)) {
      m(
        "RESULT %s@.%s",
        fn,
        Sexplib.Sexp.to_string_hum(sexp),
        ~tags=timestamp(),
      );
    }
  );

/* Intermediate States */

let debug_state = (fn: string, name: string, sexp: Sexplib.Sexp.t) =>
  debug(m =>
    if (watching(fn)) {
      m(
        "STATE %s.%s@.%s",
        fn,
        name,
        Sexplib.Sexp.to_string_hum(sexp),
        ~tags=timestamp(),
      );
    }
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
  let fname =
    fn ++ Option.fold(~none="", ~some=id => "/" ++ Int.to_string(id), id);
  info_call(fname);
  if (Option.is_some(args)) {
    debug_args(fn, Option.value(~default=[], args));
  };
  let result = thunk();
  debug_result(fn, result_sexp(result));
  result;
};

/* Logs Reporter */

let reporter = ppf => {
  let report = (src, level, ~over, k, msgf) => {
    let k = _ => {
      over();
      k();
    };
    let timestamped = (h, tags, k, ppf, fmt) => {
      let ts =
        {
          open OptUtil.Syntax;
          let* tags = tags;
          Logs.Tag.find(timestamp_tag, tags);
        }
        |> Option.value(~default=0.0);
      Format.kfprintf(
        k,
        ppf,
        "%a %+04.0f %s @[" ^^ fmt ^^ "@]@.",
        Logs.pp_header,
        (level, h),
        ts,
        Logs.Src.name(src),
      );
    };
    msgf((~header=?, ~tags=?, fmt) =>
      timestamped(header, tags, k, ppf, fmt)
    );
  };
  {Logs.report: report};
};
