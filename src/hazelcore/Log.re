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
    | Pre(string)
    | Suf(string)
    | Has(string)
    | Re(Re.re);

  type t =
    | Disabled
    | All
    | And(t, t)
    | Or(t, t)
    | Not(t)
    | Mod(loc)
    | Fun(loc)
    | Args(list(loc));

  let eq = str => Eq(str);
  let pre = str => Pre(str);
  let suf = str => Suf(str);
  let has = str => Has(str);
  let re = str => Re(Re.compile(Re.Perl.re(str)));

  let (+^) = (a, b) => And(a, b);
  let (/^) = (a, b) => Or(a, b);
  let not = a => Not(a);

  let md = loc => Mod(loc);
  let fn = loc => Fun(loc);
  let args = locs => Args(locs);

  let matches_loc = (str, loc) =>
    switch (loc) {
    | Eq(str') => String.equal(str', str)
    | Re(re) => Re.execp(re, str)
    | Pre(prefix) => String.starts_with(~prefix, str)
    | Suf(suffix) => String.ends_with(~suffix, str)
    | Has(substr) => StringUtil.contains_substring(str, substr)
    };

  let rec matches = (flt, (mod_name, fun_name), arg_opt) =>
    switch (flt) {
    | Disabled => false
    | All => true
    | And(flt1, flt2) =>
      matches(flt1, (mod_name, fun_name), arg_opt)
      && matches(flt2, (mod_name, fun_name), arg_opt)
    | Or(flt1, flt2) =>
      matches(flt1, (mod_name, fun_name), arg_opt)
      || matches(flt2, (mod_name, fun_name), arg_opt)
    | Not(flt1) => !matches(flt1, (mod_name, fun_name), arg_opt)
    | Mod(loc) => matches_loc(mod_name, loc)
    | Fun(loc) => matches_loc(fun_name, loc)
    | Args(locs) =>
      {
        open OptUtil.Syntax;
        let+ arg = arg_opt;
        List.exists(matches_loc(arg), locs);
      }
      |> Option.value(~default=true)
    };
};

let watch_list =
  Filter.(
    md(pre("Action")) /^ fn(eq("subst_tyvars")) /^ fn(eq("rescope"))
  );

let watching = (fn, arg_opt) =>
  Filter.matches(
    watch_list,
    switch (List.rev(String.split_on_char('.', fn))) {
    | [] => ("--", "--")
    | [fun_name] => ("--", fun_name)
    | [fun_name, ...mod_names] => (
        String.concat(".", List.rev(mod_names)),
        fun_name,
      )
    },
    arg_opt,
  );

/* Level-specific Helpers */

let debug_msg = (msg: string) =>
  debug(m => m("%s@?", msg, ~tags=timestamp()));

let info_call = (fn: string) =>
  info(m => m("CALL %s@?", fn, ~tags=timestamp()));

/* Function Arguments */

let debug_arg = (fn: string, arg: string, sexp: unit => Sexplib.Sexp.t) =>
  if (watching(fn, Some(arg))) {
    debug(m =>
      m(
        "ARG %s@.%s@?",
        arg,
        Sexplib.Sexp.to_string_hum(sexp()),
        ~tags=timestamp(),
      )
    );
  };

let debug_args = (fn: string, args: list((string, unit => Sexplib.Sexp.t))) =>
  List.iter(((name, sexp)) => debug_arg(fn, name, sexp), args);

let debug_result =
    (
      fn: string,
      args: option(list((string, unit => Sexplib.Sexp.t))),
      sexp: Sexplib.Sexp.t,
    ) =>
  if (watching(fn, None)) {
    debug(m =>
      m(
        "RESULT %s@.%s@?",
        fn,
        Sexplib.Sexp.to_string_hum(sexp),
        ~tags=timestamp(),
      )
    );
    if (Option.is_some(args)) {
      debug_args(fn, Option.value(~default=[], args));
    };
  };

/* Intermediate States */

let debug_state = (fn: string, name: string, sexp: Sexplib.Sexp.t) =>
  /* if (watching(fn)) { */
  debug(m =>
    m(
      "STATE %s.%s@.%s@?",
      fn,
      name,
      Sexplib.Sexp.to_string_hum(sexp),
      ~tags=timestamp(),
    )
  );
/* }; */

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
  debug_result(fname, args, result_sexp(result));
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
        "%a %+04.0f %s @[" ^^ fmt ^^ "@]@?",
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
