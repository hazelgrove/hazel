open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  initial_state: option(Export.all),
  actions: Log.Entry.s,
};

let serialize = (log: t): string =>
  log |> sexp_of_t |> Sexplib.Sexp.to_string;

let deserialize = (data: string): t =>
  data |> Sexplib.Sexp.of_string |> t_of_sexp;

let mk = (~initial_state: option(Export.all), ~log_data: string): t => {
  let actions =
    log_data
    |> Sexplib.Sexp.of_string
    |> Log.Entry.s_of_sexp_opt
    |> List.filter_map(x => x);
  {
    initial_state,
    actions,
  };
};

let of_file = (data: string): t => {
  let trimmed = String.trim(data);
  if (String.length(trimmed) > 0 && trimmed.[0] == '{') {
    /* JSON submission file format */
    let log_str = Export.import_just_log(trimmed);
    let actions =
      log_str
      |> Sexplib.Sexp.of_string
      |> Log.Entry.s_of_sexp_opt
      |> List.filter_map(x => x);
    {
      initial_state: None,
      actions,
    };
  } else {
    /* Sexp replay log format */
    deserialize(trimmed);
  };
};
