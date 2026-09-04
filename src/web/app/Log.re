/* Logging system for actions. Persists log via HazelDB shared database. */

open Util_web;

module DB = {
  let add = (key: string, value: string): unit => {
    LogCount.increment();
    HazelDB.log_add(key, value);
  };

  let get_all = HazelDB.log_get_all;

  let clear_and = (callback): unit => {
    LogCount.clear();
    HazelDB.log_clear(~callback, ());
  };
};

module Entry = {
  [@deriving (show({with_path: false}), yojson, sexp)]
  type timestamp = float;

  [@deriving (show({with_path: false}), yojson, sexp)]
  type t = (timestamp, Page.Update.t);

  [@deriving (show({with_path: false}), yojson, sexp)]
  type s = list(t);

  let mk = (update): t => {
    (JsUtil.timestamp(), update);
  };

  let save = ((ts, action): t) =>
    DB.add(
      Printf.sprintf("%.0f", ts),
      (ts, action) |> sexp_of_t |> Sexplib.Sexp.to_string,
    );

  let s_of_sexp_opt = (sexp: Sexplib.Sexp.t): list(option(t)) =>
    switch (sexp) {
    | Sexplib.Sexp.List(lst) =>
      List.rev_map(
        entry_sexp =>
          try(Some(t_of_sexp(entry_sexp))) {
          | _ => None
          },
        lst,
      )
      |> List.rev
    | _ => []
    };
};

let get_and = (f: string => unit): unit =>
  DB.get_all(entries => f("(" ++ String.concat(" ", entries) ++ ")"));

// Sync the cached count with the database
let sync_count = (): unit =>
  DB.get_all(entries => LogCount.set(List.length(entries)));

let import = (data: string): unit =>
  /* Should be fine to fire saves concurrently? */
  DB.clear_and(() => {
    try(
      data
      |> Sexplib.Sexp.of_string
      |> Entry.s_of_sexp
      |> List.iter(Entry.save)
    ) {
    | _ => Printf.printf("Log.Entry.import: Deserialization error")
    };
    // Sync count after import completes
    sync_count();
  });

let update = (action: Page.Update.t, result: Updated.t('a)): unit =>
  if (result.logged) {
    Entry.save(Entry.mk(action));
  };

let to_actions = () => {
  print_endline("HELLO??");
  let actions = ref([]);
  DB.get_all(entries => {
    print_endline(
      "num of entries: " ++ string_of_int(List.length(entries)),
    );
    entries
    |> List.iter(entry_str =>
         try({
           let (_ts, action) =
             entry_str |> Sexplib.Sexp.of_string |> Entry.t_of_sexp;
           actions := [action, ...actions^];
         }) {
         | _ => print_endline("Log.to_actions: Deserialization error")
         }
       );
    actions := List.rev(actions^);
  });
  print_endline("num of actions: " ++ string_of_int(List.length(actions^)));
  actions^;
};

// If the user switched browsers or devices, they may have imported a save state from another device, this includes the log from the previous device in a complete stitched log.
let flatten_imports =
    (
      ~of_data: string => list((float, Page.Update.t)),
      log: list((float, Page.Update.t)),
    )
    : list((float, Page.Update.t)) => {
  let rec inner =
          (
            log: list((float, Page.Update.t)),
            acc: list((float, Page.Update.t)),
          ) => {
    switch (log) {
    | [] => acc
    | [(_t, Globals(FinishImportAll(Some(data)))), ..._rest] =>
      inner(List.rev(of_data(data)), acc)
    | [x, ...rest] => inner(rest, [x, ...acc])
    };
  };
  log |> List.rev |> inner(_, []);
};
