/* Logging system for actions. Persists log via HazelDB shared database. */

open Util;

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

/* One unreadable entry used to cost the whole log.

   The strict `s_of_sexp` raises on the first entry it cannot read, and the
   database has already been cleared by the time it runs -- so a bundle
   exported from an older build, carrying one action constructor that has
   since been renamed, emptied the log and put nothing back. Every entry that
   would have parsed went with it.

   `s_of_sexp_opt` reads them one at a time and drops only what it cannot
   read, which is what the replay path in Logged already uses. The count of
   what was dropped is worth printing: a silent gap in a log is worse than a
   noisy one. */
let import = (data: string): unit =>
  /* Should be fine to fire saves concurrently? */
  DB.clear_and(() => {
    switch (data |> Sexplib.Sexp.of_string |> Entry.s_of_sexp_opt) {
    | exception _ =>
      Printf.printf("Log.Entry.import: could not read the log at all")
    | entries =>
      List.iter(Option.iter(Entry.save), entries);
      switch (List.length(List.filter(Option.is_none, entries))) {
      | 0 => ()
      | dropped =>
        Printf.printf(
          "Log.Entry.import: dropped %d unreadable of %d entries",
          dropped,
          List.length(entries),
        )
      };
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
