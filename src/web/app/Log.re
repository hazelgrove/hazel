/* Logging system for actions. Persists log via IndexedDB */

open Util;

module DB = {
  open Ezjs_idb;

  module Store = Ezjs_idb.Store(StringTr, StringTr);

  type db = Ezjs_min.t(Types.iDBDatabase);

  let db_name = "hazel_db";
  let table_name = "log";

  let kv_store = (db: db): Store.store =>
    Store.store(~mode=READWRITE, db, table_name);

  let with_db = (f): unit => {
    let error = _: unit => print_endline("ERROR: Log.IDBKV.open");
    let upgrade = (db: db, e: db_upgrade): unit =>
      e.new_version >= 1 && e.old_version == 0
        ? ignore(Store.create(db, table_name)) : ();
    openDB(~upgrade, ~error, ~version=1, db_name, db => f(db));
  };

  let add = (key: string, value: string): unit => {
    LogCount.increment();
    with_db(db =>
      Store.add(~key, ~callback=_key => (), kv_store(db), value)
    );
  };

  let get = (key: string, f: option(string) => unit): unit => {
    let error = _ => Printf.printf("ERROR: Log.IDBKV.get");
    with_db(db => Store.get(~error, kv_store(db), f, K(key)));
  };

  let get_all = (f: list(string) => unit): unit => {
    let error = _ => Printf.printf("ERROR: Log.IDBKV.get_all");
    with_db(db => Store.get_all(~error, kv_store(db), f));
  };

  let clear_and = (callback): unit => {
    let error = _ => Printf.printf("ERROR: Log.IDBKV.clear");
    LogCount.clear();
    with_db(db => Store.clear(~error, ~callback, kv_store(db)));
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

let get_count = (f: int => unit): unit =>
  DB.get_all(entries => f(List.length(entries)));

// Synchronously get the cached count (may be stale until sync_count is called)
let get_count_sync = (): int => LogCount.get();

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
