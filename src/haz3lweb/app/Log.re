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

  let add = (key: string, value: string): unit =>
    with_db(db =>
      Store.add(~key, ~callback=_key => (), kv_store(db), value)
    );

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
};

let import = (data: string): unit =>
  /* Should be fine to fire saves concurrently? */
  DB.clear_and(() =>
    try(
      data
      |> Sexplib.Sexp.of_string
      |> Entry.s_of_sexp
      |> List.iter(Entry.save)
    ) {
    | _ => Printf.printf("Log.Entry.import: Deserialization error")
    }
  );

let update = (action: Page.Update.t, result: Updated.t('a)): unit =>
  if (result.logged) {
    Entry.save(Entry.mk(action));
  };

let get_and = (f: string => unit): unit =>
  DB.get_all(entries => f("(" ++ String.concat(" ", entries) ++ ")"));
