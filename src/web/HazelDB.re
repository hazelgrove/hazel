/* Single shared IndexedDB database for all Hazel persistence.
   Two object stores:
   - "log": append-only action log (used by Log.re)
   - "kv": key-value store for all app state (settings, editors, agents, etc.)

   All modules that need persistence go through this single database
   to avoid version-coordination issues between independent databases. */

open Ezjs_idb;

module IDBStore = Ezjs_idb.Store(StringTr, StringTr);

type db = Ezjs_min.t(Types.iDBDatabase);

let db_name = "hazel";
let log_table = "log";
let kv_table = "kv";

let log_store = (db: db): IDBStore.store =>
  IDBStore.store(~mode=READWRITE, db, log_table);

let kv_store = (db: db): IDBStore.store =>
  IDBStore.store(~mode=READWRITE, db, kv_table);

let with_db = (f): unit => {
  let error = _: unit => print_endline("ERROR: HazelDB.open");
  let upgrade = (db: db, e: db_upgrade): unit =>
    if (e.new_version >= 1 && e.old_version == 0) {
      ignore(IDBStore.create(db, log_table));
      ignore(IDBStore.create(db, kv_table));
    };
  openDB(~upgrade, ~error, ~version=1, db_name, db => f(db));
};

/* === KV table operations === */

let kv_save = (key: string, value: string): unit =>
  with_db(db => IDBStore.put(~key, ~callback=_ => (), kv_store(db), value));

let kv_load = (key: string, callback: option(string) => unit): unit =>
  with_db(db => {
    let error = _ => print_endline("ERROR: HazelDB.kv_load(" ++ key ++ ")");
    IDBStore.get(~error, kv_store(db), callback, K(key));
  });

let kv_delete = (key: string): unit =>
  with_db(db =>
    IDBStore.delete(~error=_ => (), ~callback=_ => (), kv_store(db), K(key))
  );

let kv_clear = (~callback=() => (), ()): unit => {
  let error = _ => print_endline("ERROR: HazelDB.kv_clear");
  with_db(db => IDBStore.clear(~error, ~callback, kv_store(db)));
};

/* Load all KV entries at once via cursor fold. Returns list of (key, value)
   pairs. Used during startup to populate the model from a single IDB read. */
let kv_load_all = (callback: list((string, string)) => unit): unit =>
  with_db(db => {
    let error = _ => print_endline("ERROR: HazelDB.kv_load_all");
    IDBStore.fold(
      ~error,
      kv_store(db),
      (key, value, acc) => [(key, value), ...acc],
      [],
      callback,
    );
  });

/* === Log table operations === */

let log_add = (key: string, value: string): unit =>
  with_db(db => IDBStore.add(~key, ~callback=_ => (), log_store(db), value));

let log_get = (key: string, f: option(string) => unit): unit => {
  let error = _ => print_endline("ERROR: HazelDB.log_get");
  with_db(db => IDBStore.get(~error, log_store(db), f, K(key)));
};

let log_get_all = (f: list(string) => unit): unit => {
  let error = _ => print_endline("ERROR: HazelDB.log_get_all");
  with_db(db => IDBStore.get_all(~error, log_store(db), f));
};

let log_clear = (~callback=() => (), ()): unit => {
  let error = _ => print_endline("ERROR: HazelDB.log_clear");
  with_db(db => IDBStore.clear(~error, ~callback, log_store(db)));
};

/* === In-memory cache ===

   Populated at startup from kv_load_all, then kept in sync on every
   kv_save. This lets synchronous code (Store.F.load, Store.F.export)
   read the latest saved state without an async IndexedDB round-trip. */

let cache: ref(Util.Maps.StringMap.t(string)) =
  ref(Util.Maps.StringMap.empty);

let init_cache = (pairs: list((string, string))): unit =>
  cache :=
    List.fold_left(
      (m, (k, v)) => Util.Maps.StringMap.add(k, v, m),
      Util.Maps.StringMap.empty,
      pairs,
    );

let get_cache = (key: string): option(string) =>
  Util.Maps.StringMap.find_opt(key, cache^);

let update_cache = (key: string, value: string): unit =>
  cache := Util.Maps.StringMap.add(key, value, cache^);

let remove_cache = (key: string): unit =>
  cache := Util.Maps.StringMap.remove(key, cache^);

/* === Database-level operations === */

/* Clear legacy localStorage data. Used during "Reset Hazel" to ensure
   no stale pre-migration data remains. Safe to remove once all users
   have upgraded to IndexedDB storage. */
let clear_legacy_storage = (): unit => {
  let local_store =
    Js_of_ocaml.Dom_html.window##.localStorage
    |> Js_of_ocaml.Js.Optdef.get(_, () => assert(false));
  local_store##clear;
};

/* Clear all data from all tables. Used by "Reset Hazel". */
let clear_all = (~callback=() => (), ()): unit => {
  let remaining = ref(2);
  let on_done = () => {
    decr(remaining);
    if (remaining^ == 0) {
      callback();
    };
  };
  kv_clear(~callback=on_done, ());
  log_clear(~callback=on_done, ());
};
