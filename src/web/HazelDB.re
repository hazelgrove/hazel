/* Single shared IndexedDB database for all Hazel persistence.
   Two object stores:
   - "log": append-only action log (used by Log.re)
   - "kv": key-value store for all app state (settings, editors, agents, etc.)

   All modules that need persistence go through this single database
   to avoid version-coordination issues between independent databases.

   KV operations maintain an in-memory cache so that reads are synchronous.
   The cache is populated at startup via kv_load_all, then kept in sync
   by kv_save/kv_delete/kv_clear. Callers never interact with the cache
   directly — they just use kv_save/kv_get/kv_delete. */

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

let with_db = (~error=_ => print_endline("ERROR: HazelDB.open"), f): unit => {
  let upgrade = (db: db, e: db_upgrade): unit =>
    if (e.new_version >= 1 && e.old_version == 0) {
      ignore(IDBStore.create(db, log_table));
      ignore(IDBStore.create(db, kv_table));
    };
  openDB(~upgrade, ~error, ~version=1, db_name, db => f(db));
};

/* === In-memory cache (private) === */

let cache: ref(Util.Maps.StringMap.t(string)) =
  ref(Util.Maps.StringMap.empty);

/* === KV operations === */

let kv_save = (key: string, value: string): unit => {
  cache := Util.Maps.StringMap.add(key, value, cache^);
  with_db(db => IDBStore.put(~key, ~callback=_ => (), kv_store(db), value));
};

let kv_get = (key: string): option(string) =>
  Util.Maps.StringMap.find_opt(key, cache^);

let kv_remove = (key: string): unit => {
  cache := Util.Maps.StringMap.remove(key, cache^);
  with_db(db =>
    IDBStore.delete(~callback=_ => (), kv_store(db), IDBStore.K(key))
  );
};
let kv_delete = kv_remove;

let kv_clear = (~callback=() => (), ()): unit => {
  cache := Util.Maps.StringMap.empty;
  let error = _ => print_endline("ERROR: HazelDB.kv_clear");
  with_db(db => IDBStore.clear(~error, ~callback, kv_store(db)));
};

/* Load all KV entries at once via cursor fold. Populates the cache
   and returns the pairs to the callback. Called once at startup. */
let kv_load_all = (callback: list((string, string)) => unit): unit =>
  with_db(db => {
    let error = _ => print_endline("ERROR: HazelDB.kv_load_all");
    IDBStore.fold(
      ~error,
      kv_store(db),
      (key, value, acc) => [(key, value), ...acc],
      [],
      pairs => {
        cache :=
          List.fold_left(
            (m, (k, v)) => Util.Maps.StringMap.add(k, v, m),
            Util.Maps.StringMap.empty,
            pairs,
          );
        callback(pairs);
      },
    );
  });

/* Complete a single transaction containing current editor data before leaving
   the page for authorization. The cache includes the just-requested save. */
let flush = (~callback: bool => unit, ()): unit => {
  let finished = ref(false);
  let finish = ok =>
    if (! finished^) {
      finished := true;
      callback(ok);
    };
  with_db(
    ~error=_ => finish(false),
    db => {
      let store = kv_store(db);
      let transaction = store##.transaction;
      transaction##.oncomplete :=
        Ezjs_min.AOpt.option(
          Some(Ezjs_min.wrap_callback(_ => finish(true))),
        );
      transaction##.onabort :=
        Ezjs_min.AOpt.option(
          Some(Ezjs_min.wrap_callback(_ => finish(false))),
        );
      Util.Maps.StringMap.iter(
        (key, value) =>
          IDBStore.put(~key, ~error=_ => finish(false), store, value),
        cache^,
      );
    },
  );
};

/* === Log operations === */

let log_add = (key: string, value: string): unit =>
  with_db(db => IDBStore.add(~key, ~callback=_ => (), log_store(db), value));

let log_get_all = (f: list(string) => unit): unit => {
  let error = _ => print_endline("ERROR: HazelDB.log_get_all");
  with_db(db => IDBStore.get_all(~error, log_store(db), f));
};

let log_clear = (~callback=() => (), ()): unit => {
  let error = _ => print_endline("ERROR: HazelDB.log_clear");
  with_db(db => IDBStore.clear(~error, ~callback, log_store(db)));
};

/* === Database-level operations === */

/* Reset editor state while preserving the dedicated browser credential.
   Credentials are removed only through the agent's settings. */
let clear_all = (~callback=() => (), ()): unit => {
  AgentAuth.clear_editor_storage();
  cache := Util.Maps.StringMap.empty;
  let remaining = ref(2);
  let on_done = () => {
    decr(remaining);
    if (remaining^ == 0) {
      callback();
    };
  };
  with_db(db =>
    List.iter(
      make_store => {
        let store = make_store(db);
        let transaction = store##.transaction;
        transaction##.oncomplete :=
          Ezjs_min.AOpt.option(Some(Ezjs_min.wrap_callback(_ => on_done())));
        IDBStore.clear(
          ~error=_ => print_endline("ERROR: HazelDB.clear_all"),
          store,
        );
      },
      [kv_store, log_store],
    )
  );
};
