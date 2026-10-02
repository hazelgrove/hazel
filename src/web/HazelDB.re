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

let with_db = (f): unit => {
  let error = _: unit => print_endline("ERROR: HazelDB.open");
  let upgrade = (db: db, e: db_upgrade): unit =>
    if (e.new_version >= 1 && e.old_version == 0) {
      ignore(IDBStore.create(db, log_table));
      ignore(IDBStore.create(db, kv_table));
    };
  openDB(~upgrade, ~error, ~version=1, db_name, db => f(db));
};

/* === Backend canister (optional) ===

   When the page sets window.hazelBackend (config.js; a canister deploy
   writes it), the tables live in that canister instead of IndexedDB, and
   these operations reach it over HTTP through ic-backend.js. The cache and
   every caller stay as they are: reads come from the cache, filled at
   startup from GET /kv, and writes go through without waiting. Stage 1 of
   the canister demo: a failed write is logged, not retried, and two tabs
   writing at once means the last write wins. */
module Backend = {
  open Js_of_ocaml;

  /* A string, or null when config.js is missing or leaves it unset. */
  let address: option(string) = {
    let a: Js.opt(Js.t(Js.js_string)) =
      Js.Unsafe.js_expr(
        "(typeof window !== 'undefined' && typeof window.hazelBackend === 'string') ? window.hazelBackend : null",
      );
    Js.Opt.to_option(a) |> Option.map(Js.to_string);
  };

  let on = Option.is_some(address);

  let call =
      (method: string, path: string, body: option(string), k: string => unit)
      : unit =>
    Js.Unsafe.fun_call(
      Js.Unsafe.get(Js.Unsafe.global, "hazelBackendCall"),
      [|
        Js.Unsafe.inject(Js.string(method)),
        Js.Unsafe.inject(Js.string(path)),
        switch (body) {
        | Some(b) => Js.Unsafe.inject(Js.string(b))
        | None => Js.Unsafe.inject(Js.null)
        },
        Js.Unsafe.inject(Js.wrap_callback(t => k(Js.to_string(t)))),
      |],
    );
};

/* === In-memory cache (private) === */

let cache: ref(Util.Maps.StringMap.t(string)) =
  ref(Util.Maps.StringMap.empty);

/* === KV operations === */

type write =
  | Put(string, string)
  | Delete(string);

/* writes queued by kv_batch, newest first */
let batched: ref(option(list(write))) = ref(None);
let write_transactions = ref(0); /* observability for tests */

/* one transaction for all of [writes]: it commits whole or not at all */
let writes_json = (writes: list(write)): string =>
  Yojson.Safe.to_string(
    `List(
      List.map(
        fun
        | Put(key, value) =>
          `Assoc([
            ("op", `String("put")),
            ("key", `String(key)),
            ("value", `String(value)),
          ])
        | Delete(key) =>
          `Assoc([("op", `String("remove")), ("key", `String(key))]),
        writes,
      ),
    ),
  );

let commit = (writes: list(write)): unit =>
  if (writes != [] && Backend.on) {
    incr(write_transactions);
    Backend.call("POST", "/kv", Some(writes_json(writes)), _ => ());
  } else if (writes != []) {
    incr(write_transactions);
    with_db(db => {
      let store = kv_store(db);
      List.iter(
        fun
        | Put(key, value) =>
          IDBStore.put(~key, ~callback=_ => (), store, value)
        | Delete(key) =>
          IDBStore.delete(~callback=_ => (), store, IDBStore.K(key)),
        writes,
      );
    });
  };

let write = (w: write): unit =>
  switch (batched^) {
  | Some(ws) => batched := Some([w, ...ws])
  | None => commit([w])
  };

/* [f]'s writes land together, so an interrupted save can't leave half
   of them (a rename's new definition beside its old use). nested calls
   join the outer batch */
let kv_batch = (f: unit => 'a): 'a =>
  switch (batched^) {
  | Some(_) => f()
  | None =>
    batched := Some([]);
    Fun.protect(
      ~finally=
        () => {
          let ws = Option.value(batched^, ~default=[]);
          batched := None;
          commit(List.rev(ws));
        },
      f,
    );
  };

let kv_save = (key: string, value: string): unit => {
  cache := Util.Maps.StringMap.add(key, value, cache^);
  write(Put(key, value));
};

let kv_get = (key: string): option(string) =>
  Util.Maps.StringMap.find_opt(key, cache^);

let kv_remove = (key: string): unit => {
  cache := Util.Maps.StringMap.remove(key, cache^);
  write(Delete(key));
};

/* every stored key [owned] claims */
let kv_remove_where = (owned: string => bool): unit =>
  kv_batch(() =>
    Util.Maps.StringMap.iter(
      (k, _) =>
        if (owned(k)) {
          kv_remove(k);
        },
      cache^,
    )
  );

/* every stored key [rekey] maps to a different key, saved there instead */
let kv_rekey = (rekey: string => option(string)): unit =>
  kv_batch(() =>
    Util.Maps.StringMap.iter(
      (k, v) =>
        switch (rekey(k)) {
        | Some(k') when k' != k =>
          kv_save(k', v);
          kv_remove(k);
        | _ => ()
        },
      cache^,
    )
  );

let kv_clear = (~callback=() => (), ()): unit => {
  cache := Util.Maps.StringMap.empty;
  if (Backend.on) {
    Backend.call("POST", "/kv/clear", None, _ => callback());
  } else {
    let error = _ => print_endline("ERROR: HazelDB.kv_clear");
    with_db(db => IDBStore.clear(~error, ~callback, kv_store(db)));
  };
};

/* Load all KV entries at once via cursor fold. Populates the cache
   and returns the pairs to the callback. Called once at startup. */
let fill_cache = (pairs: list((string, string))): unit =>
  cache :=
    List.fold_left(
      (m, (k, v)) => Util.Maps.StringMap.add(k, v, m),
      Util.Maps.StringMap.empty,
      pairs,
    );

let rec kv_load_all = (callback: list((string, string)) => unit): unit =>
  if (Backend.on) {
    Backend.call(
      "GET",
      "/kv",
      None,
      text => {
        let pairs =
          switch (Yojson.Safe.from_string(text)) {
          | `Assoc(fields) =>
            List.filter_map(
              fun
              | (k, `String(v)) => Some((k, v))
              | _ => None,
              fields,
            )
          | _ => []
          | exception _ =>
            print_endline("ERROR: HazelDB.kv_load_all: backend sent no JSON");
            [];
          };
        fill_cache(pairs);
        callback(pairs);
      },
    );
  } else {
    kv_load_all_idb(callback);
  }
and kv_load_all_idb = (callback: list((string, string)) => unit): unit =>
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

/* === Log operations === */

let log_add = (key: string, value: string): unit =>
  if (Backend.on) {
    Backend.call(
      "POST",
      "/log",
      Some(
        Yojson.Safe.to_string(
          `Assoc([("key", `String(key)), ("value", `String(value))]),
        ),
      ),
      _ =>
      ()
    );
  } else {
    with_db(db =>
      IDBStore.add(~key, ~callback=_ => (), log_store(db), value)
    );
  };

let log_get_all = (f: list(string) => unit): unit =>
  if (Backend.on) {
    Backend.call("GET", "/log", None, text =>
      f(
        switch (Yojson.Safe.from_string(text)) {
        | `List(items) =>
          List.filter_map(
            fun
            | `String(v) => Some(v)
            | _ => None,
            items,
          )
        | _ => []
        | exception _ => []
        },
      )
    );
  } else {
    let error = _ => print_endline("ERROR: HazelDB.log_get_all");
    with_db(db => IDBStore.get_all(~error, log_store(db), f));
  };

let log_clear = (~callback=() => (), ()): unit =>
  if (Backend.on) {
    Backend.call("POST", "/log/clear", None, _ => callback());
  } else {
    let error = _ => print_endline("ERROR: HazelDB.log_clear");
    with_db(db => IDBStore.clear(~error, ~callback, log_store(db)));
  };

/* === Database-level operations === */

/* Clear all data from all tables and legacy localStorage.
   Used by "Reset Hazel". */
let clear_all = (~callback=() => (), ()): unit => {
  /* Clear legacy localStorage (safe to remove once all users upgraded) */
  try({
    let local_store =
      Js_of_ocaml.Dom_html.window##.localStorage
      |> Js_of_ocaml.Js.Optdef.get(_, () => assert(false));
    local_store##clear;
  }) {
  | _ => ()
  };
  cache := Util.Maps.StringMap.empty;
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
