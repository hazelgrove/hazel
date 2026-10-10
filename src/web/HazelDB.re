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

/* === In-memory cache (private) === */

let cache: ref(Util.Maps.StringMap.t(string)) =
  ref(Util.Maps.StringMap.empty);

/* Set by clear_all, which every caller follows with a reload: a later
   save (the autosave alarm, the page-leave flush) would undo the reset. */
let saves_suspended = ref(false);

/* === Unload journal ===
   Chrome drops IndexedDB writes made while the page unloads, so the
   page-leave flush (kv_journal) writes the entries that changed since the
   last save to localStorage, which is synchronous; the next load moves
   them into the database. A normal save of a key drops its entry. */

let journal_prefix = "hazel-unsaved:";
let journaling = ref(false);
let journaled: ref(Util.Maps.StringMap.t(unit)) =
  ref(Util.Maps.StringMap.empty);

let local_storage = () =>
  Js_of_ocaml.Js.Optdef.to_option(Js_of_ocaml.Dom_html.window##.localStorage);

let journal_set = (key: string, value: string): unit =>
  switch (local_storage()) {
  | Some(ls) =>
    try(
      {
        ls##setItem(
          Js_of_ocaml.Js.string(journal_prefix ++ key),
          Js_of_ocaml.Js.string(value),
        );
        journaled := Util.Maps.StringMap.add(key, (), journaled^);
      }
    ) {
    | _ => () /* over quota: best effort */
    }
  | None => ()
  };

let journal_drop = (key: string): unit =>
  if (Util.Maps.StringMap.mem(key, journaled^)) {
    journaled := Util.Maps.StringMap.remove(key, journaled^);
    Option.iter(
      ls => ls##removeItem(Js_of_ocaml.Js.string(journal_prefix ++ key)),
      local_storage(),
    );
  };

/* === KV operations === */

let kv_save = (key: string, value: string): unit =>
  if (saves_suspended^) {
    ();
  } else if (journaling^) {
    if (Util.Maps.StringMap.find_opt(key, cache^) != Some(value)) {
      journal_set(key, value);
    };
  } else {
    cache := Util.Maps.StringMap.add(key, value, cache^);
    journal_drop(key);
    with_db(db =>
      IDBStore.put(~key, ~callback=_ => (), kv_store(db), value)
    );
  };

/* Runs `save` (which calls kv_save) with its writes going to the journal */
let kv_journal = (save: unit => unit): unit => {
  journaling := true;
  Fun.protect(save, ~finally=() => journaling := false);
};

/* Moves a previous page's journal into the cache and the database */
let recover_journal = (): unit =>
  switch (local_storage()) {
  | None => ()
  | Some(ls) =>
    let n = String.length(journal_prefix);
    List.init(ls##.length, i => Js_of_ocaml.Js.Opt.to_option(ls##key(i)))
    |> List.filter_map(Option.map(Js_of_ocaml.Js.to_string))
    |> List.filter(String.starts_with(~prefix=journal_prefix))
    |> List.iter(k => {
         let jk = Js_of_ocaml.Js.string(k);
         Js_of_ocaml.Js.Opt.iter(ls##getItem(jk), v =>
           kv_save(
             String.sub(k, n, String.length(k) - n),
             Js_of_ocaml.Js.to_string(v),
           )
         );
         ls##removeItem(jk);
       });
  };

let kv_get = (key: string): option(string) =>
  Util.Maps.StringMap.find_opt(key, cache^);

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
        recover_journal();
        callback(pairs);
      },
    );
  });

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
  saves_suspended := true;
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
