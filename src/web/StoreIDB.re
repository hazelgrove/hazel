/* IndexedDB-backed key-value store.
   Async counterpart to Store.F (which uses localStorage).
   Each store instance gets its own IndexedDB database to avoid
   version-coordination issues between independent stores. */

module F =
       (
         STORE_KIND: {
           [@deriving (show({with_path: false}), sexp, yojson)]
           type t;
           let default: unit => t;
           let db_name: string;
         },
       ) => {
  include STORE_KIND;

  open Ezjs_idb;
  module IDBStore = Ezjs_idb.Store(StringTr, StringTr);

  type db = Ezjs_min.t(Types.iDBDatabase);

  let table_name = "kv";

  let kv_store = (db: db): IDBStore.store =>
    IDBStore.store(~mode=READWRITE, db, table_name);

  let with_db = (f): unit => {
    let error = _: unit =>
      print_endline("ERROR: StoreIDB(" ++ db_name ++ ").open");
    let upgrade = (db: db, e: db_upgrade): unit =>
      e.new_version >= 1 && e.old_version == 0
        ? ignore(IDBStore.create(db, table_name)) : ();
    openDB(~upgrade, ~error, ~version=1, db_name, db => f(db));
  };

  let serialize = (data: t): string =>
    data |> sexp_of_t |> Sexplib.Sexp.to_string;

  let deserialize = (data: string): option(t) =>
    try(Some(data |> Sexplib.Sexp.of_string |> t_of_sexp)) {
    | _ =>
      print_endline("StoreIDB(" ++ db_name ++ "): deserialization error");
      None;
    };

  let save = (key: string, data: t): unit =>
    with_db(db =>
      IDBStore.put(~key, ~callback=_ => (), kv_store(db), serialize(data))
    );

  let load = (key: string, callback: option(t) => unit): unit =>
    with_db(db => {
      let error = _ =>
        print_endline("ERROR: StoreIDB(" ++ db_name ++ ").get");
      IDBStore.get(
        ~error,
        kv_store(db),
        fun
        | None => callback(None)
        | Some(data) => callback(deserialize(data)),
        K(key),
      );
    });

  let delete = (key: string): unit =>
    with_db(db =>
      IDBStore.delete(
        ~error=_ => (),
        ~callback=_ => (),
        kv_store(db),
        K(key),
      )
    );

  let clear = (~callback=() => (), ()): unit => {
    let error = _ =>
      print_endline("ERROR: StoreIDB(" ++ db_name ++ ").clear");
    with_db(db => IDBStore.clear(~error, ~callback, kv_store(db)));
  };
};
