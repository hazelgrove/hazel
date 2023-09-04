/*
   Logging system for actions. Persists log in local storage.

   Careful: local storage has a maximum size of around 5MB in most browsers. Once
   this limit is exceeded, remaining entries are printed to console rather than
   persisted.
 */

open Sexplib.Std;
open Ezjs_min;
open Ezjs_idb;
open Types;

module Store = Store(StringTr, StringTr);

let test_db = "test_db";
let test_table = "test_table";

let upgrade = (db: t(iDBDatabase), e) =>
  if (e.new_version >= 1 && e.old_version == 0) {
    ignore @@ Store.create(db, test_table);
  };

let error = _e => print_endline("error: ");

let add = db => {
  let st = Store.store(~mode=READWRITE, db, test_table);
  let key = "test-key";
  let value = "test-value";
  Store.add(
    ~key,
    ~callback=key => print_endline("added key:" ++ key),
    st,
    value,
  );
};

let get = db => {
  let st = Store.store(~mode=READWRITE, db, test_table);
  let key = "test-key";
  Store.get_key(
    ~error,
    st,
    v =>
      switch (v) {
      | None => print_endline("key not found")
      | Some(v) => print_endline("value:" ++ v)
      },
    K(key),
  );
};

let doo = {
  print_endline("adding to db");
  openDB(~upgrade, ~error, ~version=1, test_db, db => add(db));
  print_endline("getting from db");
  openDB(~upgrade, ~error, ~version=1, test_db, db => get(db));
};

let is_action_logged: UpdateAction.t => bool =
  fun
  | Mousedown
  | Mouseup
  | Save
  | SetFontMetrics(_)
  | SetLogoFontMetrics(_)
  | SetShowBackpackTargets(_)
  | InitImportAll(_)
  | InitImportScratchpad(_)
  | UpdateResult(_)
  | DebugAction(_)
  | ExportPersistentData
  | Benchmark(_) => false
  | ReparseCurrentEditor
  | Set(_)
  | FinishImportAll(_)
  | FinishImportScratchpad(_)
  | ResetCurrentEditor
  | SetMode(_)
  | SwitchScratchSlide(_)
  | SwitchExampleSlide(_)
  | SwitchEditor(_)
  | PerformAction(_)
  | Cut
  | Copy
  | Paste(_)
  | Undo
  | Redo
  | MoveToNextHole(_)
  | UpdateLangDocMessages(_) => true;

let storage_key = "LOG_" ++ ExerciseSettings.log_key;
let max_log_string_length = 4_750_000; // based on 5MB limit on localstore in browser

module Entry = {
  [@deriving (show({with_path: false}), yojson, sexp)]
  type t = (Model.timestamp, UpdateAction.t);

  let mk = (update): t => {
    (JsUtil.timestamp(), update);
  };

  let to_string = ((timestamp, update): t) => {
    /*let status =
      switch (entry.error) {
      | None => "SUCCESS"
      | Some(failure) => "FAILURE(" ++ UpdateAction.Failure.show(failure) ++ ")"
      };*/
    Printf.sprintf(
      "%.0f: %s",
      timestamp,
      UpdateAction.show(update),
      //status,
    );
  };

  let serialize = (entry: t): string => {
    entry |> sexp_of_t |> Sexplib.Sexp.to_string;
  };

  let deserialize = (s: string): t => {
    s |> Sexplib.Sexp.of_string |> t_of_sexp;
  };
};

let init_log = () => {
  JsUtil.set_localstore(storage_key, "");
};

let rec get_log_string = () => {
  switch (JsUtil.get_localstore(storage_key)) {
  | Some(log) => log
  | None =>
    init_log();
    get_log_string();
  };
};

let append_entry = (entry: Entry.t) => {
  let log_string = get_log_string();
  let entry_string = Entry.serialize(entry);
  let new_log_string = log_string ++ entry_string;
  if (String.length(new_log_string) >= max_log_string_length) {
    print_endline("Log limit exceeded. Printing new entries to console.");
    print_endline("Log entry: " ++ entry_string);
  } else {
    JsUtil.set_localstore(storage_key, new_log_string);
  };
};

[@deriving sexp]
type entries = list(Entry.t);

let logstring_to_entries = (s: string) => {
  // make the adjacent entries into a single list and deserialize as an sexp
  "(" ++ s ++ ")" |> Sexplib.Sexp.of_string |> entries_of_sexp;
};

let entries_to_logstring = (entries: entries): string => {
  let s = entries |> sexp_of_entries |> Sexplib.Sexp.to_string;
  // chop off leading and trailing parens
  String.sub(s, 1, String.length(s) - 2);
};

let export = () => {
  get_log_string();
};

let import = data => {
  JsUtil.set_localstore(storage_key, data);
};

let update = (action: UpdateAction.t) =>
  if (is_action_logged(action)) {
    let new_entry = Entry.mk(action);
    append_entry(new_entry);
  };
