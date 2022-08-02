let debug_update = ref(false);
let debug_zipper = ref(false);
let debug_keystoke = ref(false);

[@deriving (show({with_path: false}), yojson)]
type entry = {
  update: Update.t,
  error: option(Update.Failure.t),
  timestamp: Model.timestamp,
  zipper: Printer.t,
};

[@deriving (show({with_path: false}), yojson)]
type updates = list(entry);

let mut_log: ref(updates) = ref([]);

let is_action_logged: Update.t => bool =
  fun
  | UpdateDoubleTap(_) => false
  | Save => false
  | SetFontMetrics(_) => false
  | _ => true;

let is_keystroke_logged: Key.t => bool = _ => true;

let mk_entry = (update, z, error): entry => {
  let error =
    switch (error) {
    | Ok(_) => None
    | Error(failure) => Some(failure)
    };
  {
    zipper: Printer.of_zipper(z),
    update,
    error,
    timestamp: JsUtil.timestamp(),
  };
};

let to_string = (entry: entry) => {
  let status =
    switch (entry.error) {
    | None => "SUCCESS"
    | Some(failure) => "FAILURE(" ++ Update.Failure.show(failure) ++ ")"
    };
  Printf.sprintf(
    "%.0f: %s %s",
    entry.timestamp,
    Update.show(entry.update),
    status,
  );
};

[@deriving (show({with_path: false}), yojson)]
type key_entry = {
  key: Key.t,
  updates: list(Update.t),
  timestamp: Model.timestamp,
};

let mk_key_entry = (key, updates): key_entry => {
  {key, updates, timestamp: JsUtil.timestamp()};
};

let key_entry_to_string = ({key, updates, timestamp}: key_entry) => {
  let updates = updates |> List.map(Update.show) |> String.concat(", ");
  Printf.sprintf("%.0f: %s -> [%s]", timestamp, Key.to_string(key), updates);
};

let updates_of_string: string => updates =
  str =>
    try(
      switch (str |> Yojson.Safe.from_string |> updates_of_yojson) {
      | Ok(updates) => updates
      | Error(err) =>
        print_endline("log: json decoding (1) read error: " ++ err ++ "\n");
        [];
      }
    ) {
    | exc =>
      print_endline(
        "log: json decoding (2) exception: "
        ++ Printexc.to_string(exc)
        ++ "\n",
      );
      [];
    };
let json_update_log_key = "JSON_UPDATE_LOG";
let get_json_update_log = () =>
  switch (LocalStorage.get_localstore(json_update_log_key)) {
  | None => []
  | Some(str) => updates_of_string(str)
  };

let get_json_update_log_string = () =>
  get_json_update_log() |> updates_to_yojson |> Yojson.Safe.to_string;

let reset_json_log = () => {
  mut_log := [];
  LocalStorage.set_localstore(json_update_log_key, "");
};

let append_json_updates_log = () => {
  let new_updates = mut_log^;
  mut_log := [];
  let old_log = get_json_update_log();
  let new_log = new_updates @ old_log;
  let blah = Yojson.Safe.to_string(updates_to_yojson(new_log));
  LocalStorage.set_localstore(json_update_log_key, blah);
};

let update = (update: Update.t, old_model: Model.t, res) => {
  if (is_action_logged(update)) {
    let cur_model =
      switch (res) {
      | Ok(model) => model
      | Error(_) => old_model
      };
    let zip = cur_model |> Model.get_zipper;
    let new_entry = mk_entry(update, zip, res);
    mut_log := List.cons(new_entry, mut_log^);
    if (debug_update^) {
      let update_str = to_string(mk_entry(update, zip, res));
      print_endline(update_str);
      //new_entry |> entry_to_yojson |> Yojson.Safe.to_string |> print_endline;
    };
    if (debug_zipper^) {
      cur_model |> Model.get_zipper |> Printer.to_string |> print_endline;
    };
  };
  res;
};

let keystoke = (key: Key.t, updates) => {
  if (is_keystroke_logged(key)) {
    if (debug_keystoke^) {
      let keystroke_str = key_entry_to_string(mk_key_entry(key, updates));
      print_endline(keystroke_str);
    };
  };
  updates;
};
