open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type all = {
  editors: string,
  settings: string,
  langDocMessages: string,
  log: string,
};

let mk_all = (~instructor_mode as _) => {
  //TODO(andrew): instructor_mode
  print_endline("Mk all");
  let settings = Store.Settings.export();
  print_endline("Settings OK");
  let langDocMessages = Store.LangDocMessages.export();
  print_endline("LangDocMessages OK");
  let editors = Store.Editors.export();
  print_endline("Editors OK");
  let log = Log.export();
  {settings, langDocMessages, editors, log};
};

let export_all = (~instructor_mode) => {
  mk_all(~instructor_mode) |> yojson_of_all;
};

let import_all = (data, ~specs as _) => {
  let all = data |> Yojson.Safe.from_string |> all_of_yojson;
  let settings = Store.Settings.import(all.settings);
  let _ = Store.LangDocMessages.import(all.langDocMessages);
  let _instructor_mode = settings.instructor_mode; //TODO(andrew)
  ignore(Store.Editors.import(all.editors));
  Log.import(all.log);
};
