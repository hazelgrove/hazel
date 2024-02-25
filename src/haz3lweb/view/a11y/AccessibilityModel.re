open Sexplib.Std;
module Sexp = Sexplib.Sexp;
open Haz3lcore;

module Settings = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    enable: bool,
    is_editing: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleEnable
    | ToggleIsEditing;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  input: string,
  query_result: option(string),
};

let init = {input: "", query_result: None};

let remove_last_char = (str: string) =>
  if (String.length(str) > 0) {
    String.sub(str, 0, String.length(str) - 1);
  } else {
    str;
  };

let perform_action = (model: t, action: Action.t) => {
  switch (action) {
  | Action.Insert(s) => {...model, input: model.input ++ s}
  | Action.Destruct(Left) => {
      ...model,
      input: remove_last_char(model.input),
    }
  | _ => model
  };
};

type update =
  | Edit(Action.t);

let update = (model: t, update: update): t =>
  switch (update) {
  | Edit(action) => perform_action(model, action)
  };
