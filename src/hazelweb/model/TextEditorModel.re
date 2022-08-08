open Sexplib.Std;

type t = {
  active: bool,
  current_text: option(string),
  error: option(string),
};

[@deriving sexp]
type update =
  | OpenEditor(Program.t)
  | CloseEditor
  | ClearError
  | SetCurrentText(string)
  | SetError(string);

let init = {active: false, current_text: None, error: None};

let extract_program_string = (prog: Program.t) => {
  let lay = Program.get_layout(~settings=Settings.init, prog);
  Hazeltext.Print.string_of_layout(lay);
};

let apply_update = (u: update, te_model: t) => {
  switch (u) {
  | OpenEditor(p) => {
      active: true,
      current_text: Some(extract_program_string(p)),
      error: None,
    }
  | CloseEditor => {...te_model, current_text: None, active: false}
  | ClearError => {...te_model, error: None}
  | SetCurrentText(s) => {...te_model, current_text: Some(s)}
  | SetError(s) => {...te_model, error: Some(s)}
  };
};

let is_valid = (te_model: t) => Option.is_none(te_model.error);

let line_count = (te_model: t) => {
  {
    switch (te_model.current_text) {
    | Some(s) => String.split_on_char('\n', s) |> List.length
    | None => 0
    };
  }
  |> (x => x + 0);
};

let get_error_string = (te_model: t) => {
  Option.value(te_model.error, ~default="");
};

let get_current_text = (te_model: t) => {
  Option.get(te_model.current_text);
};
