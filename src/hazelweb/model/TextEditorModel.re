open Sexplib.Std;

type t = {
  active: bool,
  valid_text: bool,
  current_text: string,
  error_text: string,
};

[@deriving sexp]
type update =
  | OpenEditor(Program.t)
  | CloseEditor
  | ToggleValid
  | ClearError
  | SetCurrentText(string)
  | SetErrorText(string);

let init = {
  active: false,
  valid_text: true,
  current_text: "",
  error_text: "",
};

let toggle_valid = m => {...m, valid_text: !m.valid_text};

let extract_program_string = prog => {
  let lay = Program.get_layout(~settings=Settings.init, prog);
  Hazeltext.Print.string_of_layout(lay);
};

let apply_update = (update, t) => {
  switch (update) {
  | OpenEditor(p) => {
      active: true,
      valid_text: true,
      current_text: extract_program_string(p),
      error_text: "",
    }
  | CloseEditor => {...t, active: false, valid_text: true}
  | ToggleValid => toggle_valid(t)
  | ClearError => {...t, valid_text: true, error_text: ""}
  | SetCurrentText(s) => {...t, current_text: s}
  | SetErrorText(s) => {...t, valid_text: false, error_text: s}
  };
};
