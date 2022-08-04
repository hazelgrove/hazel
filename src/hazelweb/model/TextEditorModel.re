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

let toggle_valid = (te_model: t) => {
  ...te_model,
  valid_text: !te_model.valid_text,
};

let extract_program_string = (prog: Program.t) => {
  let lay = Program.get_layout(~settings=Settings.init, prog);
  Hazeltext.Print.string_of_layout(lay);
};

let apply_update = (u: update, te_model: t) => {
  switch (u) {
  | OpenEditor(p) => {
      active: true,
      valid_text: true,
      current_text: extract_program_string(p),
      error_text: "",
    }
  | CloseEditor => {...te_model, active: false, valid_text: true}
  | ToggleValid => toggle_valid(te_model)
  | ClearError => {...te_model, valid_text: true, error_text: ""}
  | SetCurrentText(s) => {...te_model, current_text: s}
  | SetErrorText(s) => {...te_model, valid_text: false, error_text: s}
  };
};

let line_count = (te_model: t) => {
  String.split_on_char('\n', te_model.current_text)
  |> List.length
  |> (x => x + 0);
};
