open Js_of_ocaml;
module Result = Stdlib.Result;

exception EmptyCurrent;

type t = Url.url;

let string_of_model = (model: Model.t): string =>
  model |> Model.get_program |> Program.get_uhexp |> Hazeltext.Print.print_exp;

let model_of_string = (~initial_model: Model.t, str: string): Model.t => {
  let e = str |> Hazeltext.Parsing.ast_of_string |> Result.to_option;
  let ze = e |> Option.map(ZExp.place_before);
  let edit_state =
    ze |> Option.map(Statics_Exp.fix_and_renumber_holes_z(Contexts.initial));
  let program =
    edit_state |> Option.map(Program.mk(~width=initial_model.cell_width));

  program
  |> Option.map(program => Model.put_program(program, initial_model))
  |> Option.value(~default=initial_model);
};

let put_model = (url: t, model: Model.t): t => {
  let str = model |> string_of_model |> Url.urlencode;

  switch (url) {
  | Http(url) => Http({...url, hu_fragment: str})
  | Https(url) => Https({...url, hu_fragment: str})
  | File(url) => File({...url, fu_fragment: str})
  };
};

let get_model = (~initial_model: Model.t, url: t): Model.t => {
  let str =
    switch (url) {
    | Http({hu_fragment: str, _})
    | Https({hu_fragment: str, _})
    | File({fu_fragment: str, _}) =>
      /* Not sure why we need to call this 3 times, but doesn't work without it. */
      str |> Url.urldecode |> Url.urldecode |> Url.urldecode
    };

  str |> model_of_string(~initial_model);
};

let set_current = Url.Current.set;

let get_current = () =>
  switch (Url.Current.get()) {
  | Some(url) => url
  | None => raise(EmptyCurrent)
  };
