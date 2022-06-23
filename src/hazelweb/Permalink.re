open Js_of_ocaml;
module Result = Stdlib.Result;

exception EmptyCurrent;

type t = Url.url;

let string_of_model = (model: Model.t): string => {
  model
  |> Model.get_program
  |> Program.get_uhexp
  |> Hazeltext.Print.print_exp
  |> Url.urlencode;
};

let model_of_string = (~initial_model: Model.t, str: string): Model.t =>
  str
  |> Hazeltext.Parsing.ast_of_string
  |> Result.to_option
  |> Option.map(e =>
       Program.mk(
         ~width=initial_model.cell_width,
         Statics_Exp.fix_and_renumber_holes_z(
           Contexts.initial,
           ZExp.place_before(e),
         ),
       )
     )
  |> Option.map(program => Model.put_program(program, initial_model))
  |> Option.value(~default=initial_model);

let put_model = (url: t, model: Model.t): t => {
  let str = model |> string_of_model;

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
    | File({fu_fragment: str, _}) => str |> Url.urldecode |> Url.urldecode
    };

  str |> model_of_string(~initial_model);
};

let set_current = Url.Current.set;

let get_current = () =>
  switch (Url.Current.get()) {
  | Some(url) => url
  | None => raise(EmptyCurrent)
  };
