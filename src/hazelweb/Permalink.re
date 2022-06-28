open Js_of_ocaml;
module Result = Stdlib.Result;

type t = string;

/**
   [string_of_model model] returns a string representation of [model] to be
   used in the URL.
 */
let string_of_model = (model: Model.t): string =>
  model |> Model.get_program |> Program.get_uhexp |> Hazeltext.Print.print_exp;

/**
   [exp_of_string str] parses [str] into a {!type:UHExp.t} if it is valid.
 */
let exp_of_string = (str: string): option(UHExp.t) =>
  try(str |> Hazeltext.Parsing.ast_of_string |> Result.to_option) {
  | _ => None
  };

let put_model = (_frag: t, model: Model.t): t => {
  /* Serialize and encode. */
  model
  |> string_of_model
  |> Js.string
  |> Js.encodeURIComponent
  |> Js.to_string;
};

let get_exp = (frag: t): option(UHExp.t) => {
  /* Extract hash fragment, deserialize, and decode. */
  let str =
    frag |> Url.urldecode |> Js.string |> Js.decodeURIComponent |> Js.to_string;

  str |> exp_of_string;
};

let fragment_of_url = (url: Url.url): string =>
  switch (url) {
  | Http({hu_fragment: str, _})
  | Https({hu_fragment: str, _})
  | File({fu_fragment: str, _}) => str
  };

let update_fragment = (f: string => string, frag: t): t => f(frag);

let clear_fragment = update_fragment(_ => "");

let set_current = frag => {
  let frag =
    switch (frag) {
    | "" => ""
    | frag => "#" ++ frag
    };
  let history = Js_of_ocaml.Dom_html.window##.history;
  history##pushState(Js.null, Js.string(""), Js.some(Js.string(frag)));
};

let get_current = () => Url.Current.get() |> Option.map(fragment_of_url);
