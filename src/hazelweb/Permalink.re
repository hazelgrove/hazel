open Js_of_ocaml;
module Result = Stdlib.Result;

exception EmptyCurrent;

type t = Url.url;

/**
   [string_of_model model] returns a string representation of [model] to be used in the URL.
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

let put_model = (url: t, model: Model.t): t => {
  /* Serialize and encode. */
  let str =
    model
    |> string_of_model
    |> Js.string
    |> Js.encodeURIComponent
    |> Js.to_string;

  /* Put in hash fragment. */
  switch (url) {
  | Http(url) => Http({...url, hu_fragment: str})
  | Https(url) => Https({...url, hu_fragment: str})
  | File(url) => File({...url, fu_fragment: str})
  };
};

let get_exp = (url: t): option(UHExp.t) => {
  /* Extract hash fragment, deserialize, and decode. */
  let str =
    switch (url) {
    | Http({hu_fragment: str, _})
    | Https({hu_fragment: str, _})
    | File({fu_fragment: str, _}) =>
      /* We need to call {Url.urldecode} because {Url.Current.set} encodes it
       * automatically. */
      str
      |> Url.urldecode
      |> Js.string
      |> Js.decodeURIComponent
      |> Js.to_string
    };

  str |> exp_of_string;
};

let set_current = Url.Current.set;

let get_current = Url.Current.get;
