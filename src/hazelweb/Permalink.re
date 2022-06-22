open Js_of_ocaml;

/**
 * Exception raised when there is no URL set for the current window.
 */
exception EmptyCurrent;

/**
 * The type for the permalink.
 */
type t = Url.url;

let key = "program";

/**
 * {update url model} updates {url} for {model}.
 */
let update = (url: t, model: Model.t): t => {
  let program =
    model
    |> Model.get_program
    |> Program.sexp_of_t
    |> Sexplib.Sexp.to_string_mach;

  let set_program = arguments =>
    arguments |> List.remove_assoc(key) |> List.cons((key, program));

  switch (url) {
  | Http(url) => Http({...url, hu_arguments: set_program(url.hu_arguments)})
  | Https(url) =>
    Https({...url, hu_arguments: set_program(url.hu_arguments)})
  | File(url) => File({...url, fu_arguments: set_program(url.fu_arguments)})
  };
};

/**
 * {get_program url} returns the {Program.t} encoded in {url}, if there is one.
 */
let get_program = (url: t): option(Program.t) => {
  let args =
    switch (url) {
    | Http({hu_arguments: args, _})
    | Https({hu_arguments: args, _})
    | File({fu_arguments: args, _}) => args
    };

  switch (List.assoc_opt(key, args)) {
  | Some(sexp) =>
    switch (sexp |> Sexplib.Sexp.of_string |> Program.t_of_sexp) {
    | program => Some(program)
    | exception _ => None
    }
  | None => None
  };
};

/**
 * {set_current url} sets the current window's URL to {url}.
 */
let set_current = Url.Current.set;

/**
 * {get_current ()} gets the current window's URL, throwing {EmptyCurrent} if
 * it is not set.
 */
let get_current = () =>
  switch (Url.Current.get()) {
  | Some(url) => url
  | None => raise(EmptyCurrent)
  };
