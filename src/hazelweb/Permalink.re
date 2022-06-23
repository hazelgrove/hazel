open Js_of_ocaml;

/**
 * Exception raised when there is no URL set for the current window.
 */
exception EmptyCurrent;

/**
 * The type for the permalink.
 */
type t = Url.url;

/**
 * {update url model} updates {url} for {model}.
 */
let put_program = (url: t, program: Program.t): t => {
  let sexp = program |> Program.sexp_of_t |> Sexplib.Sexp.to_string_mach;

  switch (url) {
  | Http(url) => Http({...url, hu_fragment: sexp})
  | Https(url) => Https({...url, hu_fragment: sexp})
  | File(url) => File({...url, fu_fragment: sexp})
  };
};

/**
 * {get_program url} returns the {Program.t} encoded in {url}, if there is one.
 */
let get_program = (url: t): option(Program.t) => {
  let sexp =
    switch (url) {
    | Http({hu_fragment: sexp, _})
    | Https({hu_fragment: sexp, _})
    | File({fu_fragment: sexp, _}) => sexp |> Url.urldecode
    };

  switch (sexp |> Sexplib.Sexp.of_string |> Program.t_of_sexp) {
  | program => Some(program)
  | exception _ => None
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
