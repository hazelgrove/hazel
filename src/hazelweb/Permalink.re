open Js_of_ocaml;

exception EmptyCurrent;

type t = Url.url;

let put_program = (url: t, program: Program.t): t => {
  let sexp =
    program
    |> Program.sexp_of_t
    |> Sexplib.Sexp.to_string_mach
    |> Url.urlencode;

  switch (url) {
  | Http(url) => Http({...url, hu_fragment: sexp})
  | Https(url) => Https({...url, hu_fragment: sexp})
  | File(url) => File({...url, fu_fragment: sexp})
  };
};

let get_program = (url: t): option(Program.t) => {
  let sexp =
    switch (url) {
    | Http({hu_fragment: sexp, _})
    | Https({hu_fragment: sexp, _})
    | File({fu_fragment: sexp, _}) => sexp |> Url.urldecode |> Url.urldecode
    };

  switch (sexp |> Sexplib.Sexp.of_string |> Program.t_of_sexp) {
  | program => Some(program)
  | exception _ => None
  };
};

let set_current = Url.Current.set;

let get_current = () =>
  switch (Url.Current.get()) {
  | Some(url) => url
  | None => raise(EmptyCurrent)
  };
