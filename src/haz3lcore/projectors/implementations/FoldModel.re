open Util;

/* Keep label geometry independent of the projector UI so formatting and
   rendered placeholders use the same display-column width. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
  expanded: bool,
  always_render: bool,
};

let default: t = {
  text: "⋱",
  expanded: false,
  always_render: false,
};

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | exception _ => default
  | t => t
  };

let of_string = s =>
  switch (Sexplib.Sexp.of_string(s) |> t_of_sexp) {
  | exception _ => default
  | m => m
  };

let shape = (m: t) =>
  ProjectorCore.Shape.inline(Unicode.Width.columns_of_string(m.text));
