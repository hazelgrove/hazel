open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

let to_string = Printer.of_zipper(~holes="");

let persist = (zipper: Zipper.t) => {
  {
    zipper: Zipper.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(zipper),
  };
};

/* serialized states predating grout-free editing carry stored grout
   in siblings/ancestors/selection; the edit state never does — strip
   everywhere on restore (holes are derived) */
let strip_zipper = (z: Zipper.t): Zipper.t => {
  ...z,
  selection: {
    ...z.selection,
    content: GroutPlace.strip(z.selection.content),
  },
  relatives: Relatives.regrout(Left, z.relatives),
};

let unpersist = (persisted: t, ~root) =>
  try(
    Sexplib.Sexp.of_string(persisted.zipper)
    |> Zipper.t_of_sexp
    |> strip_zipper
  ) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    switch (Parser.to_zipper(persisted.backup_text, ~root)) {
    | None => Zipper.init()
    | Some(z) => z
    };
  };
