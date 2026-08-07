open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

let to_string = Printer.of_zipper(~holes="", ~indent="");

let persist = (zipper: Zipper.t) => {
  {
    zipper: Zipper.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(zipper),
  };
};

let unpersist = (persisted: t, ~root) =>
  try(Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    /* Persisted programs are complete, so the linear Menhir zip usually
       takes this; the simulated-typing parser is quadratic in program
       size and can hang startup on big stale-sexp slides. */
    switch (
      FastParse.of_text(
        ~materialize=Triggers.invoked_projector,
        ~root,
        String.trim(persisted.backup_text),
      )
    ) {
    | Some(segment) => Zipper.unzip(segment)
    | None =>
      switch (Parser.to_zipper(persisted.backup_text, ~root)) {
      | None => Zipper.init()
      | Some(z) => z
      }
    };
  };
