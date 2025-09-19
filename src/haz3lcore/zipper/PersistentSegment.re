open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  segment: string,
  backup_text: string,
};

let to_string = Printer.of_segment(~holes="", ~indent="");

let persist = (zipper: Segment.t) => {
  {
    segment: Segment.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(zipper),
  };
};

let unpersist = (persisted: t, ~root) =>
  try(Sexplib.Sexp.of_string(persisted.segment) |> Segment.t_of_sexp) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    switch (Parser.to_segment(persisted.backup_text, ~root)) {
    | None => Segment.empty
    | Some(z) => z
    };
  };

let to_persistent_zipper = (persisted: t, ~root): PersistentZipper.t => {
  zipper:
    unpersist(persisted, ~root)
    |> Zipper.unzip(~direction=Left)
    |> Zipper.sexp_of_t
    |> Sexplib.Sexp.to_string,
  backup_text: persisted.backup_text,
};
