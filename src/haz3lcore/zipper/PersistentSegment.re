open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  segment: string,
  backup_text: string,
  refractors: string,
};

let to_string = Printer.of_segment(~holes="", ~indent="");

let persist = (zipper: Zipper.t) => {
  let segment = zipper |> Zipper.zip;
  {
    segment: segment |> Segment.sexp_of_t |> Sexplib.Sexp.to_string,
    backup_text: to_string(segment, ~refractors=zipper.refractors.manuals),
    refractors:
      zipper.refractors.manuals
      |> ZipperBase.Refractor.Map.sexp_of_t
      |> Sexplib.Sexp.to_string,
  };
};

let restore_refractors =
    (persisted: string, refractors: ZipperBase.Refractor.t) => {
  ...refractors,
  manuals:
    try(
      persisted |> Sexplib.Sexp.of_string |> ZipperBase.Refractor.Map.t_of_sexp
    ) {
    | _ => Id.Map.empty
    },
};

let restore = (persisted: t): Zipper.t =>
  persisted.segment
  |> Sexplib.Sexp.of_string
  |> Segment.t_of_sexp
  |> Zipper.unzip(~direction=Left)
  |> Zipper.update_refractors(_, restore_refractors(persisted.refractors));

let restore_from_backup_text = (backup_text: string): Zipper.t =>
  (
    switch (Parser.to_segment(backup_text)) {
    | None => Segment.empty
    | Some(z) => z
    }
  )
  |> Zipper.unzip(~direction=Left);

let unpersist = (persisted: t): PersistentZipper.t => {
  zipper:
    (
      try(restore(persisted)) {
      | _ =>
        print_endline(
          "Warning: using backup text! Serialization may be for an older version of Hazel.",
        );
        restore_from_backup_text(persisted.backup_text);
      }
    )
    |> Zipper.sexp_of_t
    |> Sexplib.Sexp.to_string,
  backup_text: persisted.backup_text,
};
