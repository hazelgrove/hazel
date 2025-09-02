open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  segment: Segment.t,
  backup_text: string,
};
// [@deriving (show({with_path: false}), sexp, yojson)]
// type t' = {
//   segment: Segment.t,
//   backup_text: string,
// };

let to_string = Printer.of_segment(~holes="", ~indent="");

let persist = (segment: Segment.t): t => {
  {
    segment,
    backup_text: to_string(segment),
  };
};

let unpersist = (persisted: t) => persisted.segment;

let to_persistent_zipper = (persisted: t): PersistentZipper.t => {
  zipper:
    unpersist(persisted)
    |> Zipper.unzip(~direction=Left)
    |> Zipper.sexp_of_t
    |> Sexplib.Sexp.to_string,
  backup_text: persisted.backup_text,
};
