open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  segment: string,
  backup_text: string,
  refractors: string,
};

/* Lossless: holes print as the ¿ marker (readers destruct via
   MarkerParse / the FastParse weave). */
let to_string =
  Printer.of_segment(
    ~holes=Token.implicit_hole_marker,
    ~concave_holes=Token.implicit_hole_marker,
    ~indent="",
  );

let refractors_init_str =
  ZipperBase.Refractor.persist(ZipperBase.Refractor.init);

let persist = (zipper: Zipper.t) => {
  let segment = zipper |> Zipper.zip;
  {
    segment: segment |> Segment.sexp_of_t |> Sexplib.Sexp.to_string,
    backup_text: to_string(segment, ~refractors=zipper.refractors.manuals),
    refractors: ZipperBase.Refractor.persist(zipper.refractors),
  };
};

let restore_refractors =
    (persisted: string, refractors: ZipperBase.Refractor.t) => {
  ...refractors,
  manuals:
    try(
      persisted
      |> Sexplib.Sexp.of_string
      |> ZipperBase.Refractor.RefractorList.t_of_sexp
    ) {
    | _ => []
    },
};

let restore = (persisted: t): Zipper.t =>
  persisted.segment
  |> Sexplib.Sexp.of_string
  |> Segment.t_of_sexp
  |> Zipper.unzip(~direction=Left)
  |> Zipper.update_refractors(_, restore_refractors(persisted.refractors));

/* Committed .hz slide text keeps human indentation, but Hazel computes
   indentation at layout time and renders literal leading spaces ON TOP
   of it (doubled, drifting) — so text slides are flattened at load.
   The strip is blind per-line (StringUtil.trim_leading); a multi-line
   string literal would be altered, so slide sources must not contain
   them. */
let flatten_indentation = StringUtil.trim_leading;

/* A text-backed slide: parsing is deferred to PersistentZipper's text
   path (FastParse first), so boot does no sexp round-trip for it. */
let of_text = (text: string): t => {
  segment: "",
  backup_text: flatten_indentation(text),
  refractors: refractors_init_str,
};

let unpersist_serialized = (persisted: t): PersistentZipper.t => {
  /* Only test/debug data reaches this arm now: every shipped slide is
     text-backed, and user idb data lives in PersistentZipper. */
  zipper: restore(persisted) |> Zipper.sexp_of_t |> Sexplib.Sexp.to_string,
  backup_text: persisted.backup_text,
};

let unpersist = (persisted: t): PersistentZipper.t =>
  persisted.segment == ""
    ? PersistentZipper.of_text(persisted.backup_text)
    : unpersist_serialized(persisted);
