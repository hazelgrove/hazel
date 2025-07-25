open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

let to_string = (~concave_holes=?, ~caret=?, z) =>
  Printer.of_zipper(~holes="", ~indent="", ~concave_holes?, ~caret?, z);

let persist = (~projector_to_segment, f, zipper: Zipper.t('p)) => {
  {
    zipper: Zipper.sexp_of_t(f, zipper) |> Sexplib.Sexp.to_string,
    backup_text: to_string(~projector_to_segment, zipper),
  };
};

let unpersist = (~projector_init, f, persisted: t) =>
  try(Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp(f)) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    switch (Parser.to_zipper(~projector_init, persisted.backup_text)) {
    | None => Zipper.init()
    | Some(z) => z
    };
  };

// let serialize = (f, zipper: Zipper.t('p)) => {
//   persist(f, zipper) |> yojson_of_t |> Yojson.Safe.to_string;
// };

// let deserialize = (f, data: string) => {
//   let persisted = data |> Yojson.Safe.from_string |> t_of_yojson;
//   unpersist(f, persisted);
// };
