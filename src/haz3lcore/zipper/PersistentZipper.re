open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  zipper: string,
  backup_text: string,
};

let persist = (zipper: Zipper.t) => {
  {
    zipper: Zipper.sexp_of_t(zipper) |> Sexplib.Sexp.to_string,
    backup_text: Printer.to_string_basic(zipper),
  };
};

let unpersist = (persisted: t) =>
  try(Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    switch (Printer.zipper_of_string(persisted.backup_text)) {
    | None => Zipper.init()
    | Some(z) => z
    };
  };

let serialize = (zipper: Zipper.t) => {
  persist(zipper) |> yojson_of_t |> Yojson.Safe.to_string;
};

let deserialize = (data: string) => {
  let persisted = data |> Yojson.Safe.from_string |> t_of_yojson;
  unpersist(persisted);
};
