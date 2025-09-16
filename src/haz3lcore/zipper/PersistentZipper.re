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
    switch (Parser.to_zipper(persisted.backup_text, ~root)) {
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
