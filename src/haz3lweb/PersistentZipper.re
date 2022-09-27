open Haz3lcore;
open Sexplib.Std;

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

let unpersist = (persisted: t, init_id: int) =>
  try((
    init_id,
    Sexplib.Sexp.of_string(persisted.zipper) |> Zipper.t_of_sexp,
  )) {
  | _ =>
    print_endline(
      "Warning: using backup text! Serialization may be for an older version of Hazel.",
    );
    switch (Printer.zipper_of_string(init_id, persisted.backup_text)) {
    | None => (init_id + 1, Haz3lcore.Zipper.init(init_id))
    | Some((z, new_id)) => (new_id, z)
    };
  };

let serialize = (zipper: Zipper.t) => {
  persist(zipper) |> yojson_of_t |> Yojson.Safe.to_string;
};

let deserialize = (data: string, init_id) => {
  let persisted = data |> Yojson.Safe.from_string |> t_of_yojson;
  unpersist(persisted, init_id);
};
