open Util;

[@deriving (show({with_path: false}), yojson, sexp)]
type t = (float, Page.Update.t);

let mk = (update): t => {
  (JsUtil.timestamp(), update);
};

let to_string = ((timestamp, update): t) => {
  /*let status =
    switch (entry.error) {
    | None => "SUCCESS"
    | Some(failure) => "FAILURE(" ++ UpdateAction.Failure.show(failure) ++ ")"
    };*/
  Printf.sprintf(
    "%.0f: %s",
    timestamp,
    Page.Update.show(update),
    //status,
  );
};

let serialize = (entry: t): string => {
  entry |> sexp_of_t |> Sexplib.Sexp.to_string;
};

let deserialize = (s: string): t => {
  s |> Sexplib.Sexp.of_string |> t_of_sexp;
};
