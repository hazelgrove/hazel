open Sexplib.Std;

module ClientCapabilities = {

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
        whyyy: string,
    }
}
