open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* The model of the projector that draws a Fumola reference.
 *
 * Here rather than beside the projector for the same reason ProjectorKind is:
 * the projector lives in haz3lcore, but the value that carries it is built in
 * language, and language cannot look upwards. Sharing the type means the two
 * sides cannot drift apart in how the model is spelled.
 *
 * Both halves are text, and deliberately so. The widget is drawn where the
 * value ends up -- in a result -- and a projector that had to consult statics
 * would find none there. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* The Fumola program that reads the cell, e.g. "peek(`x)!". */
  reads: string,
  /* What that program produced, already rendered. */
  shown: string,
};

let empty: t = {
  reads: "",
  shown: "",
};

/* A projector's model travels as the sexp of its model type; see
   ProjectorBase.Cook. */
let serialize = (m: t): string => m |> sexp_of_t |> Sexplib.Sexp.to_string;

let deserialize = (s: string): t =>
  switch (s |> Sexplib.Sexp.of_string |> t_of_sexp) {
  | exception _ => empty
  | m => m
  };
