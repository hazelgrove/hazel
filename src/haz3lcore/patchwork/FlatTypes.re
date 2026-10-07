/* FlatTypes.re - Type definitions for flat document representation
   This module is separate from FlatConvert to avoid dependency cycles.
   It contains only type definitions with minimal dependencies. */

open Util;

module Flat = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type tile = {
    id: Id.t,
    label: Label.t,
    mold: Mold.t,
    shards: list(int),
    children: list(list(Id.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type projector = {
    id: Id.t,
    kind: string,
    syntax: Id.t,
    model: string,
  };

  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type piece =
    | Tile(tile)
    | Grout(Grout.t)
    | Secondary(Secondary.t)
    | Projector(projector);
};

module Doc = {
  include Id.Map;
  type nonrec t = t(Flat.piece);

  let union_all = (docs: list(t)): t => {
    List.fold_left(union((_, _, a) => Some(a)), empty, docs);
  };

  /* pp and show functions compatible with ppx_deriving_show */
  let pp = (fmt: Format.formatter, doc: t): unit => {
    Format.fprintf(fmt, "<Doc: %d pieces>", cardinal(doc));
  };

  let show = (doc: t): string => {
    Format.asprintf("<Doc: %d pieces>", cardinal(doc));
  };

  /* sexp functions for ppx_sexp_conv */
  let sexp_of_t = (doc: t): Sexplib.Sexp.t => {
    Id.Map.sexp_of_t(Flat.sexp_of_piece, doc);
  };

  let t_of_sexp = (sexp: Sexplib.Sexp.t): t => {
    Id.Map.t_of_sexp(Flat.piece_of_sexp, sexp);
  };

  /* yojson functions for ppx_yojson_conv */
  let yojson_of_t = (doc: t): Yojson.Safe.t => {
    Id.Map.yojson_of_t(Flat.yojson_of_piece, doc);
  };

  let t_of_yojson = (json: Yojson.Safe.t): t => {
    Id.Map.t_of_yojson(Flat.piece_of_yojson, json);
  };

  /* equal function for ppx_deriving eq */
  let equal = (a: t, b: t): bool => {
    equal(Flat.equal_piece, a, b);
  };
};
