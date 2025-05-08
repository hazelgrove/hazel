include Haz3lcorep;
open Util;

let of_projector = (_, _) => Grammar.Any();

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type segment = Base.segment(unit);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type piece = Base.piece(unit);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector(unit);
};

module Editor = {
  include Editor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(ProjectorCore.Kind.t, unit);
  module Model = {
    include Model;
    let mk: ZipperBase.t(unit) => t(ProjectorCore.Kind.t, unit) =
      Editor.Model.mk(
        ~projector_to_term=of_projector,
        ~sort=Exp,
        ~shape_of_projector=(_, _, _) => failwith("not implemented"),
        _,
      );
    let to_move_s: t(ProjectorCore.Kind.t, unit) => 'a = to_move_s;
  };
};

module MakeTerm = {
  include MakeTerm;

  let parse_exp = s =>
    s |> parse_exp(~of_projector) |> Option.bind(_, Any.is_exp);
  let from_zip_for_sem = from_zip_for_sem(~of_projector);
};

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(unit);
};
