include Haz3lcorep;

let of_projector = (~sort as _, ~id as _, _) => Language.Grammar.Any();

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
  type t = Editor.t(ProjectorKind.t, unit, unit);
  module Model = {
    include Model;
    let mk: ZipperBase.t(unit) => t(ProjectorKind.t, unit, unit) =
      Editor.Model.mk(_);
    let to_move_s: t(ProjectorKind.t, unit, unit) => 'a = to_move_s;
  };
};

module MakeTerm = {
  include MakeTerm;
  let from_zip_for_sem = from_zip_for_sem(~of_projector);
};
module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(unit);
};
