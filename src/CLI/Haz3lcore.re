include Haz3lcorep;

let of_projector = (~sort as _, ~id as _, _) => Language.Grammar.Any();

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type segment = Base.segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type piece = Base.piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector;
};

module Editor = {
  include Editor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(unit);
  module Model = {
    include Model;
    let mk_uncalculated: ZipperBase.t => t(unit) =
      Editor.Model.mk_uncalculated(_);
    let to_move_s: t(unit) => 'a = to_move_s;
  };
};

module MakeTerm = {
  include MakeTerm;
  let from_zip_for_sem = from_zip_for_sem(~of_projector);
};
module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t;
};
