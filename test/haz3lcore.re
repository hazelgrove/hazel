include Haz3lcorep;

let of_projector = (model, xs) => ProjectorInit.make_term(model, List.hd(xs));

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type segment = Base.segment(ProjectorCore.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type piece = Base.piece(ProjectorCore.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector(ProjectorCore.model);
};

module Editor = {
  include Editor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(ProjectorCore.model);
  module Model = {
    include Model;
    let mk = Editor.Model.mk(~projector_to_term=(_, e) => List.hd(e));
  };
}

module MakeTerm = {
  include MakeTerm;
  
  let parse_exp = parse_exp(~of_projector);
  let from_zip_for_sem =
    from_zip_for_sem(~of_projector);
}

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(ProjectorCore.model);
}