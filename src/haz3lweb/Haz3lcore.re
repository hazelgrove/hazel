include Haz3lcorep;

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Action.t(ProjectorCore.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = Action.project(ProjectorCore.model);
  [@deriving (show({with_path: false}), sexp, yojson)];
};

module Ancestor = {
  include Ancestor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestor.t(ProjectorCore.model);
};

module Ancestors = {
  include Ancestors;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestors.t(ProjectorCore.model);
};

module Backpack = {
  include Backpack;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Backpack.t(ProjectorCore.model);
};

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector(ProjectorCore.model);
};

module Indicated = {
  include Indicated;

  type piece = Indicated.piece(ProjectorCore.model);
};

module Piece = {
  include Piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Piece.t(ProjectorCore.model);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tile = Piece.tile(ProjectorCore.model);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Piece.projector(ProjectorCore.model);
};

module ProjectorBase = {
  include ProjectorBase;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type info = ProjectorBase.info(ProjectorCore.model);

  type external_action = ProjectorBase.external_action(ProjectorCore.model);
};

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(ProjectorCore.model);
};

module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Selection.t(ProjectorCore.model);
};

module Tile = {
  include Tile;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Tile.t(ProjectorCore.model);
};

module Zipper = {
  include Zipper;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Zipper.t(ProjectorCore.model);
};

/* A little more interesting */

module MakeTerm = {
  include MakeTerm;

  // TODO Wrap Probes here
  let from_zip_for_sem =
    from_zip_for_sem(~of_projector=(_, e) => List.hd(e));

  let for_projection =
    for_projection(
      ~of_projector=(_, e) => List.hd(e),
      ~log_projector=_ => (),
      _,
    );
};

module PersistentZipper = {
  include PersistentZipper;
  let persist = persist(ProjectorCore.sexp_of_model);
  let unpersist = unpersist(ProjectorCore.model_of_sexp);
};

module Editor = {
  include Editor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(ProjectorCore.model);

  module Model = {
    include Model;

    let mk = Editor.Model.mk(~projector_to_term=(_, e) => List.hd(e));
  };

  module Update = {
    include Update;

    let update = Editor.Update.update(~projector_init=ProjectorInit.init);

    let calculate =
      Editor.Update.calculate(
        ~projector_to_term=(_, e) => List.hd(e),
        ~projector_init=ProjectorInit.init,
      );
  };
};

module CachedStatics = {
  include CachedStatics;

  let init = init(~projector_to_term=(_, e) => List.hd(e));
};
