include Haz3lcorep;

/* The interface we need with the projectors module */

module type Projectors = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind;

  let init: (kind, Semantics.Any.t) => option(model);
  let kind_of_model: model => kind;
  let make_term: (model, Semantics.Any.t) => Semantics.Any.t;
};

module Projectors = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = ProjectorCore.model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type kind = ProjectorCore.Kind.t;

  let init = ProjectorInit.init;
  let kind_of_model = ProjectorCore.kind_of_model;
  let make_term = ProjectorInit.make_term;
};

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Action.t(Projectors.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = Action.project(Projectors.model);
  [@deriving (show({with_path: false}), sexp, yojson)];
};

module Ancestor = {
  include Ancestor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestor.t(Projectors.model);
};

module Ancestors = {
  include Ancestors;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestors.t(Projectors.model);
};

module Backpack = {
  include Backpack;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Backpack.t(Projectors.model);
};

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector(Projectors.model);
};

module Indicated = {
  include Indicated;

  type piece = Indicated.piece(Projectors.model);
};

module Piece = {
  include Piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Piece.t(Projectors.model);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tile = Piece.tile(Projectors.model);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Piece.projector(Projectors.model);
};

module ProjectorBase = {
  include ProjectorBase;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type info = ProjectorBase.info(Projectors.model);

  type external_action = ProjectorBase.external_action(Projectors.model);
};

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(Projectors.model);
};

module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Selection.t(Projectors.model);
};

module Tile = {
  include Tile;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Tile.t(Projectors.model);
};

module Zipper = {
  include Zipper;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Zipper.t(Projectors.model);
};

/* A little more interesting */

let of_projector = (model, xs) =>
  ProjectorInit.make_term(model, List.hd(xs));

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
  let unpersist = unpersist(Projectors.model_of_sexp);
};

module Editor = {
  include Editor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(Projectors.model);

  module Model = {
    include Model;

    let mk = Editor.Model.mk(~projector_to_term=of_projector);
  };

  module Update = {
    include Update;

    let update = Editor.Update.update(~projector_init=ProjectorInit.init);

    let calculate =
      Editor.Update.calculate(
        ~projector_to_term=of_projector,
        ~projector_init=ProjectorInit.init,
      );
  };
};

module CachedStatics = {
  include CachedStatics;

  let init = init(~projector_to_term=of_projector);
};
