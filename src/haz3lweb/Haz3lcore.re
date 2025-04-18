include Haz3lcorep;

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Action.t(ProjectorCore.model);
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

module Editor = {
  include Editor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t(ProjectorCore.model);
};

module Piece = {
  include Piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Piece.t(ProjectorCore.model);
};

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(ProjectorCore.model);
};

module Zipper = {
  include Zipper;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Zipper.t(ProjectorCore.model);
};

/* A little more interesting */

module MakeTerm = {
  open MakeTerm;

  // TODO Wrap Probes here
  let from_zip_for_sem =
    from_zip_for_sem(~of_projector=(_, e) => List.hd(e));
};
