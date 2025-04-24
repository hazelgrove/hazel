include Haz3lcorep;

module rec Projectors: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;

  let make_term: (Projectors.model, Sort.t) => Any.t;

  let shape_of_projector:
    (Statics.Map.t, Dynamics.Map.t, Base.projector(model)) => ProjectorShape.t;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = ProjectorCore.model(Editor.t);

  let make_term = (_, _) => Grammar.Any(); //(ProjectorInit.make_term(~term_of_ed=Editor.make_term));

  let shape_of_projector =
    Haz3lcorep.ProjectorInfo.ShapeMapSemantics.from_semantics(
      ~ed_str=Editor.print_string,
    );
}
and Editor: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  module Zipper: {
    type t;
  };

  module Model: {
    let mk: (~sort: Sort.t, Zipper.t) => t;
  };

  let make_term: (Sort.t, t) => Any.t;
  let print_string: t => string;
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Haz3lcorep.Editor.t(Projectors.model);

  module Zipper = {
    type t = Zipper.t(Projectors.model);
  };

  module Model = {
    let mk = (~sort, z: Zipper.t) =>
      Haz3lcorep.Editor.Model.mk(
        ~sort,
        ~shape_of_projector=Projectors.shape_of_projector,
        ~projector_to_term=Projectors.make_term,
        z,
      );
  };

  let make_term = (sort: Sort.t, m: t) => Grammar.Any();
  // MakeTerm.go(
  //   ~of_projector=Projectors.make_term,
  //   sort,
  //   m.state.zipper
  //   |> Haz3lcorep.Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true),
  // ).
  //   term;

  let print_string = _ => "TODO";
};

// module type Projectors = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type model;
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type kind;

//   let init: (kind, Any.t) => option(model);
//   let kind_of_model: model => kind;
//   let make_term: (model, Any.t) => Any.t;
// };
// module rec Projectors: {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type model =
//     | M(ProjectorCore.model(Editor'.t(model)));
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type kind;

//   let make_term: (model, Any.t) => Any.t;
// } = {
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type model =
//     | M(ProjectorCore.model(Editor.t));
//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type kind = ProjectorCore.Kind.t;

//   let init = ProjectorInit.init;
//   let kind_of_model = ProjectorCore.kind_of_model;
//   let make_term: (model, Any.t) => Any.t =
//     (M(m)) => ProjectorInit.make_term(m);
// }

// and Editor: {
//   include (module type of Editor');

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = Editor'.t(Projectors.model);

//   module Model: {
//     include (module type of Editor'.Model);

//     let mk: Zipper.t(Projectors.model) => t(Projectors.model);
//   };

//   module Update: {
//     include (module type of Editor'.Update);
//   };
// } = {
//   include Editor';

//   let of_projector = (m: ProjectorCore.model(t(Projectors.model)), e) =>
//     Projectors.make_term(M(m), List.hd(e));

//   [@deriving (show({with_path: false}), sexp, yojson)]
//   type t = Editor'.t(Projectors.model);

//   module Model = {
//     include Model;

//     let mk = (z: Zipper.t(Projectors.model)) =>
//       Editor'.Model.mk(~projector_to_term=of_projector);
//   };

//   module Update = {
//     include Update;

//     let update = Editor'.Update.update(~projector_init=ProjectorInit.init);

//     let calculate =
//       Editor'.Update.calculate(
//         ~projector_to_term=of_projector,
//         ~projector_init=ProjectorInit.init,
//       );
//   };
// };

module PersistentZipper = {
  include PersistentZipper;
  let persist = persist(Projectors.sexp_of_model);
  let unpersist = unpersist(Projectors.model_of_sexp);
};

module CachedStatics = {
  include CachedStatics;

  let init = init(~projector_to_term=Projectors.make_term');
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
