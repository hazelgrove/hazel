include Haz3lcorep;
open Util;

module rec Projectors: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;

  let make_term: (Projectors.model, Sort.t) => Any.t;

  let shape_of_projector:
    (Statics.Map.t, Dynamics.Map.t, Base.projector(model)) => ProjectorShape.t;

  let kind_of_model: model => ProjectorCore.Kind.t;
  let sort_of_model: model => Sort.t;
  let init:
    (ProjectorCore.Kind.t, Any.t, unit => option(Editor.t)) => option(model);

  module View: {
    let split_views:
      (
        ~settings: CoreSettings.t,
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
        ~set_model: model => Ui_effect.t(unit),
        ~make_active: Ui_effect.t(unit),
        ProjectorView.Model.projector_data(model)
      ) =>
      (Web.Node.t, option(Web.Node.t));

    let mk_status:
      (
        Base.projector(ProjectorCore.model('ed)),
        ~editor_active: bool,
        ~indicated: option((Id.t, Direction.t)),
        ~selection_ids: list(Id.t),
        ~info: ProjectorBase.info,
        ~id: Id.t
      ) =>
      ProjectorView.Model.status;
  };
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = ProjectorCore.model(Editor.t);

  let make_term = (_, _) => Grammar.Any(); //(ProjectorInit.make_term(~term_of_ed=Editor.make_term));

  let shape_of_projector =
    Haz3lcorep.ProjectorInfo.ShapeMapSemantics.from_semantics(
      ~ed_str=Editor.print_string,
    );

  let kind_of_model = ProjectorCore.kind_of_model;

  //TODO(andrew): proper sort for deco
  let sort_of_model = _ => Sort.Exp;

  let init = ProjectorInit.init;

  module View = {
    let split_views =
        (~settings: CoreSettings.t, ~font_metrics, ~secondary_icons) =>
      ProjectorView.split_views(
        ~ed_str=Editor.print_string,
        ~view_any=
          Editor.View.view_any(~settings, ~font_metrics, ~secondary_icons),
        ~font_metrics,
      );

    let mk_status = ProjectorView.Model.mk_status;
  };
}
and Editor: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t;

  module Zipper: {
    type t = Zipper.t(Projectors.model);
  };

  module Model: {
    let mk: (~sort: Sort.t, Zipper.t) => t;
    let mk_from_exp: (~settings: CoreSettings.t, ~inline: bool=?, Exp.t) => t;
  };

  // module Action: {
  //   include (module type of Action);
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type t;
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type project;
  //   module Result: (module type of Action.Result);

  //   let paste_string: string => t;
  //   let paste_segment: Segment.t(Projectors.model) => t;

  //   let is_edit: t => bool;
  //   let should_scroll_active: t => bool;
  // };

  module Update: {
    let update:
      (
        ~settings: CoreSettings.t,
        Action.t(Projectors.model),
        ~sort: Sort.t,
        CachedStatics.t,
        t
      ) =>
      Action.Result.t(t);

    let calculate:
      (
        ~settings: CoreSettings.t,
        ~is_edited: bool,
        ~sort: Sort.t,
        CachedStatics.t,
        Dynamics.Map.t,
        t
      ) =>
      t;

    let undo: t => option(t);
    let redo: t => option(t);
  };

  let make_term: (Sort.t, t) => Any.t;

  let make_z_serialization: t => string;
  let get_syntax_cache:
    t => Haz3lcorep.Editor.CachedSyntax.t(Projectors.model);
  let print_string: t => string;
  let key_handoff: (t, Key.t) => option(Action.project(Projectors.model));
  let get_z: t => Zipper.t;
  let jump_to_tile_action: (Id.t, t) => option(Action.t(Projectors.model));

  let get_projectors: t => Id.Map.t(Base.projector(Projectors.model));
  let get_measured: t => Measured.t;
  let get_selection_ids: t => list(Id.t);
  let get_indicated: t => option((Id.t, Direction.t));
  let get_tiles: t => TileMap.t(Projectors.model);

  let trailing_hole_ctx: (t, Statics.Map.t) => option(Ctx.t);

  module View: {
    let all_projectors:
      (
        ~settings: CoreSettings.t,
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~inject: Action.t(Projectors.model) => Ui_effect.t(unit),
        ~make_active: Ui_effect.t(unit),
        list(ProjectorView.Model.projector_data(Projectors.model))
      ) =>
      list(Web.Node.t);

    let view:
      (
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~sort: Sort.t,
        t
      ) =>
      Web.Node.t;

    let view_any:
      (
        ~settings: CoreSettings.t,
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        Any.t
      ) =>
      Web.Node.t;

    let mk_projector_model:
      (
        Id.Map.t(Tile.projector(Projectors.model)),
        Measured.t,
        list(TileMap.key),
        option((TileMap.key, Direction.t)),
        Haz3lcorep.Statics.Map.t,
        Dynamics.Map.t,
        bool
      ) =>
      list(ProjectorView.Model.projector_data(Projectors.model));
  };
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

    let mk_from_exp = (~settings: CoreSettings.t, ~inline=false, term: Exp.t) => {
      ExpToSegment.exp_to_segment(
        term,
        ~settings=ExpToSegment.Settings.of_core(~inline, settings),
      )
      |> Haz3lcorep.Zipper.unzip
      |> mk(~sort=Exp);
    };
  };

  // module Action = {
  //   include Action;
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type t = Action.t(Projectors.model);
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type project = Action.project(Projectors.model);

  //   let paste_string = s => Action.Paste(String(s));
  //   let paste_segment = s => Action.Paste(Segment(s));
  // };

  module Update = {
    let update = (~settings, action, ~sort, statics, editor) => {
      Haz3lcorep.Editor.Update.update(
        ~settings,
        ~sort,
        ~projector_init=(_, _, _) => failwith("not implemented"),
        ~projector_to_term=Projectors.make_term,
        ~shape_of_projector=Projectors.shape_of_projector,
        ~seg_of_projector=
          (sort, p) =>
            Projectors.make_term(p, sort)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        action,
        statics,
        editor,
      );
    };

    let calculate =
        (
          ~settings: CoreSettings.t,
          ~is_edited: bool,
          ~sort: Sort.t,
          statics: CachedStatics.t,
          dynamics: Dynamics.Map.t,
          ed: t,
        )
        : t =>
      Haz3lcorep.Editor.Update.calculate(
        ~settings,
        ~is_edited,
        ~projector_init=(_, _, _) => failwith("not implemented"),
        ~projector_to_term=Projectors.make_term,
        ~shape_of_projector=Projectors.shape_of_projector,
        ~seg_of_projector=
          (sort, p) =>
            Projectors.make_term(p, sort)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        ~sort,
        statics,
        dynamics,
        ed,
      );

    let undo = Haz3lcorep.Editor.Update.undo;
    let redo = Haz3lcorep.Editor.Update.redo;
  };

  let get_syntax_cache = (m: t) => m.syntax;

  let get_z = (m: t) => m.state.zipper;

  let make_z_serialization = (m: t) =>
    //TODO(andrew): actual serialization fn for projectors
    m |> get_z |> Haz3lcorep.Zipper.show((_, _) => ());

  let make_seg = (m: t) =>
    m
    |> get_z
    |> Haz3lcorep.Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true);

  let make_term = (sort: Sort.t, m: t) =>
    MakeTerm.go(~of_projector=Projectors.make_term, sort, make_seg(m)).term;

  let trailing_hole_ctx = Haz3lcorep.Editor.Model.trailing_hole_ctx;

  let print_string = _ => "TODO";

  let move_dir = (key: Key.t): option(Direction.t) =>
    switch (key) {
    | {key: D("ArrowLeft"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
      Some(Left)
    | {key: D("ArrowRight"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
      Some(Right)
    | _ => None
    };

  let key_handoff =
      (editor: t, key: Key.t): option(Action.project(Projectors.model)) => {
    let z = editor.state.zipper;
    switch (
      move_dir(key),
      Siblings.neighbors(editor.state.zipper.relatives.siblings),
    ) {
    | _ when z.caret != Outer => None
    | (Some(Left), (Some(Projector({id, model, _})), _)) =>
      open ProjectorCore.Kind;
      let kind = Projectors.kind_of_model(model);
      let.gadt W(kind_gadt) = kind;
      let methods = ProjectorInit.to_module(kind_gadt);
      methods.focusable.keyboard != None
        ? Some(Haz3lcorep.Action.Focus(id, kind, Some(Right))) : None;
    | (Some(Right), (_, Some(Projector({id, model, _})))) =>
      open ProjectorCore.Kind;
      let kind = Projectors.kind_of_model(model);
      let.gadt W(kind_gadt) = kind;
      let methods = ProjectorInit.to_module(kind_gadt);
      methods.focusable.keyboard != None
        ? Some(Haz3lcorep.Action.Focus(id, kind, Some(Left))) : None;
    | _ => None
    };
  };

  let jump_to_tile_action = (tile, model: t) =>
    switch (TileMap.find_opt(tile, model.syntax.tiles)) {
    | Some(_) => Some(Haz3lcorep.Action.Jump(TileId(tile)))
    | None => None
    };

  let get_projectors = (m: t) => m.syntax.projectors;
  let get_measured = (m: t) => m.syntax.measured;
  let get_selection_ids = (m: t) => m.syntax.selection_ids;
  let get_indicated = (m: t): option((Id.t, Direction.t)) =>
    switch (Indicated.piece(m.state.zipper)) {
    | None => None
    | Some((p, side, _)) => Some((Piece.id(p), side))
    };
  let get_tiles = (m: t) => m.syntax.tiles;

  module View = {
    let mk_projector_model =
      ProjectorView.Model.mk(~mk_status=Projectors.View.mk_status);

    let all_projectors =
        (~settings: CoreSettings.t, ~font_metrics, ~secondary_icons) =>
      ProjectorView.all(
        ~split_views=
          Projectors.View.split_views(
            ~settings: CoreSettings.t,
            ~font_metrics,
            ~secondary_icons,
          ),
      );

    let view = (~font_metrics, ~secondary_icons, ~sort, m: t) =>
      CodeViewable.view_editor(~font_metrics, ~secondary_icons, ~sort, m);

    let view_any = (~settings) =>
      CodeViewable.view_any(
        ~settings=ExpToSegment.Settings.of_core(~inline=true, settings),
      );
  };
};

module PersistentZipper = {
  include PersistentZipper;
  let persist = persist(Projectors.sexp_of_model);
  let unpersist = unpersist(Projectors.model_of_sexp);
};

module CachedStatics = {
  include CachedStatics;

  let init = init(~projector_to_term=Projectors.make_term);
};

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Action.t(Projectors.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = Action.project(Projectors.model);
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
  type info = ProjectorBase.info;

  type external_action = ProjectorBase.external_action;
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

module Indicated = {
  include Indicated;

  type piece = Indicated.piece(Projectors.model);
  let ci_of: (Zipper.t, Statics.Map.t) => option(Statics.Info.t) = Indicated.ci_of;
};
