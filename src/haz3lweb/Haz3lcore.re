include Haz3lcorep;
open Util;

module rec Projector: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let mk:
      (ProjectorCore.Kind.t, Any.t, unit => option(Editor.Model.t)) =>
      option(t);

    let get_sort: t => Sort.t;
    let get_kind: t => ProjectorCore.Kind.t;
    let get_shape:
      (Statics.Map.t, Dynamics.Map.t, Base.projector(t)) => ProjectorShape.t;
    let get_focusable: t => ProjectorBase.Focusable.t;

    let make_term: (t, Sort.t) => Any.t;
  };

  module View: {
    let split_views:
      (
        ~settings: CoreSettings.t,
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
        ~set_model: Model.t => Ui_effect.t(unit),
        ~make_active: Ui_effect.t(unit),
        ProjectorView.Model.projector_data(Model.t)
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
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = ProjectorCore.model(Editor.Model.t);

    let get_shape =
      Haz3lcorep.ProjectorInfo.ShapeMapSemantics.from_semantics(
        ~ed_str=Editor.View.print_string,
      );
    let get_kind = ProjectorCore.kind_of_model;
    let get_focusable = ProjectorInit.focusable_of_model;

    //TODO(andrew): proper sort for deco
    let get_sort = _ => Sort.Exp;

    let make_term =
      ProjectorInit.make_term(~term_of_ed=Editor.Model.make_term);

    let mk = ProjectorInit.init;
  };

  module View = {
    let split_views =
        (~settings: CoreSettings.t, ~font_metrics, ~secondary_icons) =>
      ProjectorView.split_views(
        ~ed_str=Editor.View.print_string,
        ~mk_ed=Editor.Model.mk(~settings),
        ~view_ed=Editor.View.view(~font_metrics, ~secondary_icons),
        ~font_metrics,
      );

    let mk_status = ProjectorView.Model.mk_status;
  };
}
and Editor: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Haz3lcorep.Editor.t(Projector.Model.t); // Transparent definition needed for handing editor to projectorinit

    let mk: (~settings: CoreSettings.t, ~inline: bool=?, Any.t) => t;

    let get_z: t => Zipper.t(Projector.Model.t);
    let make_term: (Sort.t, t) => Any.t;
    let get_trailing_hole_ctx: (t, Statics.Map.t) => option(Ctx.t);
    // [@deriving (show({with_path: false}), sexp, yojson)]
    // type persistent;
    // let persist: t => persistent;
    // let unpersist: persistent => t;
    let of_zipper: (~sort: Sort.t, Zipper.t(Projector.Model.t)) => t; // TODO: Replace with persistence logic
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Action.t(Projector.Model.t);

    let update:
      (
        ~settings: CoreSettings.t,
        ~sort: Sort.t,
        CachedStatics.t,
        t,
        Model.t
      ) =>
      Action.Result.t(Model.t);

    let calculate:
      (
        ~settings: CoreSettings.t,
        ~is_edited: bool,
        ~sort: Sort.t,
        CachedStatics.t,
        Dynamics.Map.t,
        Model.t
      ) =>
      Model.t;

    let undo: Model.t => option(Model.t);
    let redo: Model.t => option(Model.t);

    let key_handoff:
      (Model.t, Key.t) => option(Action.project(Projector.Model.t));
    let jump_to_tile_action:
      (Id.t, Model.t) => option(Action.t(Projector.Model.t));
  };

  module View: {
    let print_string: Model.t => string;

    // TODO[Matt]: This should be the only function in projectors
    let view:
      (
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~sort: Sort.t,
        Model.t
      ) =>
      Web.Node.t;

    let all_projectors:
      (
        ~settings: CoreSettings.t,
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~inject: Action.t(Projector.Model.t) => Ui_effect.t(unit),
        ~make_active: Ui_effect.t(unit),
        list(ProjectorView.Model.projector_data(Projector.Model.t))
      ) =>
      list(Web.Node.t);
    // let view_any:
    //   (
    //     ~settings: CoreSettings.t,
    //     ~font_metrics: FontMetrics.t,
    //     ~secondary_icons: bool,
    //     Any.t
    //   ) =>
    //   Web.Node.t;
    let mk_projector_model:
      (
        Id.Map.t(Tile.projector(Projector.Model.t)),
        Measured.t,
        list(TileMap.key),
        option((TileMap.key, Direction.t)),
        Haz3lcorep.Statics.Map.t,
        Dynamics.Map.t,
        bool
      ) =>
      list(ProjectorView.Model.projector_data(Projector.Model.t));
  };

  // TODO: refactor these helper functions away

  let get_syntax_cache:
    Model.t => Haz3lcorep.Editor.CachedSyntax.t(Projector.Model.t);
  let get_projectors: Model.t => Id.Map.t(Base.projector(Projector.Model.t));
  let get_measured: Model.t => Measured.t;
  let get_selection_ids: Model.t => list(Id.t);
  let get_indicated: Model.t => option((Id.t, Direction.t));
  let get_tiles: Model.t => TileMap.t(Projector.Model.t);
} = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Haz3lcorep.Editor.t(Projector.Model.t);

    let mk = (~settings: CoreSettings.t, ~inline=false, term: Any.t) => {
      ExpToSegment.any_to_segment(
        term,
        ~settings=ExpToSegment.Settings.of_core(~inline, settings),
      )
      |> Haz3lcorep.Zipper.unzip
      |> Haz3lcorep.Editor.Model.mk(
           ~sort=Any.sort(term),
           ~shape_of_projector=Projector.Model.get_shape,
           ~projector_to_term=Projector.Model.make_term,
         );
    };

    let of_zipper =
      Haz3lcorep.Editor.Model.mk(
        ~shape_of_projector=Projector.Model.get_shape,
        ~projector_to_term=Projector.Model.make_term,
      );

    let get_z = (m: t) => m.state.zipper;

    let make_seg = (m: t) =>
      m
      |> get_z
      |> Haz3lcorep.Zipper.smart_seg(~dump_backpack=true, ~erase_buffer=true);

    let make_term = (sort: Sort.t, m: t) =>
      MakeTerm.go(~of_projector=Projector.Model.make_term, sort, make_seg(m)).
        term;

    let get_trailing_hole_ctx = Haz3lcorep.Editor.Model.trailing_hole_ctx;
  };

  // module Action = {
  //   include Action;
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type t = Action.t(Projector.Model.t);
  //   [@deriving (show({with_path: false}), sexp, yojson)]
  //   type project = Action.project(Projector.Model.t);

  //   let paste_string = s => Action.Paste(String(s));
  //   let paste_segment = s => Action.Paste(Segment(s));
  // };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Action.t(Projector.Model.t);

    let update = (~settings, ~sort, statics, action, editor: Model.t) => {
      Haz3lcorep.Editor.Update.update(
        ~settings,
        ~sort,
        ~projector_init=Projector.Model.mk,
        ~projector_to_term=Projector.Model.make_term,
        ~shape_of_projector=Projector.Model.get_shape,
        ~seg_of_projector=
          (sort, p) =>
            Projector.Model.make_term(p, sort)
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
          ed: Model.t,
        )
        : Model.t =>
      Haz3lcorep.Editor.Update.calculate(
        ~settings,
        ~is_edited,
        ~projector_init=Projector.Model.mk,
        ~projector_to_term=Projector.Model.make_term,
        ~shape_of_projector=Projector.Model.get_shape,
        ~seg_of_projector=
          (sort, p) =>
            Projector.Model.make_term(p, sort)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        ~sort,
        statics,
        dynamics,
        ed,
      );

    let undo = Haz3lcorep.Editor.Update.undo;
    let redo = Haz3lcorep.Editor.Update.redo;

    let move_dir = (key: Key.t): option(Direction.t) =>
      switch (key) {
      | {key: D("ArrowLeft"), sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} =>
        Some(Left)
      | {
          key: D("ArrowRight"),
          sys: _,
          shift: Up,
          meta: Up,
          ctrl: Up,
          alt: Up,
        } =>
        Some(Right)
      | _ => None
      };

    let key_handoff =
        (editor: Model.t, key: Key.t)
        : option(Action.project(Projector.Model.t)) => {
      let z = editor.state.zipper;
      switch (
        move_dir(key),
        Siblings.neighbors(editor.state.zipper.relatives.siblings),
      ) {
      | _ when z.caret != Outer => None
      | (Some(Left), (Some(Projector({id, model, _})), _)) =>
        let kind = Projector.Model.get_kind(model);
        let focusable = Projector.Model.get_focusable(model);
        focusable.keyboard != None
          ? Some(Haz3lcorep.Action.Focus(id, kind, Some(Right))) : None;
      | (Some(Right), (_, Some(Projector({id, model, _})))) =>
        let kind = Projector.Model.get_kind(model);
        let focusable = Projector.Model.get_focusable(model);
        focusable.keyboard != None
          ? Some(Haz3lcorep.Action.Focus(id, kind, Some(Left))) : None;
      | _ => None
      };
    };

    let jump_to_tile_action = (tile, model: Model.t) =>
      switch (TileMap.find_opt(tile, model.syntax.tiles)) {
      | Some(_) => Some(Haz3lcorep.Action.Jump(TileId(tile)))
      | None => None
      };
  };

  let get_syntax_cache = (m: Model.t) => m.syntax;

  let get_projectors = (m: Model.t) => m.syntax.projectors;
  let get_measured = (m: Model.t) => m.syntax.measured;
  let get_selection_ids = (m: Model.t) => m.syntax.selection_ids;
  let get_indicated = (m: Model.t): option((Id.t, Direction.t)) =>
    switch (Indicated.piece(m.state.zipper)) {
    | None => None
    | Some((p, side, _)) => Some((Piece.id(p), side))
    };
  let get_tiles = (m: Model.t) => m.syntax.tiles;

  module View = {
    // TODO[Matt]: This should be the only function in view.
    let view = (~font_metrics, ~secondary_icons, ~sort, m: Model.t) =>
      CodeViewable.view_editor(~font_metrics, ~secondary_icons, ~sort, m);

    let mk_projector_model =
      ProjectorView.Model.mk(~mk_status=Projector.View.mk_status);

    let all_projectors =
        (~settings: CoreSettings.t, ~font_metrics, ~secondary_icons) =>
      ProjectorView.all(
        ~split_views=
          Projector.View.split_views(
            ~settings: CoreSettings.t,
            ~font_metrics,
            ~secondary_icons,
          ),
      );

    let print_string = _ => "TODO";
    // let view_any = (~settings) =>
    //   CodeViewable.view_any(
    //     ~settings=ExpToSegment.Settings.of_core(~inline=true, settings),
    //   );
  };
};

module PersistentZipper = {
  include PersistentZipper;
  // TODO: move these into Editor
  let persist = persist(Projector.Model.sexp_of_t);
  let unpersist = unpersist(Projector.Model.t_of_sexp);
};

module CachedStatics = {
  include CachedStatics;

  let init = init(~projector_to_term=Projector.Model.make_term);
};

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Action.t(Projector.Model.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = Action.project(Projector.Model.t);
};

module Ancestor = {
  include Ancestor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestor.t(Projector.Model.t);
};

module Ancestors = {
  include Ancestors;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Ancestors.t(Projector.Model.t);
};

module Backpack = {
  include Backpack;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Backpack.t(Projector.Model.t);
};

module Base = {
  include Base;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Base.projector(Projector.Model.t);
};

module Piece = {
  include Piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Piece.t(Projector.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tile = Piece.tile(Projector.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Piece.projector(Projector.Model.t);
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
  type t = Segment.t(Projector.Model.t);
};

module Selection = {
  include Selection;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Selection.t(Projector.Model.t);
};

module Tile = {
  include Tile;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Tile.t(Projector.Model.t);
};

module Zipper = {
  include Zipper;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Zipper.t(Projector.Model.t);
};

module Indicated = {
  include Indicated;

  type piece = Indicated.piece(Projector.Model.t);
  let ci_of: (Zipper.t, Statics.Map.t) => option(Statics.Info.t) = Indicated.ci_of;
};
