include Haz3lcorep;
open Util;

module rec Projector: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let mk:
      (ProjectorCore.Kind.t, Any.t, unit => option(Editor.Model.t)) =>
      option(t);

    let get_kind: t => ProjectorCore.Kind.t;
    let get_shape:
      (Statics.Map.t, Dynamics.Map.t, Base.projector(t)) => ProjectorShape.t;
    let get_focusable: t => ProjectorBase.Focusable.t;
    let focusable_of_kind: ProjectorCore.Kind.t => ProjectorBase.Focusable.t;

    let make_term: (t, Sort.t) => Any.t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let update:
      (
        ~common: ProjectorInterface.common,
        ~sort: Sort.t,
        ~id: Id.t,
        t,
        Model.t
      ) =>
      Model.t;

    let calculate:
      (~common: ProjectorInterface.common, ~sort: Sort.t, Model.t) => Model.t;
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let handle_key_event:
      (~focus: t, ~key: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    let split_views:
      (
        ~common: ProjectorInterface.common,
        ~sort: Sort.t,
        ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        ProjectorView.Model.projector_data(Model.t)
      ) =>
      (Web.Node.t, option(Web.Node.t));

    let mk_status:
      (
        Base.projector(Model.t),
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
    type t =
      ProjectorCore.model(Editor.Model.t, Editor.Update.t, Editor.Focus.t);

    let get_shape =
      Haz3lcorep.ProjectorInfo.ShapeMapSemantics.from_semantics(
        ~ed_str=Editor.View.print_string,
      );
    let get_kind = ProjectorCore.kind_of_model;
    let get_focusable = ProjectorInit.focusable_of_model;
    let focusable_of_kind = ProjectorInit.focusable_of_kind;

    //TODO(andrew): proper sort for deco

    let make_term =
      ProjectorInit.make_term(~term_of_ed=Editor.Model.make_term);

    let mk = ProjectorInit.init;
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = ProjectorCore.Update.t(Editor.Update.t);

    let update = (~common) =>
      ProjectorCore.Update.update(
        ~common,
        ~update_ed=Editor.Update.update(~common),
      );

    let calculate = (~common) =>
      ProjectorCore.Update.calculate(
        ~calculate_ed=Editor.Update.calculate(~common, ~is_edited=true),
        ~common,
      );
  };

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = ProjectorCore.Focus.t(Editor.Focus.t);

    let handle_key_event =
      ProjectorCore.Focus.handle_key_event(
        ~handle_key_ed=Editor.Focus.handle_key_event,
      );
  };

  module View = {
    let split_views =
        (
          ~common: ProjectorInterface.common,
          ~sort as _: Sort.t,
          ~parent: ProjectorBase.external_action => Ui_effect.t(unit),
          ~inject: Update.t => Ui_effect.t(unit),
          ~focus: Focus.t => Ui_effect.t(unit),
          ~focussed: option(Focus.t),
          m: ProjectorView.Model.projector_data(Model.t),
        )
        : (Web.Node.t, option(Web.Node.t)) =>
      ProjectorView.split_views(
        ~common,
        ~parent,
        ~inject,
        ~ed_str=Editor.View.print_string,
        ~mk_ed=Editor.Model.mk(~settings=common.settings),
        ~view_ed=
          Editor.View.view(
            ~font_metrics=common.font_metrics,
            ~secondary_icons=common.secondary_icons,
          ),
        ~view_editable=Editor.View.view_editable,
        ~focus,
        ~focussed,
        m,
      );

    let mk_status = ProjectorView.Model.mk_status;
  };
}
and Editor: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      Haz3lcorep.Editor.t(
        ProjectorCore.Kind.t,
        Projector.Model.t,
        Projector.Update.t,
      ); // Transparent definition needed for handing editor to projectorinit

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
    type t =
      Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t);

    let update:
      (~common: ProjectorInterface.common, ~sort: Sort.t, t, Model.t) =>
      Model.t;

    let calculate:
      (
        ~common: ProjectorInterface.common,
        ~is_edited: bool,
        ~sort: Sort.t,
        Model.t
      ) =>
      Model.t;

    let key_handoff:
      (Model.t, Key.t) =>
      option(
        Action.project(
          ProjectorCore.Kind.t,
          Projector.Model.t,
          Projector.Update.t,
        ),
      );
    let jump_to_tile_action:
      (Id.t, Model.t) =>
      option(
        Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t),
      );
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    // TODO[Matt]: Used in jump to tile logic which will need updating.
    let here: t;

    let handle_key_event:
      (~focus: t, ~key: Key.t, Model.t) => option(Update.t);
  };

  module View: {
    let print_string: Model.t => string;

    let view:
      (
        ~font_metrics: FontMetrics.t,
        ~secondary_icons: bool,
        ~sort: Sort.t,
        Model.t
      ) =>
      Web.Node.t;

    let view_editable:
      (
        ~common: ProjectorInterface.common,
        ~inject:
          Action.t(
            ProjectorCore.Kind.t,
            Projector.Model.t,
            Projector.Update.t,
          ) =>
          Ui_effect.t(unit),
        ~focus: Focus.t => Ui_effect.t(unit),
        ~focussed: option(Focus.t),
        ~overlays: list(Web.Node.t)=?,
        ~sort: Sort.t,
        Model.t
      ) =>
      Web.Node.t;
  };

  // TODO: refactor these helper functions away

  let get_measured: Model.t => Measured.t;
  let get_tiles: Model.t => TileMap.t(Projector.Model.t);
} = {
  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      Haz3lcorep.Editor.t(
        ProjectorCore.Kind.t,
        Projector.Model.t,
        Projector.Update.t,
      );

    let mk = (~settings: CoreSettings.t, ~inline=false, term: Any.t): t => {
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

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t);

    let update =
        (
          ~common: ProjectorInterface.common,
          ~sort,
          action: t,
          editor: Model.t,
        ) => {
      switch (
        Haz3lcorep.Editor.Update.update(
          ~settings=common.settings,
          ~sort,
          ~projector_init=Projector.Model.mk,
          ~projector_to_term=Projector.Model.make_term,
          ~shape_of_projector=Projector.Model.get_shape,
          ~update_projector=Projector.Update.update(~common),
          ~seg_of_projector=
            (sort, p) =>
              Projector.Model.make_term(p, sort)
              |> ExpToSegment.any_to_segment(
                   ~settings=ExpToSegment.Settings.on,
                 ),
          ~get_focusable=Projector.Model.focusable_of_kind,
          ~livelit_projectors=ProjectorCore.Kind.livelit_projectors,
          action,
          common.statics,
          editor,
        )
      ) {
      | Ok(editor) => editor
      | Error(e) => raise(Failure.Exception(e))
      };
    };

    let calculate =
        (
          ~common: ProjectorInterface.common,
          ~is_edited: bool,
          ~sort: Sort.t,
          ed: Model.t,
        )
        : Model.t =>
      Haz3lcorep.Editor.Update.calculate(
        ~common,
        ~settings=common.settings,
        ~is_edited,
        ~projector_init=Projector.Model.mk,
        ~projector_to_term=Projector.Model.make_term,
        ~shape_of_projector=Projector.Model.get_shape,
        ~seg_of_projector=
          (sort, p) =>
            Projector.Model.make_term(p, sort)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        ~get_focusable=Projector.Model.focusable_of_kind,
        ~livelit_projectors=ProjectorCore.Kind.livelit_projectors,
        ~update_projector=Projector.Update.update(~common),
        ~calculate_projector=Projector.Update.calculate,
        ~sort,
        common.statics,
        common.dynamics,
        ed,
      );

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
        : option(
            Action.project(
              ProjectorCore.Kind.t,
              Projector.Model.t,
              Projector.Update.t,
            ),
          ) => {
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

  let get_measured = (m: Model.t) => m.syntax.measured;
  let get_tiles = (m: Model.t) => m.syntax.tiles;

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = EditorView.Focus.t(Projector.Focus.t);

    let here = EditorView.Focus.Here;

    let handle_key_event =
      EditorView.Focus.handle_key_event(
        ~handle_key_pr=Projector.Focus.handle_key_event,
        ~info_projector=ProjectorCore.Kind.Info: ProjectorCore.Kind.t,
      );
  };

  module View = {
    // TODO[Matt]: This should be the only function in view.
    let view = (~font_metrics, ~secondary_icons, ~sort, m: Model.t) =>
      CodeViewable.view_editor(~font_metrics, ~secondary_icons, ~sort, m);

    let view_editable = (~common) =>
      EditorView.view_code_editable(
        ~common,
        ~split_views=Projector.View.split_views(~common),
        ~mk_status=Projector.View.mk_status,
      );

    let print_string = (ed: Model.t) =>
      ed.state.zipper
      |> Zipper.zip
      |> Printer.of_segment(~holes=Some("?"))
      |> Re.Str.global_replace(Re.Str.regexp("\n"), " ")
      |> (
        str =>
          String.length(str) > 30 ? String.sub(str, 0, 30) ++ "..." : str
      );
  };
};

module PersistentZipper = {
  include PersistentZipper;
  // TODO: move these into Editor
  let persist = persist(Projector.Model.sexp_of_t);
  let unpersist = unpersist(Projector.Model.t_of_sexp);
};

/* Just types */

module Action = {
  include Action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project =
    Action.project(
      ProjectorCore.Kind.t,
      Projector.Model.t,
      Projector.Update.t,
    );
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

module Piece = {
  include Piece;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Piece.t(Projector.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tile = Piece.tile(Projector.Model.t);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type projector = Piece.projector(Projector.Model.t);
};

module Segment = {
  include Segment;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Segment.t(Projector.Model.t);
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
