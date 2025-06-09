include Haz3lcorep;
open Util;

module rec Projector: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let mk:
      (
        ~copy_ed: Editor.Model.t => Editor.Model.t,
        ProjectorCore.Kind.t,
        Any.t,
        unit => option(Editor.Model.t)
      ) =>
      option(t);

    let get_kind: t => ProjectorCore.Kind.t;
    let get_shape:
      (Statics.Map.t, Dynamics.Map.t, Base.projector(t)) => ProjectorShape.t;
    let get_cached_term: t => Term.Any.t;
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

    let make_term: (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Any.t));

    let calculate: (~common: ProjectorInterface.common, Model.t) => Model.t;
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let get_cursor_info:
      (
        ~common: ProjectorInterface.common,
        ~inject: Update.t => Ui_effect.t(unit),
        ~read_only: bool,
        Model.t,
        t
      ) =>
      Cursor.t;
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
        ~handoff_map:
          Hashtbl.t(Id.t, (Ui_effect.t(unit), Ui_effect.t(unit))),
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
        ~ed_size=Editor.View.get_dimensions,
      );
    let get_kind = ProjectorCore.kind_of_model;

    let mk = ProjectorInit.init;

    let get_cached_term = (ProjectorCore.V(_, _, exp_cache)) =>
      Calc.get_saved_exc(exp_cache);
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = ProjectorCore.Update.t(Editor.Update.t);

    let update = (~common) =>
      ProjectorCore.Update.update(~common, ~update_ed=Editor.Update.update);

    let make_term = (~sort, model: Model.t): (Model.t, Calc.t(Any.t)) =>
      ProjectorInit.make_term(
        ~mk_term_ed=Editor.Update.make_term,
        ~sort,
        model,
      );

    let calculate = (~common) =>
      ProjectorCore.Update.calculate(
        ~calculate_ed=Editor.Update.calculate,
        ~common,
      );
  };

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = ProjectorCore.Focus.t(Editor.Focus.t);

    let get_cursor_info =
        (
          ~common: ProjectorInterface.common,
          ~inject: Update.t => Ui_effect.t(unit),
          ~read_only: bool,
          model: Model.t,
          focus: t,
        )
        : Cursor.t =>
      ProjectorCore.Focus.get_cursor_info(
        ~get_cursor_info_ed=Editor.Focus.get_cursor_info,
        ~common,
        ~inject,
        ~read_only,
        model,
        focus,
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
          ~handoff_map,
          m: ProjectorView.Model.projector_data(Model.t),
        )
        : (Web.Node.t, option(Web.Node.t)) =>
      ProjectorView.split_views(
        ~common,
        ~parent,
        ~inject,
        ~ed_str=Editor.View.print_string,
        ~mk_ed=Editor.Model.mk(~settings=common.settings),
        ~mk_term_ed=Editor.Update.make_term,
        ~calculate_ed=Editor.Update.calculate,
        ~view_ed=
          Editor.View.view(
            ~font_metrics=common.font_metrics,
            ~secondary_icons=common.secondary_icons,
          ),
        ~view_editable=Editor.View.view_editable,
        ~enter_ed=Editor.Focus.enter,
        ~focus,
        ~focussed,
        ~handoff_map,
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
    let get_trailing_hole_ctx: (t, Statics.Map.t) => option(Ctx.t);
    // [@deriving (show({with_path: false}), sexp, yojson)]
    // type persistent;
    // let persist: t => persistent;
    // let unpersist: persistent => t;
    let of_zipper: Zipper.t(Projector.Model.t) => t; // TODO: Replace with persistence logic

    let get_cached_term: t => Term.Any.t;

    let copy: t => t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t);

    let update: (~common: ProjectorInterface.common, t, Model.t) => Model.t;

    let make_term: (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Any.t));

    let calculate: (~common: ProjectorInterface.common, Model.t) => Model.t;

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
    // Thunked to make module "safe"
    let here: unit => t;

    let get_cursor_info:
      (
        ~common: ProjectorInterface.common,
        ~inject: Update.t => Ui_effect.t(unit),
        ~read_only: bool,
        Model.t,
        t
      ) =>
      Cursor.t;

    let enter:
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~focus: t => Ui_effect.t(unit),
        Direction.t,
        Model.t
      ) =>
      Ui_effect.t(unit);
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

    let get_dimensions: Model.t => Point.t;

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
        ~escape: Direction.t => Ui_effect.t(unit),
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
      |> Haz3lcorep.Editor.Model.mk;
    };

    let of_zipper = Haz3lcorep.Editor.Model.mk;

    let get_z = (m: t) => m |> Haz3lcorep.Editor.Model.get_z;

    let get_trailing_hole_ctx = Haz3lcorep.Editor.Model.trailing_hole_ctx;

    let get_cached_term = Haz3lcorep.Editor.Model.get_cached_term;

    let copy = Haz3lcorep.Editor.Model.copy;
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      Action.t(ProjectorCore.Kind.t, Projector.Model.t, Projector.Update.t);

    let update =
        (~common: ProjectorInterface.common, action: t, editor: Model.t) => {
      switch (
        Haz3lcorep.Editor.Update.update(
          ~settings=common.settings,
          ~projector_init=Projector.Model.mk(~copy_ed=Editor.Model.copy),
          ~update_projector=Projector.Update.update(~common),
          ~seg_of_projector=
            p =>
              Projector.Model.get_cached_term(p)
              |> ExpToSegment.any_to_segment(
                   ~settings=ExpToSegment.Settings.on,
                 ),
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

    let make_term = (~sort: Sort.t, m: Model.t) =>
      Haz3lcorep.Editor.Update.make_term(
        ~make_term_prj=Projector.Update.make_term,
        ~sort,
        m,
      );

    let calculate = (~common: ProjectorInterface.common, ed: Model.t): Model.t =>
      Haz3lcorep.Editor.Update.calculate(
        ~common,
        ~projector_init=Projector.Model.mk(~copy_ed=Editor.Model.copy),
        // TODO[Matt]: Ask andrew about whether this sort argument should be unused
        ~projector_to_term=
          (~sort as _, ~id as _, m) => Projector.Model.get_cached_term(m),
        ~shape_of_projector=Projector.Model.get_shape,
        ~seg_of_projector=
          p =>
            Projector.Model.get_cached_term(p)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        ~livelit_projectors=ProjectorCore.Kind.livelit_projectors,
        ~update_projector=Projector.Update.update(~common),
        ~calculate_projector=Projector.Update.calculate,
        ed,
      );

    let jump_to_tile_action = (tile, model: Model.t) =>
      switch (TileMap.find_opt(tile, Calc.get_saved_exc(model.syntax).tiles)) {
      | Some(_) => Some(Haz3lcorep.Action.Jump(TileId(tile)))
      | None => None
      };
  };

  let get_measured = (m: Model.t) => Calc.get_saved_exc(m.syntax).measured;
  let get_tiles = (m: Model.t) => Calc.get_saved_exc(m.syntax).tiles;

  module Focus = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = EditorView.Focus.t(Projector.Focus.t);

    let here = () => EditorView.Focus.Here;

    let get_cursor_info =
        (
          ~common: ProjectorInterface.common,
          ~inject: Update.t => Ui_effect.t(unit),
          ~read_only: bool,
          m: Model.t,
          f: t,
        )
        : Cursor.t =>
      EditorView.Focus.get_cursor_info(
        ~get_cursor_info_pr=Projector.Focus.get_cursor_info,
        ~common,
        ~inject,
        ~read_only,
        ~mk_projector=Projector.Model.mk(~copy_ed=Editor.Model.copy),
        ~make_term_prj=Projector.Update.make_term,
        ~get_kind=Projector.Model.get_kind,
        m,
        f,
      );

    let enter =
        (
          ~inject: Update.t => Ui_effect.t(unit),
          ~focus: t => Ui_effect.t(unit),
          dir: Direction.t,
          m: Model.t,
        ) => {
      EditorView.Focus.enter(~inject, ~focus, dir, m);
    };
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
        ~info_projector=ProjectorCore.Kind.Info,
      );

    let get_dimensions = (ed: Model.t) =>
      Haz3lcorep.Editor.Model.get_dimensions(ed);

    let print_string = (ed: Model.t) =>
      ed
      |> Haz3lcorep.Editor.Model.get_z
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
