include Haz3lcorep;
open Util;

module rec Projector: {
  module Model: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let mk:
      (ProjectorKind.t, Language.Any.t, unit => option(Editor.Model.t)) =>
      option(t);

    let get_kind: t => ProjectorKind.t;
    let get_cached_term: t => Language.Term.Any.t;
  };

  module Update: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let update:
      (~common: Common.t, ~sort: Sort.t, ~id: Id.t, t, Model.t) => Model.t;

    let make_term:
      (~sort: Sort.t, Model.t) => (Model.t, Calc.t(Language.Any.t));

    let calculate: (~common: Common.t, Model.t) => Model.t;
  };

  module Focus: {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t;

    let get_cursor_info:
      (
        ~common: Common.t,
        ~inject: Update.t => Ui_effect.t(unit),
        ~read_only: bool,
        Model.t,
        t
      ) =>
      Cursor.t;
  };

  module View: {
    let get_placeholder:
      (~common: Common.t, Base.projector(Model.t)) => ProjectorShape.t;

    let mk_status:
      (
        Base.projector(Model.t),
        ~common: Common.t,
        ~editor_active: bool,
        ~indicated: option((Id.t, Direction.t)),
        ~selection_ids: list(Id.t),
        ~id: Id.t
      ) =>
      ProjectorView.Model.status;

    let view:
      (
        ~common: Common.t,
        ~inject: Update.t => Ui_effect.t(unit),
        ~escape: ProjectorInterface.external_action => Ui_effect.t(unit),
        ~take_focus: Focus.t => Ui_effect.t(unit),
        ~focus: option(Focus.t),
        ~id: Id.t,
        Model.t
      ) =>
      ProjectorInterface.View.t;
  };
} = {
  module Model = {
    type t =
      Haz3lcorep.Projector.model(
        Editor.Model.t,
        Editor.Update.t,
        Editor.Focus.t,
      );
    let pp = Haz3lcorep.Projector.pp_model;
    let show = Format.asprintf("%a", pp);
    let t_of_sexp =
      Haz3lcorep.Projector.model_of_sexp(~editor_module=(module Editor));
    let sexp_of_t =
      Haz3lcorep.Projector.sexp_of_model(~editor_module=(module Editor));
    let yojson_of_t =
      Haz3lcorep.Projector.yojson_of_model(~editor_module=(module Editor));
    let t_of_yojson =
      Haz3lcorep.Projector.model_of_yojson(~editor_module=(module Editor));

    let get_kind = Haz3lcorep.Projector.kind_of_model;

    let mk = Haz3lcorep.Projector.init(~editor_module=(module Editor));

    let get_cached_term = (Haz3lcorep.Projector.V(_, _, exp_cache)) =>
      Calc.get_saved_exc(exp_cache);
  };

  module Update = {
    type t =
      Haz3lcorep.Projector.action(
        Editor.Model.t,
        Editor.Update.t,
        Editor.Focus.t,
      );
    let pp = Haz3lcorep.Projector.pp_action;
    let show = Format.asprintf("%a", pp);
    let t_of_sexp =
      Haz3lcorep.Projector.action_of_sexp(~editor_module=(module Editor));
    let sexp_of_t =
      Haz3lcorep.Projector.sexp_of_action(~editor_module=(module Editor));
    let yojson_of_t =
      Haz3lcorep.Projector.yojson_of_action(~editor_module=(module Editor));
    let t_of_yojson =
      Haz3lcorep.Projector.action_of_yojson(~editor_module=(module Editor));

    let update = (~common) =>
      Haz3lcorep.Projector.update(~editor_module=(module Editor), ~common);

    let make_term =
        (~sort, model: Model.t): (Model.t, Calc.t(Language.Any.t)) =>
      Haz3lcorep.Projector.make_term(
        ~editor_module=(module Editor),
        ~sort,
        model,
      );

    let calculate = (~common) =>
      Haz3lcorep.Projector.calculate(~editor_module=(module Editor), ~common);
  };

  module Focus = {
    type t =
      Haz3lcorep.Projector.focus(
        Editor.Model.t,
        Editor.Update.t,
        Editor.Focus.t,
      );
    let pp = Haz3lcorep.Projector.pp_focus;
    let show = Format.asprintf("%a", pp);
    let t_of_sexp =
      Haz3lcorep.Projector.focus_of_sexp(~editor_module=(module Editor));
    let sexp_of_t =
      Haz3lcorep.Projector.sexp_of_focus(~editor_module=(module Editor));
    let yojson_of_t =
      Haz3lcorep.Projector.yojson_of_focus(~editor_module=(module Editor));
    let t_of_yojson =
      Haz3lcorep.Projector.focus_of_yojson(~editor_module=(module Editor));

    let get_cursor_info =
        (
          ~common: Common.t,
          ~inject: Update.t => Ui_effect.t(unit),
          ~read_only: bool,
          model: Model.t,
          focus: t,
        )
        : Cursor.t =>
      Haz3lcorep.Projector.get_cursor_info(
        ~editor_module=(module Editor),
        ~common,
        ~inject,
        ~read_only,
        model,
        focus,
      );
  };

  module View = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    let get_placeholder =
      Haz3lcorep.Projector.placeholder(~editor_module=(module Editor));

    let mk_status = ProjectorView.Model.mk_status;

    let view =
        (
          ~common: Common.t,
          ~inject: Update.t => Ui_effect.t(unit),
          ~escape: ProjectorInterface.external_action => Ui_effect.t(unit),
          ~take_focus: Focus.t => Ui_effect.t(unit),
          ~focus: option(Focus.t),
          ~id: Id.t,
          m: Model.t,
        ) =>
      Haz3lcorep.Projector.view(
        ~editor_module=(module Editor),
        ~common,
        ~inject,
        ~escape,
        ~take_focus,
        ~focus,
        ~id,
        m,
      );
  };
}
and Editor: {
  include
    EditorInterface.EDITOR with
      type model =
        Haz3lcorep.Editor.t(
          ProjectorKind.t,
          Projector.Model.t,
          Projector.Update.t,
        ) and
      type action =
        Action.t(ProjectorKind.t, Projector.Model.t, Projector.Update.t);
  // model and action have transparent definitions for handing editor to projectorinit

  // TODO: refactor these helper functions away
  let get_measured: Model.t => Measured.t;
  let get_tiles: Model.t => TileMap.t(Projector.Model.t);
  let get_z: Model.t => Haz3lcorep.Zipper.t(Projector.Model.t);
  let of_zipper: Zipper.t(Projector.Model.t) => Model.t; // TODO: Replace with persistence logic
  let get_trailing_hole_ctx:
    (Model.t, Language.Statics.Map.t) => option(Language.Ctx.t);
} = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    Haz3lcorep.Editor.t(
      ProjectorKind.t,
      Projector.Model.t,
      Projector.Update.t,
    );
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    Action.t(ProjectorKind.t, Projector.Model.t, Projector.Update.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus = EditorView.Focus.t(Projector.Focus.t);

  module Model = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = model;

    let mk = (~inline=false, term: Language.Any.t): t => {
      ExpToSegment.any_to_segment(
        term,
        ~settings=ExpToSegment.Settings.editable(~inline),
      )
      |> Haz3lcorep.Zipper.unzip
      |> Haz3lcorep.Editor.Model.mk;
    };

    let split = Haz3lcorep.Editor.Model.split;

    let get_z = (m: t) => m |> Haz3lcorep.Editor.Model.get_z;

    let get_cached_term = Haz3lcorep.Editor.Model.get_cached_term;

    let copy = Haz3lcorep.Editor.Model.copy;
  };

  module Update = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = action;

    let update = (~common: Common.t, action: t, editor: Model.t) => {
      switch (
        Haz3lcorep.Editor.Update.update(
          ~settings=common.settings,
          ~projector_init=Projector.Model.mk,
          ~update_projector=Projector.Update.update(~common),
          ~seg_of_projector=
            p =>
              Projector.Model.get_cached_term(p)
              |> ExpToSegment.any_to_segment(
                   ~settings=ExpToSegment.Settings.on,
                 ),
          ~livelit_projectors=ProjectorKind.livelit_projectors,
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

    let calculate = (~common: Common.t, ed: Model.t): Model.t =>
      Haz3lcorep.Editor.Update.calculate(
        ~common,
        ~projector_init=Projector.Model.mk,
        // TODO[Matt]: Ask andrew about whether this sort argument should be unused
        ~projector_to_term=
          (~sort as _, ~id as _, m) => Projector.Model.get_cached_term(m),
        ~shape_of_projector=Projector.View.get_placeholder,
        ~seg_of_projector=
          p =>
            Projector.Model.get_cached_term(p)
            |> ExpToSegment.any_to_segment(~settings=ExpToSegment.Settings.on),
        ~livelit_projectors=ProjectorKind.livelit_projectors,
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
    type t = focus;
    let here = () => EditorView.Focus.Here;

    let get_cursor_info =
        (
          ~common: Common.t,
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
        ~mk_projector=Projector.Model.mk,
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
    let view =
        (
          ~font_metrics,
          ~secondary_icons,
          ~sort,
          ~background=false,
          m: Model.t,
        ) =>
      CodeViewable.view_editor(
        ~font_metrics,
        ~secondary_icons,
        ~sort,
        ~background,
        m,
      );

    let view_editable = (~common) =>
      EditorView.view_code_editable(
        ~common,
        ~view_projector=Projector.View.view,
        ~mk_status=Projector.View.mk_status,
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

    let term = (~common: Common.t, term: Language.Any.t) => {
      let sort = Language.Any.sort(term);
      let ed =
        term
        |> Editor.Model.mk
        |> Editor.Update.make_term(~sort)
        |> fst
        |> Editor.Update.calculate(~common);
      (
        Editor.View.view(
          ~font_metrics=common.font_metrics,
          ~secondary_icons=common.secondary_icons,
          ~sort,
          ed,
        ),
        Point.{
          row: ed |> Editor.View.print_string |> String.length,
          col: 1,
        },
      );
    };
  };

  let get_z = Model.get_z;
  let of_zipper = Haz3lcorep.Editor.Model.mk;

  let get_trailing_hole_ctx = Haz3lcorep.Editor.Model.trailing_hole_ctx;
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
  type t = Action.t(ProjectorKind.t, Projector.Model.t, Projector.Update.t);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project =
    Action.project(ProjectorKind.t, Projector.Model.t, Projector.Update.t);
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
  let ci_of:
    (Zipper.t, Language.Statics.Map.t) => option(Language.Statics.Info.t) = Indicated.ci_of;
};
