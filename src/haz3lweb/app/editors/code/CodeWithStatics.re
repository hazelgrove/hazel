// open Util.Web;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.Model.t,
    statics: CachedStatics.t,
    dynamics: Dynamics.Map.t,
  };

  let mk = editor => {
    editor,
    statics: CachedStatics.empty,
    dynamics: Dynamics.Map.empty,
  };

  let mk_from_exp = (~settings: CoreSettings.t, ~inline=false, term: Exp.t) => {
    Editor.Model.mk(~settings, ~inline, Exp(term)) |> mk;
  };

  let get_statics = (model: t) => model.statics;

  let get_dynamics = (model: t) => model.dynamics;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = (model: t) =>
    model.editor |> Editor.Model.get_z |> PersistentZipper.persist;
  let to_string = (model: t) =>
    model.editor |> Editor.Model.get_z |> PersistentZipper.to_string;
  let unpersist = p =>
    p |> PersistentZipper.unpersist |> Editor.Model.of_zipper |> mk;
};

module Update = {
  // There are no events for a read-only editor
  type t;

  /* Calculates the statics for the editor. */
  let calculate =
      (
        ~globals: Globals.t,
        ~stitch,
        ~dynamics: Dynamics.Map.t,
        ~is_dynamic_term,
        {editor, statics, dynamics: _}: Model.t,
      )
      : Model.t => {
    let (editor, term) = Editor.Update.make_term(~sort=Exp, editor);
    let statics =
      switch (term) {
      | NewValue(term) =>
        CachedStatics.init_from_term(
          ~settings=globals.settings.core,
          ~is_dynamic_term,
          term |> Any.is_exp |> Option.get |> stitch,
        )
      | OldValue(_) => statics
      };
    let editor =
      Editor.Update.calculate(
        ~common=
          ProjectorInterface.{
            settings: globals.settings.core,
            font_metrics: globals.font_metrics,
            secondary_icons: globals.settings.secondary_icons,
            show_backpack_targets: globals.show_backpack_targets,
            color_highlights: globals.color_highlights,
            statics,
            dynamics,
          },
        editor,
      );
    {
      editor,
      statics,
      dynamics,
    };
  };
};

// module View = {
//   // There are no events for a read-only editor
//   type event;

//   let view =
//       (
//         ~globals: Globals.t,
//         ~overlays: list(Node.t)=[],
//         ~sort=Sort.root,
//         model: Model.t,
//       ) => {
//     let {editor, _}: Model.t = model;
//     let code_text_view =
//       Editor.View.view(
//         ~secondary_icons=globals.settings.secondary_icons,
//         ~font_metrics=globals.font_metrics,
//         ~sort,
//         editor,
//       );
//     let statics_decos = {
//       module Deco =
//         Deco.Deco({
//           type projector_kind = ProjectorCore.Kind.t;
//           type projector = Projector.Model.t;
//           let globals =
//             ProjectorInterface.{
//               settings: globals.settings.core,
//               font_metrics: globals.font_metrics,
//               secondary_icons: globals.settings.secondary_icons,
//               show_backpack_targets: globals.show_backpack_targets,
//               color_highlights: globals.color_highlights,
//               statics: model.statics,
//               dynamics: model.dynamics,
//             };
//           let editor = model.editor;
//         });
//       Deco.statics();
//     };
//     div_c("code-container", [code_text_view] @ statics_decos @ overlays);
//   };
// };
