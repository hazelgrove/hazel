open Util.Web;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    // Calculated:
    statics: CachedStatics.statics,
  };

  // Note: statics aren't calculated until `calculate` is run!
  let mk = editor => {editor, statics: CachedStatics.empty_statics};

  let mk_from_exp = (~inline=false, term: Exp.t) => {
    ExpToSegment.exp_to_editor(term, ~inline) |> mk;
  };

  let get_term = model => model.statics.term;

  let get_statics = model => model.statics.info_map;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = model => model.editor.state.zipper |> PersistentZipper.persist;
  let unpersist = p => p |> PersistentZipper.unpersist |> Editor.init |> mk;
};

module Update = {
  // There are no events for a read-only editor
  type t;

  /* Calculates the statics for the editor.
     Interface.Statics.mk_map_ctx handles memoization */
  let calculate = (~settings, ~stitch, model: Model.t): Model.t => {
    let term =
      MakeTerm.from_zip_for_sem(model.editor.state.zipper) |> fst |> stitch;
    let info_map =
      Interface.Statics.mk_map_ctx(settings, Builtins.ctx_init, term);
    let error_ids =
      Statics.Map.error_ids(
        model.editor.state.meta.projected.term_ranges,
        info_map,
      );
    {
      editor: model.editor,
      statics: {
        term,
        info_map,
        error_ids,
      },
    };
  };
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let code_text_view = CodeViewable.view(~globals, ~sort, model.editor);
    let statics_decos = {
      module Deco =
        Deco.Deco({
          let globals = globals;
          let editor = model.editor;
        });
      Deco.statics(model.statics.error_ids);
    };
    div_c("code-container", [code_text_view] @ statics_decos @ overlays);
  };
};
