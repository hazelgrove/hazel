open Util.Web;
open Haz3lcore;

/* Read-only code viewer with statics, but no interaction. Notably,
   since there is no interaction, the user can see that there is an
   error but cannot select the error for more details. */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Editor.t;

  let mk = editor => editor;

  let mk_from_exp = (~inline=false, term: Exp.t) => {
    ExpToSegment.exp_to_segment(term, ~inline)
    |> Zipper.unzip
    |> Editor.init
    |> mk;
  };

  let get_term = (model: t) => model.state.meta.statics.term;

  let get_statics = (model: t) => model.state.meta.statics.info_map;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = (model: t) => model.state.zipper |> PersistentZipper.persist;
  let unpersist = p => p |> PersistentZipper.unpersist |> Editor.init |> mk;
};

module Update = {
  // There are no events for a read-only editor
  type t;
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let {
      state:
        {
          meta:
            {
              syntax: {measured, selection_ids, segment, holes, _},
              statics: {info_map, _},
              _,
            },
          _,
        },
      _,
    }: Editor.t = model;
    let code_text_view =
      CodeViewable.view(
        ~globals,
        ~sort,
        ~measured,
        ~buffer_ids=selection_ids,
        ~segment,
        ~holes,
        ~info_map,
      );
    let statics_decos = {
      module Deco =
        Deco.Deco({
          let globals = globals;
          let editor = model;
        });
      Deco.statics();
    };
    div_c("code-container", [code_text_view] @ statics_decos @ overlays);
  };
};
