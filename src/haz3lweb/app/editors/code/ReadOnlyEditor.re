open Virtual_dom.Vdom;
open Haz3lcore;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated:
    editor: Editor.t,
    // Calculated:
    statics: CachedStatics.statics,
  };

  let mk = editor => {editor, statics: CachedStatics.empty_statics};

  let mk_from_editor = (~settings, ~stitch, editor: Editor.t) => {
    let term =
      MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst |> stitch;
    let info_map =
      Interface.Statics.mk_map_ctx(settings, Builtins.ctx_init, term);
    let error_ids =
      Statics.Map.error_ids(editor.state.meta.term_ranges, info_map);
    {
      editor,
      statics: {
        term,
        info_map,
        error_ids,
      },
    };
  };

  let mk_from_exp = (~inline=false, term: Exp.t) => {
    ExpToSegment.exp_to_editor(term, ~inline)
    |> mk_from_editor(~stitch=x => x);
  };

  let get_elab = model => model.statics.term;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = PersistentZipper.t;
  let persist = model => model.editor.state.zipper |> PersistentZipper.persist;
  let unpersist = p =>
    p |> PersistentZipper.unpersist |> Editor.init |> mk_from_editor;
};

module View = {
  // There are no events for a read-only editor
  type event;

  let view =
      (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: Model.t) => {
    let code_text_view = Code.view(~globals, ~sort, model.editor);
    let statics_decos = {
      module Deco =
        Deco.Deco({
          let globals = globals;
          let editor = model.editor;
        });
      Deco.statics(model.statics.error_ids);
    };
    Node.div(
      ~attr=Attr.many([Attr.classes(["code-container"])]),
      [code_text_view] @ statics_decos @ overlays,
    );
  };
};
