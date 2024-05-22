open Virtual_dom.Vdom;
open Util.Result.Syntax;
open Haz3lcore;

type model = {
  // Update:
  editor: Editor.t,
  // Calculate:
  statics: CachedStatics.statics,
};

type action = Action.t;

let update = (~settings, action, model) => {
  let+ editor = Perform.go(~settings, action, model.editor);
  {editor, statics: model.statics};
};

let calculate = (~settings, ~stitch, {editor, statics} as model) => {
  let term = MakeTerm.from_zip_for_sem(editor.state.zipper) |> fst |> stitch;
  if (Exp.fast_equal(term, statics.term)) {
    model;
  } else {
    {
      editor,
      statics: CachedStatics.statics_of_term(~settings, term, editor),
    };
  };
};

// There are no events for a read-only editor
type event;

// read-only editors cannot be selected
type selection;

let view =
    (~globals, ~overlays: list(Node.t)=[], ~sort=Sort.root, model: model) => {
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
