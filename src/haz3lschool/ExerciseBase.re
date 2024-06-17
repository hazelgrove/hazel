open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = (string, int);

module TermItem = {
  type t = {
    term: TermBase.UExp.t,
    term_ranges: TermRanges.t,
  };
};

module StaticsItem = {
  type t = CachedStatics.statics;
};

module DynamicsItem = {
  type t = {
    term: TermBase.UExp.t,
    info_map: Statics.Map.t,
    result: ModelResult.t,
  };
  let empty: t = {
    term: {
      term: Tuple([]),
      ids: [Id.mk()],
    },
    info_map: Id.Map.empty,
    result: NoElab,
  };
  let statics_only = ({term, info_map, _}: StaticsItem.t): t => {
    {term, info_map, result: NoElab};
  };
};

// # Stitching

let wrap_filter = (act: FilterAction.action, term: Term.UExp.t): Term.UExp.t =>
  TermBase.UExp.{
    term:
      TermBase.UExp.Filter(
        FilterAction.(act, One),
        {term: Constructor("$e"), ids: [Id.mk()]},
        term,
      ),
    ids: [Id.mk()],
  };

let wrap = (term, editor: Editor.t): TermItem.t => {
  term,
  term_ranges: editor.state.meta.term_ranges,
};

let term_of = (editor: Editor.t): Term.UExp.t => editor.state.meta.view_term;

let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
  EditorUtil.append_exp(
    EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
    term_of(ed3),
  );
