open Example;
open ExplainThisForm;

/* `(tpat)` — parens around a type pattern. Transparent: the inner
   tpat is the actual binder. The cursor inspector generally shows
   the inner node's info, so this group is only used when cursor is
   on the parens themselves. */

let _inner = tpat("a");

let parens_tpat_form: form = {
  let explanation = "Parentheses around a type pattern. They have no semantic effect — the inner type pattern is the binder.";
  {
    id: ParensTPat,
    syntactic_form: [_inner],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let parens_tpats: group = {
  id: ParensTPat,
  forms: [parens_tpat_form],
};
