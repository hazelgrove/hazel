[@deriving sexp]
type edit_state = {
  prelude: UHExp.t,
  template: ZExp.t,
  tester: UHExp.t,
  id_gen: IDGen.t,
  ty: HTyp.t,
};

/**
 * The typing mode for some subexpression in the program
 */
type type_mode =
  | Syn
  | Ana(HTyp.t);

/*
  Split edit state into three parts, prelude, template, and tester.
  We split based on the presence of a #START_TEMPLATE opening comment and a #END_TEMPLATE opening comment.
  Assumption: both start and end comment appear exactly once, and in the right order
 */
let split_edit_states = (edit_state: edit_state): edit_state => {
  let (prelude, rest) =
    ListUtil.split_at(
      ZExp.erase(edit_state.template),
      UHExp.CommentLine("START_TEMPLATE"),
    );
  let (template, tester) =
    ListUtil.split_at(rest, UHExp.CommentLine("END_TEMPLATE"));
  //TODO preserve cursor location
  {...edit_state, prelude, template: ZExp.place_before(template), tester};
};

// Merge split edit states into one, and re insert comments
let combine_to_template = (edit_state: edit_state): edit_state => {
  let new_prelude =
    [UHExp.CommentLine("START_TEMPLATE")] @ edit_state.prelude;
  let new_tester =
    if (edit_state.tester == UHExp.empty_block) {
      [UHExp.CommentLine("END_TEMPLATE")]
      @ UHExp.Block.wrap(UHExp.EmptyHole(0));
    } else {
      [UHExp.CommentLine("END_TEMPLATE")] @ edit_state.tester;
    };
  let prepended = ZExp.prepend(new_prelude, edit_state.template);
  let new_template = ZExp.append(new_tester, prepended);
  {
    prelude: UHExp.empty_block,
    template: new_template,
    tester: UHExp.empty_block,
    id_gen: IDGen.init,
    ty: Hole,
  };
};

// combine edit states into one zexp
let merge_edit_states = (state: edit_state): ZExp.t => {
  let prepended = ZExp.prepend(state.prelude, state.template);
  ZExp.append(state.tester, prepended);
};

// convert new edit state into the old edit state that the evaluator uses
let get_template_edit_state = (state: edit_state): (ZExp.t, HTyp.t, IDGen.t) => {
  (state.template, state.ty, state.id_gen);
};
