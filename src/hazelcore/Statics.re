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
  TODO(ygaitonde)
  Split edit state into three parts, prelude, template, and tester.
  We split based on the presence of a #START_TEMPLATE opening comment and a #END_TEMPLATE opening comment.
  edge cases: neither present, one present but not the other, end template comment before start template comment
  Invariant: valid edit state to switch from instructor mode to student mode
 */
let split_edit_states = (edit_state: edit_state): edit_state => {
  // get index of start comment
  // get index of closing comment
  // if index of start comment is not < index of closing comment then the split fails
  // prelude is from start of list to start comment idx
  // template btwn start comment and closing comment idx
  // tester is from closing comment to end
  edit_state;
};

// Merge split edit states into one, and re insert comments
let combine_to_template = (edit_state: edit_state): edit_state => {
  let new_template_unzipped =
    edit_state.prelude
    @ [UHExp.CommentLine("START_TEMPLATE")]
    @ ZExp.erase(edit_state.template)
    @ [UHExp.CommentLine("END_TEMPLATE")]
    @ edit_state.tester;
  {
    prelude: [],
    template: ZExp.place_before(new_template_unzipped),
    tester: [],
    id_gen: IDGen.init,
    ty: Hole,
  };
};

// combine edit states into one zexp
let merge_edit_states = (state: edit_state): ZExp.t => {
  let unzipped_template = ZExp.erase(state.template);

  // TODO: place cursor where it was before
  ZExp.place_before(state.prelude @ unzipped_template @ state.tester);
};

// convert new edit state into the old edit state that the evaluator uses
let wrap_edit_state = (state: edit_state): (ZExp.t, HTyp.t, IDGen.t) => {
  (merge_edit_states(state), state.ty, state.id_gen);
};
