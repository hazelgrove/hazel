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
   We split based on the presence of two comments. TODO specify these comments.
 */
let split_edit_states = (edit_state: edit_state): edit_state => {};

let combine_to_template = (edit_state: edit_state): edit_state => {};

let merge_edit_states = (state: edit_state): ZExp.t => {
  let unzipped_template = ZExp.erase(state.template);

  // TODO: place cursor where it was before
  ZExp.place_before(state.prelude @ unzipped_template @ state.tester);
};

let wrap_edit_state = (state: edit_state): (ZExp.t, HTyp.t, IDGen.t) => {
  (merge_edit_states(state), state.ty, state.id_gen);
};
