[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  form: ShardForm.t,
};

let id_ = s => s.id;
let form_ = s => s.form;

let nibs = s => s.form.mold.nibs;

let map_form = (f, {id, form}) => {id, form: f(form)};
