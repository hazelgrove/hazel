open Sexplib.Std;

[@deriving sexp]
type t = int;

let recent = (var_1: t, var_2: t) => {
  max(var_1, var_2);
};

let type_variable = ref(0);

let gen_new_type_var = () => {
  let var = type_variable^;
  incr(type_variable);
  var;
};

let reset_type_var = () => {
  type_variable := 0;
};
