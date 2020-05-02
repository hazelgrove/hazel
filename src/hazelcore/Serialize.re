open Sexplib;

let string_of_exp = e => {
  Sexp.to_string(UHExp.sexp_of_t(e));
};
