open Sexplib.Sexp;
open Sexplib0;

let of_sexp_error = (what, sexp) =>
  raise(Sexp.Of_sexp_error(Failure(what), sexp));

let sexp_of_result = (sexp_of__a, sexp_of__b, res) => {
  switch (res) {
  | Ok(x) => List([Atom("ok"), sexp_of__a(x)])
  | Error(err) => List([Atom("error"), sexp_of__b(err)])
  };
};

let result_of_sexp = (a__of_sexp, b__of_sexp, sexp) => {
  switch (sexp) {
  | List([Atom("ok" | "Ok"), el]) => Ok(a__of_sexp(el))
  | List([Atom("error" | "Error"), el]) => Error(b__of_sexp(el))
  | List(_) =>
    of_sexp_error("result_of_sexp: list must be (some el) or (erro el)", sexp)
  | Atom(_) => of_sexp_error("result_of_sexp: list needed", sexp)
  };
};
