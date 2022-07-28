/**
  "ast_sexp.gr", handwritten.
 */
open Grain;
open Grain.Ident;
open Rt_.Stub({
       let path = "ast_sexp" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Use = (I: Make.I) => {
  open Use(I);

  let imp = imp;

  let sexp_of_ast = ap1(v("sexpOfAst"));
};
