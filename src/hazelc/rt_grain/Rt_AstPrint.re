/**
  "ast_print.gr", handwritten.
 */
open Grain;
open Grain.Ident;
open Rt_.Stub({
       let path = "./ast_print" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Use = (I: Make.I) => {
  include Use(I);

  let imp = imp;

  let print = ap1(v("print"));
};
