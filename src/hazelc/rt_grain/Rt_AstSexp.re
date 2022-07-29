/**
  "ast_sexp.gr", handwritten.
 */
open Rt_.WithoutImpl({
       let name = "AstSexp";
       let path = "ast_sexp";
     });
/**
  Import statement.
 */
let import = import;

/**
  Implementation module.
 */
let impl_md = impl_md;

let sexp_of_ast = ap1("sexpOfAst");
