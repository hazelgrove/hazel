// This file declares all intermediate data structure or types

// the intermediate type in extraction
type inter_t =
  // coresponding to types in hazel/ocaml
  | HOLE //a hole is an error because ocaml doesn't support
  | Bool
  | Integer
  | Float
  | List(inter_t)
  | Arrow(inter_t, inter_t) // a -> b
  | Sum(inter_t, inter_t)
  | Prod(inter_t, inter_t)
  // only for intermediate usage by extraction system
  | VAR(string, inter_t) //variable string, inter_type
  | STR // a string of ocaml code, don't need further extraction
  // for example, the ocaml string of let-line should be a STR
  | ERR(string) // error message, will always be passed on
  | TBD; // a placeholder for types we can't decide yet

// a packed type, <ocaml code string, inter_type>
type extract_t = (string, inter_t);

// the environment type, record the types of vairable in current environment
// <name, type> should be one variable
// We use list because it's easy to insert and read at the head
type variable_env = list((string, inter_t));

//==================================

// translate the inter_t to the corresponding string
let string_of_inter_t =
  fun
  | _ => "To implement";
