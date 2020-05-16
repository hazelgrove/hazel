// this file is the functions related to inference or environment
open Extraction_types;

// This function applies whenever we know two types should have same type
// The return value is the inter_t inferred, will try best-effort to infer TBD & detect error
let pass_infer = (~t1: inter_t, ~t2: inter_t): inter_t =>
  switch (t1, t2) {
  | _ => STR
  };

// The function to infer a inter_t whenever an argument is given to somewhere required
// The return value is the inter_t inferred, will try best-effort to infer TBD & detect error
// require should be exactly some type besides TBD
let require_infer = (~given: inter_t, ~require: inter_t): inter_t =>
  switch (given, require) {
  | _ => STR
  };

// Find a variable in the environment
let find_variable = (~v: string, ~env: variable_env): inter_t =>
  switch (v, env) {
  | _ => ERR("variable can't find")
  };

// Add or overwrite an variable in the environment
// v should be VAR(_, _) type
let add_variable = (~v: inter_t, ~env: variable_env): variable_env =>
  switch (v) {
  | _ => env
  };
