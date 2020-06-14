/*  This file will contain some tool functions to make life better
       The functions are a combination of infer functions
    */
open Extraction_types;
//open Extraction_infer;

// read an variable and give the extract_t of annotated ocaml format
let variable_annotate = (~v: string, ~env: variable_env): extract_t =>
  switch (v, env) {
  | _ => ("", TBD)
  };

// infer an unknown variable with a term and update the environment
// If t is also a variable, we further update t
// v should be exactly an variable
let pass_infer_update =
    (~v: inter_t, ~t: inter_t, env: variable_env): (inter_t, variable_env) =>
  switch (v, t, env) {
  | _ => (TBD, env)
  };

// similar function, but we know require some specific term
let require_infer_update =
    (~v: inter_t, ~require: inter_t, ~env: variable_env)
    : (inter_t, variable_env) =>
  switch (v, require, env) {
  | _ => (TBD, env)
  };
