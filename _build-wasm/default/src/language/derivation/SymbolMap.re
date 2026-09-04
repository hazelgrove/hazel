/**
  Canonical spec-variable names used across [RuleSpec.re].

  The [M] functor lets each client decide how to represent a symbol: the spec
  itself uses [DrvGrammar.M]-level [Var]s, while [RuleFormula] uses its own
  [LookUp*] constructors so the verifier can look matches up by name.

  Naming convention:
  - [e, e1, …]           expression
  - [v, v1, …]           value
  - [t, t1, …, t_in, …]  type
  - [n, n1, …]           numeric literal
  - [x, y]               pattern
  - [gamma, delta]       context
  - [a, b, c]            propositional atoms
  - [tpat]               type pattern
 */
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = string;

module type Wrapper = {
  type exp;
  type pat;
  type typ;
  type tpat;
  let exp: key => exp;
  let pat: key => pat;
  let typ: key => typ;
  let tpat: key => tpat;
};

module M = (W: Wrapper) => {
  let e = "e" |> W.exp;
  let e' = "e'" |> W.exp;
  let e_def = "e_def" |> W.exp;
  let e_body = "e_body" |> W.exp;
  let e_body' = "e_body'" |> W.exp;
  let e1 = "e1" |> W.exp;
  let e1' = "e1'" |> W.exp;
  let e2 = "e2" |> W.exp;
  let e2' = "e2'" |> W.exp;
  let e3 = "e3" |> W.exp;
  let v = "v" |> W.exp;
  let v_def = "v_def" |> W.exp;
  let v' = "v'" |> W.exp;
  let v1 = "v1" |> W.exp;
  let v2 = "v2" |> W.exp;
  let v3 = "v3" |> W.exp;
  let t = "t" |> W.typ;
  let t' = "t'" |> W.typ;
  let t_def = "t_def" |> W.typ;
  let t_body = "t_body" |> W.typ;
  let t_body' = "t_body'" |> W.typ;
  let t_in = "t_in" |> W.typ;
  let t_out = "t_out" |> W.typ;
  let t_in' = "t_in'" |> W.typ;
  let t_out' = "t_out'" |> W.typ;
  let t1 = "t1" |> W.typ;
  let t2 = "t2" |> W.typ;
  let t3 = "t3" |> W.typ;
  let t1' = "t1'" |> W.typ;
  let t2' = "t2'" |> W.typ;
  let n = "n" |> W.exp;
  let n' = "n'" |> W.exp;
  let n1 = "n1" |> W.exp;
  let n2 = "n2" |> W.exp;
  let n3 = "n3" |> W.exp;
  let tpat = "a" |> W.tpat;
  let a = "A" |> W.exp;
  let b = "B" |> W.exp;
  let c = "C" |> W.exp;
  let x = "x" |> W.pat;
  let ex = "x" |> W.exp;
  let y = "y" |> W.pat;
  let gamma = "gamma" |> W.exp;
  let gamma' = "gamma'" |> W.exp;
  let gamma'' = "gamma''" |> W.exp;
  let delta = "delta" |> W.exp;
  let delta' = "delta'" |> W.exp;
};
