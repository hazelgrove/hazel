open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = string;

module type Wrapper = {
  type target;
  let f: key => target;
};

module M = (W: Wrapper) => {
  let e = "e" |> W.f;
  let e' = "e'" |> W.f;
  let e_def = "e_def" |> W.f;
  let e_body = "e_body" |> W.f;
  let e_body' = "e_body'" |> W.f;
  let e1 = "e1" |> W.f;
  let e1' = "e1'" |> W.f;
  let e2 = "e2" |> W.f;
  let e2' = "e2'" |> W.f;
  let e3 = "e3" |> W.f;
  let v = "v" |> W.f;
  let v_def = "v_def" |> W.f;
  let v' = "v'" |> W.f;
  let v1 = "v1" |> W.f;
  let v2 = "v2" |> W.f;
  let v3 = "v3" |> W.f;
  let t = "t" |> W.f;
  let t' = "t'" |> W.f;
  let t_def = "t_def" |> W.f;
  let t_body = "t_body" |> W.f;
  let t_body' = "t_body'" |> W.f;
  let t_in = "t_in" |> W.f;
  let t_out = "t_out" |> W.f;
  let t_in' = "t_in'" |> W.f;
  let t_out' = "t_out'" |> W.f;
  let t1 = "t1" |> W.f;
  let t2 = "t2" |> W.f;
  let t3 = "t3" |> W.f;
  let t1' = "t1'" |> W.f;
  let t2' = "t2'" |> W.f;
  let n = "n" |> W.f;
  let n' = "n'" |> W.f;
  let n1 = "n1" |> W.f;
  let n2 = "n2" |> W.f;
  let n3 = "n3" |> W.f;
  let tpat = "a" |> W.f;
  let a = "A" |> W.f;
  let b = "B" |> W.f;
  let c = "C" |> W.f;
  let x = "x" |> W.f;
  let y = "y" |> W.f;
  let gamma = "gamma" |> W.f;
  let gamma' = "gamma'" |> W.f;
  let gamma'' = "gamma''" |> W.f;
  let delta = "delta" |> W.f;
  let delta' = "delta'" |> W.f;
};
