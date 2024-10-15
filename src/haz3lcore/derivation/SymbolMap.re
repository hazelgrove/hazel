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
  let e1 = "e₁" |> W.f;
  let e1' = "e₁'" |> W.f;
  let e2 = "e₂" |> W.f;
  let e2' = "e₂'" |> W.f;
  let e3 = "e₃" |> W.f;
  let v = "v" |> W.f;
  let v_def = "v_def" |> W.f;
  let v' = "v'" |> W.f;
  let v1 = "v₁" |> W.f;
  let v2 = "v₂" |> W.f;
  let v3 = "v₃" |> W.f;
  let t = "τ" |> W.f;
  let t_def = "τ_def" |> W.f;
  let t_body = "τ_body" |> W.f;
  let t_body' = "τ_body'" |> W.f;
  let t_in = "τ_in" |> W.f;
  let t_out = "τ_out" |> W.f;
  let t1 = "τ₁" |> W.f;
  let t2 = "τ₂" |> W.f;
  let t3 = "τ₃" |> W.f;
  let n = "n" |> W.f;
  let n' = "n'" |> W.f;
  let n1 = "n₁" |> W.f;
  let n2 = "n₂" |> W.f;
  let n3 = "n₃" |> W.f;
  let tpat = "a" |> W.f;
  let a = "A" |> W.f;
  let b = "B" |> W.f;
  let c = "C" |> W.f;
  let x = "x" |> W.f;
  let y = "y" |> W.f;
  let gamma = "Γ" |> W.f;
  let gamma' = "Γ'" |> W.f;
  let gamma'' = "Γ''" |> W.f;
  let delta = "Δ" |> W.f;
  let delta' = "Δ'" |> W.f;
};
