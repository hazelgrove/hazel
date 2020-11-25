open Lang

(* Hole set functions *)

module Hole_set = Set.Make (struct
  type t = hole_name

  let compare = compare
end)

let domain (m : 'a hole_map) : Hole_set.t =
  Hole_set.of_list (List.map fst @@ Hole_map.bindings m)

(* Hole map functions *)

let delete_min map =
  let open Option2.Syntax in
  let+ k, v = Hole_map.min_binding_opt map in
  ((k, v), Hole_map.remove k map)

let delete hole_name map =
  let open Option2.Syntax in
  let+ v = Hole_map.find_opt hole_name map in
  ((hole_name, v), Hole_map.remove hole_name map)

let empty = (Hole_map.empty, Hole_map.empty)

let from_hole_filling hf = (hf, Hole_map.empty)

let from_unsolved_constraints us = (Hole_map.empty, us)

let solved_singleton h e = (Hole_map.singleton h e, Hole_map.empty)

let unsolved_singleton h w = (Hole_map.empty, Hole_map.singleton h w)

let merge_solved fs =
  let exception Merge_failure in
  let merge_map =
    Hole_map.union (fun _ e1 e2 ->
        if Exp.syntactically_equal e1 e2 then Some e1
        else raise_notrace Merge_failure)
  in
  try Some (List.fold_left merge_map Hole_map.empty fs)
  with Merge_failure -> None

let merge_unsolved us =
  let merge_map = Hole_map.union (fun _ v1 v2 -> Some (v1 @ v2)) in
  List.fold_left merge_map Hole_map.empty us

let merge ks =
  let open Option2.Syntax in
  let fs, us = List.split ks in
  let+ f = merge_solved fs in
  let u = merge_unsolved us in
  (f, u)

let satisfies hf (f0, us) =
  let open Option2.Syntax in
  Option2.with_default false
  @@ let* _ = Option2.guard @@ Hole_set.subset (domain f0) (domain hf) in
     (* When successful, hf_merged = hf because dom(f0) `subset` dom(hf) *)
     let+ hf_merged = merge_solved [f0; hf] in
     Hole_map.for_all
       (fun hole_name worlds ->
         Example.exp_satisfies hf_merged (EHole hole_name) worlds)
       us
