open Derivation_system.Verification

(* A ∧ B ⊢ B ∧ A *)
let d_simple : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let gamma : Ctx.t = [ And (a, b) ] in
  D
    ( Entail (gamma, And (b, a)),
      And_I,
      [
        D
          ( Entail (gamma, b),
            And_E_R,
            [ D (Entail (gamma, And (a, b)), Assumption, []) ] );
        D
          ( Entail (gamma, a),
            And_E_L,
            [ D (Entail (gamma, And (a, b)), Assumption, []) ] );
      ] )

(* ⊢ ¬(A ∨ B) ⊃ (¬A ∧ ¬B) *)
let d_490_24wi_a6_e1_p3 : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let not_a : Prop.t = Implies (a, Falsity) in
  let not_b : Prop.t = Implies (b, Falsity) in
  let not_a__and__not_b : Prop.t = And (not_a, not_b) in
  let a_or_b : Prop.t = Or (a, b) in
  let not__a_or_b : Prop.t = Implies (a_or_b, Falsity) in
  let gamma : Ctx.t = [ not__a_or_b ] in
  let gamma_a : Ctx.t = gamma @ [ a ] in
  let gamma_b : Ctx.t = gamma @ [ b ] in
  let d_a : Derivation.t =
    D
      ( Entail (gamma_a, Falsity),
        Implies_E,
        [
          D (Entail (gamma_a, not__a_or_b), Assumption, []);
          D
            ( Entail (gamma_a, a_or_b),
              Or_I_L,
              [ D (Entail (gamma_a, a), Assumption, []) ] );
        ] )
  in
  let d_b : Derivation.t =
    D
      ( Entail (gamma_b, Falsity),
        Implies_E,
        [
          D (Entail (gamma_b, not__a_or_b), Assumption, []);
          D
            ( Entail (gamma_b, a_or_b),
              Or_I_R,
              [ D (Entail (gamma_b, b), Assumption, []) ] );
        ] )
  in
  D
    ( Entail ([], Implies (not__a_or_b, not_a__and__not_b)),
      Implies_I,
      [
        D
          ( Entail (gamma, not_a__and__not_b),
            And_I,
            [
              D (Entail (gamma, not_a), Implies_I, [ d_a ]);
              D (Entail (gamma, not_b), Implies_I, [ d_b ]);
            ] );
      ] )

(* ⊢ (A ∧ (B ∨ C)) ⊃ ((A ∧ B) ∨ (A ∧ C)) *)
let d_490_24wi_final_q1 : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let c : Prop.t = Atom "C" in
  let gamma : Ctx.t = [ And (a, Or (b, c)) ] in
  let gamma_b : Ctx.t = gamma @ [ b ] in
  let gamma_c : Ctx.t = gamma @ [ c ] in
  D
    ( Entail ([], Implies (And (a, Or (b, c)), Or (And (a, b), And (a, c)))),
      Implies_I,
      [
        D
          ( Entail (gamma, Or (And (a, b), And (a, c))),
            Or_E,
            [
              D
                ( Entail (gamma, Or (b, c)),
                  And_E_R,
                  [ D (Entail (gamma, And (a, Or (b, c))), Assumption, []) ] );
              D
                ( Entail (gamma_b, Or (And (a, b), And (a, c))),
                  Or_I_L,
                  [
                    D
                      ( Entail (gamma_b, And (a, b)),
                        And_I,
                        [
                          D
                            ( Entail (gamma_b, a),
                              And_E_L,
                              [
                                D
                                  ( Entail (gamma_b, And (a, Or (b, c))),
                                    Assumption,
                                    [] );
                              ] );
                          D (Entail (gamma_b, b), Assumption, []);
                        ] );
                  ] );
              D
                ( Entail (gamma_c, Or (And (a, b), And (a, c))),
                  Or_I_R,
                  [
                    D
                      ( Entail (gamma_c, And (a, c)),
                        And_I,
                        [
                          D
                            ( Entail (gamma_c, a),
                              And_E_L,
                              [
                                D
                                  ( Entail (gamma_c, And (a, Or (b, c))),
                                    Assumption,
                                    [] );
                              ] );
                          D (Entail (gamma_c, c), Assumption, []);
                        ] );
                  ] );
            ] );
      ] )

(* A ⊃ ⊥, B ⊃ ⊥, A ∨ B ⊢ ⊥ *)
let d_490_24wi_a6_eg : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let not_a : Prop.t = Implies (a, Falsity) in
  let not_b : Prop.t = Implies (b, Falsity) in
  let a_or_b : Prop.t = Or (a, b) in
  let gamma : Ctx.t = [ not_a; not_b; a_or_b ] in
  let gamma_a : Ctx.t = gamma @ [ a ] in
  let gamma_b : Ctx.t = gamma @ [ b ] in
  let d_a : Derivation.t =
    D
      ( Entail (gamma_a, Falsity),
        Implies_E,
        [
          D (Entail (gamma_a, not_a), Assumption, []);
          D (Entail (gamma_a, a), Assumption, []);
        ] )
  in
  let d_b : Derivation.t =
    D
      ( Entail (gamma_b, Falsity),
        Implies_E,
        [
          D (Entail (gamma_b, not_b), Assumption, []);
          D (Entail (gamma_b, b), Assumption, []);
        ] )
  in
  D
    ( Entail (gamma, Falsity),
      Or_E,
      [ D (Entail (gamma, a_or_b), Assumption, []); d_a; d_b ] )

let ds =
  [ d_simple; d_490_24wi_a6_e1_p3; d_490_24wi_final_q1; d_490_24wi_a6_eg ]

let _ = ignore ds
(* let () =
   List.iter
     (fun d -> print_endline ("------\n" ^ MarkedDerivation.repr (mark d)))
     ds *)

open Derivation_system.Code

let ex_derivation : derive tree =
  Node
    ( { jdmt = "gamma |- b /\\ a"; rule = "And_I" },
      [
        Node
          ( { jdmt = "gamma |- b"; rule = "And_E_R" },
            [ Node ({ jdmt = "gamma |- a /\\ b"; rule = "Assumption" }, []) ] );
        Node
          ( { jdmt = "gamma |- a"; rule = "And_E_L" },
            [ Node ({ jdmt = "gamma |- a /\\ b"; rule = "Assumption" }, []) ] );
      ] )

(* let _ = pipeline ex_derivation
   let _ = print_endline ""
   let _ = print_endline "Done!"

   let ex_derivation : derive tree =
    Node
      ( { jdmt = "gamma |- b"; rule = "And_E_R" },
        [ Node ({ jdmt = "gamma |- a /\\ b"; rule = "Assumption" }, []) ] ) *)

(* let vnt = mk_virtual_node_tree ex_derivation
   let szt = mk_size_tree vnt

   let _ = Tree.combine (szt, vnt) |>
   Tree.map(
     fun ({w;h}, vn) ->
       print_endline (Printf.sprintf "(%d,%d): %s" w h (vn |> (function
       | Leaf s -> s | Branch (_) -> ""))
       )
   )

   let _ = ex_derivation |> mk_virtual_node_tree |> mk_loc_list |>
   List.map(
     fun (loc, s) -> print_endline (Printf.sprintf "(%s,%s): %s" (string_of_int loc.y)(string_of_int loc.x) s)
   )

   let ex_derivation : derive tree =
     Node ({ jdmt = "gamma |- a /\\ b"; rule = "Assumption" }, [])
*)
let _ = pipeline ex_derivation
