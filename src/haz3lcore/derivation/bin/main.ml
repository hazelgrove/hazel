open Derivation_system.Verification
open Derivation_system.DerivationType

let d1 : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  D
    ( Entail ([ And (a, b) ], And (b, a)),
      And_I,
      [
        D
          ( Entail ([ And (a, b) ], b),
            And_E_R,
            [ D (Entail ([ And (a, b) ], And (a, b)), Assumption, []) ] );
        D
          ( Entail ([ And (a, b) ], a),
            And_E_L,
            [ D (Entail ([ And (a, b) ], And (a, b)), Assumption, []) ] );
      ] )

let d2 : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let c : Prop.t = Atom "C" in
  let gamma : Prop.t list = [ And (a, Or (b, c)) ] in
  let gamma_b : Prop.t list = gamma @ [ b ] in
  let gamma_c : Prop.t list = gamma @ [ c ] in
  D
    ( Entail ([], Implies (And (a, Or (b, c)), Or (And (a, b), And (a, c)))),
      Implies_I,
      [
        D
          ( Entail (gamma, Or (And (a, b), And (a, c))),
            Or_E,
            [
              D (Entail (gamma, Or (b, c)), Assumption, []);
              D
                ( Entail (gamma_b, Or (And (a, b), And (a, c))),
                  Or_I_L,
                  [
                    D
                      ( Entail (gamma_b, And (a, b)),
                        And_I,
                        [
                          D (Entail (gamma_b, a), Assumption, []);
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
                          D (Entail (gamma_c, a), Assumption, []);
                          D (Entail (gamma_c, c), Assumption, []);
                        ] );
                  ] );
            ] );
      ] )

let d3 : Derivation.t =
  let a : Prop.t = Atom "A" in
  let b : Prop.t = Atom "B" in
  let c : Prop.t = Atom "C" in
  let gamma : Prop.t list = [ And (a, Or (b, c)) ] in
  let gamma_b : Prop.t list = gamma @ [ b ] in
  let gamma_c : Prop.t list = gamma @ [ c ] in
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

let ds = [ d1; d2; d3 ]

let () =
  List.iter
    (fun d -> print_endline ("------\n" ^ MarkedDerivation.repr (mark d)))
    ds

(* open Derivation_system.Abbreviation

   let ( let* ) (b : string * DerivationBind.bindable)
       (a : unit -> DerivationBind.t) : DerivationBind.t =
     match b with s, b -> Bind (s, b, a ())

   let da1 : DerivationBind.t =
     let a : PropAbbr.t = Just (Atom "A") in
     let b : PropAbbr.t = Just (Atom "B") in
     let c : PropAbbr.t = Just (Atom "C") in
     let gamma' : PropAbbr.t list = [ Just (And (a, b)) ] in
     let gamma : CtxAbbr.t = Just gamma' in
     let gamma_b : CtxAbbr.t = Just (b :: gamma') in
     let gamma_c : CtxAbbr.t = Just (c :: gamma') in
     let* _ = ("Γ", Ctx gamma) in
     let* _ = ("Γ_b", Ctx gamma_b) in
     let* _ = ("Γ_c", Ctx gamma_c) in
     let* _ =
       ( "D_1",
         Derivation
           (Just
              (D
                 ( Just (Entail (Abbr "Γ_b", Just (And (a, b)))),
                   And_I,
                   [
                     Just
                       (D
                          ( Just (Entail (Abbr "Γ_b", a)),
                            And_E_L,
                            [
                              Just
                                (D
                                   ( Just
                                       (Entail
                                          ( Abbr "Γ_b",
                                            Just (And (a, Just (Or (b, c)))) )),
                                     Assumption,
                                     [] ));
                            ] ));
                     Just (D (Just (Entail (Abbr "Γ_b", b)), Assumption, []));
                   ] ))) )
     in
     let* _ =
       ( "D_2",
         Derivation
           (Just
              (D
                 ( Just (Entail (Abbr "Γ_c", Just (And (a, b)))),
                   And_I,
                   [
                     Just
                       (D
                          ( Just (Entail (Abbr "Γ_c", a)),
                            And_E_L,
                            [
                              Just
                                (D
                                   ( Just
                                       (Entail
                                          ( Abbr "Γ_c",
                                            Just (And (a, Just (Or (b, c)))) )),
                                     Assumption,
                                     [] ));
                            ] ));
                     Just (D (Just (Entail (Abbr "Γ_c", c)), Assumption, []));
                   ] ))) )
     in
     Just
       (Just
          (D
             ( Just
                 (Entail
                    ( Just [],
                      Just
                        (Implies
                           ( Just (And (a, Just (Or (b, c)))),
                             Just (Or (Just (And (a, b)), Just (And (a, c)))) )) )),
               Implies_I,
               [
                 Just
                   (D
                      ( Just
                          (Entail
                             ( Abbr "Γ",
                               Just (Or (Just (And (a, b)), Just (And (a, c)))) )),
                        Or_E,
                        [
                          Just
                            (D
                               ( Just (Entail (Abbr "Γ", Just (Or (b, c)))),
                                 And_E_R,
                                 [
                                   Just
                                     (D
                                        ( Just
                                            (Entail
                                               ( Abbr "Γ",
                                                 Just (And (a, Just (Or (b, c))))
                                               )),
                                          Assumption,
                                          [] ));
                                 ] ));
                          Just
                            (D
                               ( Just
                                   (Entail
                                      ( Abbr "Γ_b",
                                        Just
                                          (Or (Just (And (a, b)), Just (And (a, c))))
                                      )),
                                 Or_I_L,
                                 [ Abbr "D_1" ] ));
                          Just
                            (D
                               ( Just
                                   (Entail
                                      ( Abbr "Γ_c",
                                        Just
                                          (Or (Just (And (a, b)), Just (And (a, c))))
                                      )),
                                 Or_I_R,
                                 [ Abbr "D_2" ] ));
                        ] ));
               ] )))

   let () = print_endline ("------\n" ^ DerivationBind.repr da1)

   let () =
     print_endline (MarkedDerivationBind.repr (MarkedDerivationBind.mark da1)) *)
