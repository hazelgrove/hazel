open Derivation

let d1 : Derivation.t =
  let a : Prop.t = PropVar "A" in
  let b : Prop.t = PropVar "B" in
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
  let a : Prop.t = PropVar "A" in
  let b : Prop.t = PropVar "B" in
  let c : Prop.t = PropVar "C" in
  let gamma : Prop.t list = [ And (a, Or (b, c)) ] in
  let gamma_b : Prop.t list = b :: gamma in
  let gamma_c : Prop.t list = c :: gamma in
  D
    ( Entail ([], Imply (And (a, Or (b, c)), Or (And (a, b), And (a, c)))),
      Imply_I,
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
  let a : Prop.t = PropVar "A" in
  let b : Prop.t = PropVar "B" in
  let c : Prop.t = PropVar "C" in
  let gamma : Prop.t list = [ And (a, Or (b, c)) ] in
  let gamma_b : Prop.t list = b :: gamma in
  let gamma_c : Prop.t list = c :: gamma in
  D
    ( Entail ([], Imply (And (a, Or (b, c)), Or (And (a, b), And (a, c)))),
      Imply_I,
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
    (fun d ->
      print_endline
        ("------\n" ^ MarkedDerivation.repr (MarkedDerivation.mark d) 0))
    ds
