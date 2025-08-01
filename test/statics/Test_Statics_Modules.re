open Test_Statics_Prelude;
open FTemp;
open Typ;
open Alcotest;
let tests = [
  fully_consistent_typecheck(
    "Empty module signature",
    {| {} |},
    Some(module_signature([])),
  ),
  fully_consistent_typecheck(
    "Module with single value",
    {| { val x = 3 } |},
    Some(
      module_signature([
        ModuleSignatureEntry.val_type(Pat.var("x"), Typ.int()),
      ]),
    ),
  ),
  fully_consistent_typecheck(
    "Valid value dot operator on module with single value",
    {| { val x = 3 }.x |},
    Some(Typ.int()),
  ),
  //   fully_consistent_typecheck(
  //     "Invalid value dot operator on module with single value",
  //     {| { val x = 3 }.y |},
  //     None,
  //   ),
  //   fully_consistent_typecheck(
  //     "Valid type dot operator on module with single value",
  //     {| x : { typedef T = Int }.T = 3 |},
  //     Some(Typ.int()),
  //   ),
  //   fully_consistent_typecheck(
  //     "Invalid type dot operator on module with single value",
  //     {| x : { typedef T = Int }.U = 3 |},
  //     None,
  //   ),
  fully_consistent_typecheck(
    "Module with value and type definition",
    {| { val x = 3 ;; typedef T = Int } |},
    Some(
      module_signature([
        ModuleSignatureEntry.val_type(Pat.var("x"), Typ.int()),
        ModuleSignatureEntry.type_def(TPat.var("T"), Typ.int()),
      ]),
    ),
  ),
  test_case(
    "Module with type definition and value binding using it", `Quick, () => {
    annotated_tree_test(
      {| { typedef T = Int ;; val x : T = 3 } |},
      FIError.(
        Exp.(
          module_([
            ModuleEntry.type_def(TPat.var("T"), Typ.int()),
            ModuleEntry.val_binding(
              Pat.asc(Pat.var("x"), Typ.var("T")),
              int(3),
            ),
          ])
        )
      ),
    )
  }),
  test_case(
    "Module with type definition and an outside exp failing to use it",
    `Quick,
    () => {
    annotated_tree_test(
      {| let m = { typedef T = Int } in let x: T = "not an int" |},
      FIError.(
        Exp.(
          let_(
            Pat.var("m"),
            module_([
              ModuleEntry.type_def(TPat.var("T"), Typ.int()),
              ModuleEntry.val_binding(
                Pat.asc(Pat.var("x"), Typ.var("T")),
                int(3),
              ),
            ]),
            let_(Pat.var("x"), string("not an int"), var("x")),
          )
        )
      ),
    )
  }),
  test_case(
    "Module with type definition and an outside exp using it via the dot operator",
    `Quick,
    () => {
    annotated_tree_test(
      {| let m = { typedef T = Int } in let x: m.T = 4 |},
      FIError.(
        Exp.(
          let_(
            Pat.var("m"),
            module_([
              ModuleEntry.type_def(TPat.var("T"), Typ.int()),
              ModuleEntry.val_binding(
                Pat.asc(Pat.var("x"), Typ.var("T")),
                int(3),
              ),
            ]),
            let_(Pat.asc(Pat.var("x"), Typ.var("T")), int(4), var("x")),
          )
        )
      ),
    )
  }),
  test_case("Module signature entry with invalid type", `Quick, () => {
    annotated_tree_test(
      {| { type T = Int ;; val x : T = "hello" } |},
      FIError.(
        Exp.(
          module_([
            ModuleEntry.type_def(TPat.var("T"), Typ.int()),
            ModuleEntry.val_binding(
              Pat.(asc(var("x"), Typ.var("T"))),
              string(
                ~ann=
                  Some(
                    Exp(
                      Common(
                        Inconsistent(
                          Expectation({
                            syn: FTemp.Typ.string(),
                            ana: FTemp.Typ.var("T"),
                          }),
                        ),
                      ),
                    ),
                  ),
                "hello",
              ),
            ),
          ])
        )
      ),
    )
  }),
  test_case("Module with correct signature hint", `Quick, () => {
    annotated_tree_test(
      {| { val x = 3 ;; typedef T = Int } : { tval x : Int } |},
      FIError.(
        Exp.(
          module_([
            ModuleEntry.val_binding(Pat.var("x"), int(3)),
            ModuleEntry.type_def(TPat.var("T"), Typ.int()),
          ])
        )
      ),
    )
  }),
  test_case("Module with incorrect signature hint", `Quick, () => {
    annotated_tree_test(
      {| { val x = 3 ;; typedef T = Int } : { tval x : String } |},
      FIError.(
        Exp.(
          module_([
            ModuleEntry.val_binding(
              Pat.asc(Pat.var("x"), Typ.string()),
              int(3),
            ),
            ModuleEntry.type_def(TPat.var("T"), Typ.int()),
          ])
        )
      ),
    )
  }),
];
