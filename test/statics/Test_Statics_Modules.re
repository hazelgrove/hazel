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
    "Module with value and type definition",
    {| { val x = 3 ;; typedef T = Int } |},
    Some(
      module_signature([
        ModuleSignatureEntry.val_type(Pat.var("x"), Typ.int()),
        ModuleSignatureEntry.type_def(TPat.var("T"), Typ.int()),
      ]),
    ),
  ),
  test_case("Module signature entry with invalid type", `Quick, () => {
    annotated_tree_test(
      {| { type T = Int ;; val x : T = "hello" } |},
      FIError.(
        Exp.(
          module_([
            ModuleEntry.type_def(TPat.var("T"), Typ.int()),
            ModuleEntry.val_binding(
              Pat.(cast(var("x"), Typ.var("T"), Typ.unknown(Internal))),
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
];
