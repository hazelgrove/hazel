open Junit_alcotest;
open Haz3lcore;
open Test_TypeAssignment;

let (suite, _) =
  run_and_report(
    ~and_exit=false,
    "Dynamics",
    [("Elaboration", Test_Elaboration.elaboration_tests)],
  );
Junit.to_file(Junit.make([suite]), "junit_tests.xml");
Bisect.Runtime.write_coverage_data();

//let l = QCheck.Gen.generate(~n=5, Test_TypeAssignment.uexp_bool_gen);
let input_ty = utyp(List(utyp(Int)));
let u = uexp_gen([], input_ty, 2);
let ty1 = Term.UTyp.to_typ([], input_ty);
let m = Interface.Statics.mk_map(CoreSettings.on, u);
List.iter(
  u => {
    print_endline("\nUExp=\n");
    print_endline(Term.UExp.show(u));
  },
  [u],
);
switch (Elaborator.fixed_exp_typ(m, u)) {
| Some(ty2) when Typ.eq(ty1, ty2) => print_endline("\nTYPES EQUAL")
| _ => print_endline("\nTYPES NOT EQUAL")
};
