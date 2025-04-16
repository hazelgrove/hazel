open Testgen;
open Junit_alcotest;
open Symex;
module Z3Int = Z3.Arithmetic.Integer;
open Haz3lmenhir;
run_and_report(
  ~and_exit=false,
  "TestGen",
  Alcotest.[
    (
      "Symbolic Execution",
      {
        let symex_exp =
          testable(Fmt.using(show_symex_exp, Fmt.string), equal_symex_exp);
        [
          test_case("constant", `Quick, () => {
            check(
              symex_exp,
              "Symbolic execution of constant",
              {
                term: Atom(Int(Bigint.of_int(1))),
                annotation: initial_symex_state,
              },
              symbolic_execution(
                ~state=initial_symex_state,
                Haz3lmenhir.AST.Atom(Int(Bigint.of_int(1))),
              ),
            )
          }),
          test_case("Branched if", `Quick, () => {
            check(
              symex_exp,
              "Symbolic execution of if",
              {
                term:
                  If(
                    {
                      term: Atom(Bool(true)),
                      annotation: initial_symex_state,
                    },
                    {
                      term: Atom(Int(Bigint.of_int(2))),
                      annotation: {
                        symbolic_variable_state: VariableState.empty,
                        assumptions: [Atom(Bool(true))],
                      },
                    },
                    {
                      term: Atom(Int(Bigint.of_int(3))),
                      annotation: {
                        symbolic_variable_state: VariableState.empty,
                        assumptions: [
                          UnOp(
                            Bool(Not),
                            AST.lift(Atom(Bool(true)): AST.exp(unit)),
                          ),
                        ],
                      },
                    },
                  ),
                annotation: initial_symex_state,
              },
              symbolic_execution(
                ~state=initial_symex_state,
                AST.If(
                  AST.lift(AST.Atom(Bool(true))),
                  AST.lift(AST.Atom(Int(Bigint.of_int(2)))),
                  AST.lift(AST.Atom(Int(Bigint.of_int(3)))),
                ),
              ),
            )
          }),
          test_case("Symbolic execution of let", `Quick, () => {
            check(
              symex_exp,
              "Symbolic execution of let",
              {
                term:
                  Let(
                    {
                      term: VarPat("x"),
                      annotation: initial_symex_state,
                    },
                    {
                      term: Atom(Int(Bigint.of_int(1))),
                      annotation: initial_symex_state,
                    },
                    {
                      term: Var("x"),
                      annotation: initial_symex_state,
                      // annotation: {
                      //   symbolic_variable_state:
                      //     VariableState.of_list([
                      //       (
                      //         "x",
                      //         Atom(Int(Bigint.of_int(1))): AST.exp(unit),
                      //       ),
                      //     ]),
                      //   assumptions: [],
                      // },
                    },
                  ),
                annotation: initial_symex_state,
              },
              symbolic_execution(
                ~state=initial_symex_state,
                AST.Let(
                  AST.lift(AST.VarPat("x")),
                  AST.lift(AST.Atom(Int(Bigint.of_int(1)))),
                  AST.lift(AST.Var("x")),
                ),
              ),
            )
          }),
          // test_case("If predicated on variable", `Quick, () => {
          //   check(
          //     symex_exp,
          //     "If in a let",
          //     {
          //       term:
          //         Let(
          //           {
          //             term: VarPat("x"),
          //             annotation: initial_symex_state,
          //           },
          //           {
          //             term: Var("y"),
          //             annotation: initial_symex_state,
          //           },
          //           {
          //             term:
          //               If(
          //                 {
          //                   term:
          //                     BinExp(
          //                       {
          //                         term: Var("x"),
          //                         annotation: initial_symex_state,
          //                       },
          //                       IntOp(Equals),
          //                       {
          //                         term: Atom(Int(Bigint.of_int(1))),
          //                         annotation: initial_symex_state,
          //                       },
          //                     ),
          //                   annotation: initial_symex_state,
          //                 },
          //                 {
          //                   term: Atom(Int(Bigint.of_int(0))),
          //                   annotation: {
          //                     symbolic_variable_state:
          //                       VariableState.of_list([
          //                         (
          //                           "x",
          //                           Atom(Int(Bigint.of_int(1))):
          //                             AST.exp(unit),
          //                         ),
          //                       ]),
          //                     assumptions: [],
          //                   },
          //                 },
          //                 {
          //                   term: Atom(Int(Bigint.of_int(2))),
          //                   annotation: initial_symex_state,
          //                 },
          //               ),
          //             annotation: {
          //               symbolic_variable_state:
          //                 VariableState.of_list([
          //                   (
          //                     "x",
          //                     Atom(Int(Bigint.of_int(1))): AST.exp(unit),
          //                   ),
          //                 ]),
          //               assumptions: [],
          //             },
          //           },
          //         ),
          //       annotation: initial_symex_state,
          //     },
          //     symbolic_execution(
          //       ~state=initial_symex_state,
          //       Haz3lmenhir.Interface.parse_program(
          //         {|let x = y in if x == 1 then 0 else 2|},
          //       ),
          //     ),
          //   )
          // }),
        ];
      },
    ),
  ],
);
