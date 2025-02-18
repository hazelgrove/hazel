open Alcotest;
open Haz3lcore;
open Grammar;

let qcheck_map_annotation_test =
  QCheck.Test.make(
    ~name="Map annotation to something and back",
    ~count=100,
    QCheck.make(
      ~print=Haz3lmenhir.AST.show_exp,
      Haz3lmenhir.AST.gen_exp_sized(7),
    ),
    exp => {
      let core_exp = Haz3lmenhir.Conversion.Exp.of_menhir_ast(exp);

      Grammar.equal_exp_t(
        (==),
        Grammar.map_exp_annotation(Fun.id, core_exp),
        core_exp,
      );
    },
  );
let tests = (
  "Grammar",
  [QCheck_alcotest.to_alcotest(qcheck_map_annotation_test)],
);
